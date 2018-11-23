%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkDomain API event watching
-module(nkdomain_api_watch).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/4]).

%%-include("nkdomain.hrl").
-include("nkdomain_api.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
%%-include_lib("nkpacket/include/nkpacket.hrl").

-define(EVENT_STREAM_HEARTBEAT, 5000).


%% ===================================================================
%% Types
%% ===================================================================


-callback event_stream_start(nkdomain_api:request()) ->
    {ok, nkdomain_api:request()} | {error, term()}.

-callback event_stream_stop(Reason::term(), nkdomain_api:request()) ->
    {ok, nkdomain_api:request()} | {error, term()}.

-callback event_stream_heartbeat(nkdomain_api:request()) ->
    {ok, nkdomain_api:request()} | {error, term()}.

-callback new_event(map(), nkdomain_api:request()) ->
    {ok, nkdomain_api:request()} | {error, term()}.


-optional_callbacks([event_stream_start/1, event_stream_stop/2, event_stream_heartbeat/1]).


%% ===================================================================
%% Watch processing
%% ===================================================================


%% @doc
%% ActorId is the actor we want to listen on
start(SrvId, ActorId, Config, ApiReq) ->
    #{params:=Params} = ApiReq,
    Filters1 = maps:with([kind], Params),
    ?API_DEBUG("processing watch ~p (~p)", [ActorId, Filters1]),
    % Get the PID of the actor we want to watch on, and start a link on it to us
    ActorPid = case nkservice_actor:activate({SrvId, ActorId}) of
        {ok, #actor_id{pid=ActorPid0}, _Meta} ->
            ActorPid0;
        {error, ActivateError} ->
            throw({error, ActivateError})
    end,
    ActorMon = monitor(process, ActorPid),
    LinkRef = make_ref(),
    Link = {nkdomain_core_v1_watch, LinkRef, self()},
    LinkOpts = #{
        get_events => true,
        avoid_unload => true,
        data => #{filters=>Filters1}
    },
    case nkservice_actor_srv:sync_op({SrvId, ActorId}, {link, Link, LinkOpts}) of
        ok ->
            % We will start receiving events now
            ok;
        {error, LinkError} ->
            throw({error, LinkError})
    end,
    ApiReq2 = stream_start(ApiReq),
    CB = maps:get(callback, ApiReq),
    nklib_proc:put(core_v1_watches, {CB, ActorId}),
    watch_db(SrvId, ActorId, Config, LinkRef, ActorMon, Params, ApiReq2).


%% @private
%% If we have a resourceVersion we need to look in database
watch_db(SrvId, ActorId, Config, LinkRef, ActorMon, #{resourceVersion:=Hash}=Params, ApiReq) ->
    #actor_id{domain=Domain, group=Group, resource=Resource, name=Name} = ActorId,
    #{camel:=Kind} = Config,
    SearchParams1 = maps:with([deep], Params),
    {QDomain, QKind, QName} = case {Group, Resource} of
        {?GROUP_CORE, ?RES_CORE_DOMAINS} ->
            {nkdomain_register:actor_id_to_managed_domain(ActorId), all, all};
        _ ->
            {Domain, Kind, Name}
    end,
    Query1 = {domain_find_event_version, QDomain, QKind, QName, Hash, SearchParams1},
    ok = nkdomain_api_events:wait_for_save(),
    case nkservice_actor_db:search(SrvId, Query1) of
        {ok, not_found, _} ->
            % Event not found in db
            {error, actor_not_found, ApiReq};
        {ok, {Date, UID}, _} ->
            % Date is the one for Vsn, UID is its uid
            SearchParams2 = SearchParams1#{domain=>QDomain, from=>0, size=>101},
            Query2 = {
                domain_search_events_from_version,
                QKind,
                QName,
                UID,
                Date,
                SearchParams2
            },
            case nkservice_actor_db:search(SrvId, Query2) of
                {ok, EvActors, _} when length(EvActors) > 100 ->
                    {error, too_many_records, ApiReq};
                {ok, EvActors, _} ->
                    ApiReq2 = watch_do_send_events(SrvId, EvActors, ApiReq),
                    Sent = [SentVsn || #actor{hash=SentVsn} <- EvActors],
                    watch_wait(SrvId, LinkRef, ActorMon, Sent, ApiReq2);
                {error, Error} ->
                    {error, Error, ApiReq}
            end;
        {error, Error} ->
            {error, Error, ApiReq}
    end;

watch_db(SrvId, _ActorId, _Config, Ref, ActorMon, _Params, ApiReq) ->
    watch_wait(SrvId, Ref, ActorMon, undefined, ApiReq).


%% @private
watch_wait(SrvId, LinkRef, ActorMon, Sent, ApiReq) ->
    receive
        {nkdomain_core_v1_watch, LinkRef, Op, EvActor, _Data} when is_list(Sent) ->
            % We have
            #actor{hash=Vsn} = EvActor,
            case lists:member(Vsn, Sent) of
                true ->
                    ?API_LLOG(info, "watcher skipping event", []),
                    watch_wait(SrvId, LinkRef, ActorMon, Sent, ApiReq);
                false ->
                    ApiReq2 = watch_do_send_events(SrvId, [{Op, EvActor}], ApiReq),
                    watch_wait(SrvId, LinkRef, ActorMon, Sent, ApiReq2)
            end;
        {nkdomain_core_v1_watch, LinkRef, Op, EvActor, _Data} ->
            ApiReq2 = watch_do_send_events(SrvId, [{Op, EvActor}], ApiReq),
            watch_wait(SrvId, LinkRef, ActorMon, Sent, ApiReq2);
        {'DOWN', ActorMon, process, _Pid, _Reason} ->
            ApiReq2 = stream_stop(process_down, ApiReq),
            {status, watch_stop, ApiReq2};
        stop_watch ->
            ApiReq2 = stream_stop(process_down, ApiReq),
            {status, watch_stop, ApiReq2}
    after ?EVENT_STREAM_HEARTBEAT ->
        case stream_heartbeat(ApiReq) of
            {ok, ApiReq2} ->
                % There is no more pending msgs, so remove Sent
                watch_wait(SrvId, LinkRef, ActorMon, undefined, ApiReq2);
            {error, Error} ->
                ApiReq2 = stream_stop(Error, ApiReq),
                {status, watch_stop, ApiReq2}
        end
    end.



%% @private
%% Throws {status, _, _}
watch_do_send_events(_SrvId, [], ApiReq) ->
    ApiReq;

watch_do_send_events(SrvId, [#actor{metadata=Meta}=EvActor|Rest], ApiReq) ->
    % We don't have Op information (from DB). Guess it.
    #{<<"creationTime">>:=CT, <<"updateTime">>:=UT} = Meta,
    Op = case CT==UT of
        true -> created;
        false -> updated
    end,
    watch_do_send_events(SrvId, [{Op, EvActor}|Rest], ApiReq);

watch_do_send_events(SrvId, [{Op, EvActor}|Rest], ApiReq) ->
    ApiReq2 = case nkdomain_api:actor_to_external(SrvId, EvActor) of
        {ok, ApiActor} ->
            Event = #{
                <<"type">> => case Op of created -> <<"ADDED">>; updated -> <<"MODIFIED">> end,
                <<"object">> => ApiActor
            },
            stream_event(Event, ApiReq);
        {error, Error} ->
            ?API_LLOG(warning, "could not process event ~p: ~p", [EvActor, Error]),
            ApiReq
    end,
    watch_do_send_events(SrvId, Rest, ApiReq2).


%% @doc
stream_event(Event, #{callback:=Callback}=ApiReq) ->
    case Callback:new_event(Event, ApiReq) of
        {ok, ApiReq2} ->
            ApiReq2;
        {error, Error} ->
            ApiReq2 = stream_stop(Error, ApiReq),
            throw({status, watch_stop, ApiReq2})
    end.


%% @private
stream_start(#{callback:=Callback}=ApiReq) ->
    case ApiReq of
        #{callback:=CallBack} ->
            case erlang:function_exported(CallBack, event_stream_start, 1) of
                true ->
                    case Callback:event_stream_start(ApiReq) of
                        {ok, ApiReq2} ->
                            ApiReq2;
                        {error, StartError} ->
                            throw({error, StartError})
                    end;
                false ->
                    ApiReq
            end;
        _ ->
            ApiReq
    end.


%% @doc
stream_heartbeat(#{callback:=Callback}=ApiReq) ->
    case ApiReq of
        #{callback:=CallBack} ->
            case erlang:function_exported(CallBack, event_stream_heartbeat, 1) of
                true ->
                    Callback:event_stream_heartbeat(ApiReq);
                false ->
                    {ok, ApiReq}
            end;
        _ ->
            {ok, ApiReq}
    end.


%% @private
stream_stop(Reason, #{callback:=Callback}=ApiReq) ->
    case ApiReq of
        #{callback:=CallBack} ->
            case erlang:function_exported(CallBack, event_stream_stop, 2) of
                true ->
                    case Callback:event_stream_stop(Reason, ApiReq) of
                        {ok, ApiReq2} ->
                            ApiReq2;
                        {error, StopError} ->
                            throw({error, StopError, ApiReq})
                    end;
                false ->
                    ApiReq
            end;
        _ ->
            ApiReq
    end.


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).
