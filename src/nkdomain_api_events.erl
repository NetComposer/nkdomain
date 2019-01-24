%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain evens management module
%%
%% Actor-generated events
%% ----------------------
%%
%% - when a event is generated at an actor, callback actor_srv_event/2 will be
%%   called at nkdomain_callbacks
%%      - if it is a 'test_api' event, an event is generated and sent
%%        using event/2 to linked processes and related actors
%%      - otherwise, it is sent to the actor than can do the same or not
%%      - any case, when calling nkdomain_api_lib:make_event_actor/1, a
%%        relation is added to my domain, except if it is a domain actor
%%        (see bellow)
%%
%% - if there is an active API listener, it will capture it in actor_srv_link_event/3 in
%%   nkdomain_callbacks, and send it to API listener
%%
%% - if the actor is a domain, it can escalate the event to its father in
%%   nkdomain_actor_domain:escalate_event/2
%%
%%
%% API-generated events
%% --------------------
%%
%% - In nkdomain_core_v1, it can generate events when the actor is not
%%   activated. It calls event/1, so it is sent to related only


-module(nkdomain_api_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([send_event/2, send_event_st/2]).
-export([make_event/3]).
-export([wait_for_save/0, start_link/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([test/0]).
-export([remove_old_hashes/1]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Events: "++Txt, Args)).

-define(TIME_TO_SAVE, 1000).
-define(MAX_REMEMBER_TIME, 5*60*1000).
-define(RETRY_WAIT_TIME, 1000).
-define(MAX_RETRIES, 5).


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public - Send events
%% ===================================================================

%% @doc
%% Standard sending for API events
%% - check and upgrade repetition counters
%% - send the event to the gen_server for save at intervals
%% - send a copy of the event to the domain (or the father domain if it's a domain)
%% - find UIDs in labels event.related:Type, and send a copy to each
-spec send_event(nkservice:id(), #actor{}) ->
    {added|updated, #actor{}}.

send_event(SrvId, #actor{metadata=Meta}=EvActor) ->
    {Op, EvActor2, Hash} = update_counter(EvActor),
    gen_server:cast(?MODULE, {new_event, Hash, EvActor2}),
    ApiEvent = {nkdomain_api_event, Op, EvActor2},
    % Send a copy of the event to the domain of the involved object, except if is root
    do_send_domain_event(EvActor, ApiEvent),
    % Send event to all related
    lists:foreach(
        fun({Label, UID}) ->
            case Label of
                <<"event.related:", _/binary>> ->
                    nkservice_actor_srv:async_op({SrvId, UID}, {send_event, ApiEvent});
                _ ->
                    ok
            end
        end,
        maps:to_list(maps:get(<<"labels">>, Meta, #{}))),
    {Op, EvActor2}.


%% @private
do_send_domain_event(EvActor, ApiEvent) ->
    #actor{data=Data} = EvActor,
    #{<<"involvedObject">> := #{
        <<"kind">> := Kind,
        <<"name">> := Name,
        <<"domain">>:= Domain
    }} = Data,
    case {Domain, Kind, Name} of
        {?ROOT_DOMAIN, ?KIND_CORE_DOMAIN, ?ROOT_DOMAIN} ->
            % lager:error("NKLOG SKIPPING ROOT");
            ok;
        _ ->
            % lager:error("SENDING EVENT UP FROM ~p TO ~p ~p", [IO, Domain, ActorId]),
            case nkdomain_register:is_domain_active(Domain) of
                {true, Pid} ->
                    nkservice_actor_srv:async_op(Pid, {send_event, ApiEvent});
                false ->
                    ?LLOG(warning, "could not send to domain '~s': not active", [Domain])
            end
    end.


%% @doc Sends an event from inside an actor process
%% - Performs the standard event processing (see above)
%% - Since the event is generated at the actor, we send the nkdomain_api_event only to
%%   linked processes avoiding sending the nkdomain_api_event to ourselves
-spec send_event_st(#actor{}, #actor_st{}) ->
    #actor_st{}.

send_event_st(EvActor, #actor_st{srv=SrvId}=ActorSt) ->
    {Op, EvActor2} = send_event(SrvId, EvActor),
    Ev = {nkdomain_api_event, Op, EvActor2},
    ActorSt2 = nkservice_actor_srv:do_event_link(Ev, ActorSt),
    send_event_callback(EvActor, ActorSt2),
    ActorSt2.


%% @private
send_event_callback(EvActor, ActorSt) ->
    #actor_st{srv=SrvId, actor=#actor{metadata=Meta}} = ActorSt,
    case Meta of
        #{<<"callbackUrl">>:=CallbackUrl} ->
            {ok, ApiEvActor} = nkdomain_api:actor_to_external(SrvId, EvActor),
            Json = nklib_json:encode_sorted(ApiEvActor),
            Hds = [{<<"content-type">>, <<"application/json">>}],
            Opts = [{pool, SrvId}],
            case hackney:request(post, CallbackUrl, Hds, Json, Opts) of
                {ok, 200, _, _} ->
                    ?LLOG(notice, "event callback ~s ok", [CallbackUrl]);
                {ok, Code, _, _} ->
                    ?LLOG(notice, "event callback ~s error: ~p", [CallbackUrl, Code]);
                {error, Error} ->
                    ?LLOG(notice, "event callback ~s error: ~p", [CallbackUrl, Error])
            end;
        _ ->
            ok
    end.


%% ===================================================================
%% Public - Make events
%% ===================================================================

%% @doc Generates an Event Actor
%% - SrvId (and Domain) for the Event Actor will be the same as involvedObject
%%   TODO
%% - For all fields in related (Type => UID) a label event.related will
%%   be generated (nkdomain_api_events will copy to those the event)
-spec make_event(nkservce:id(), nkdomain_api:api_event(), #actor{}) ->
    #actor{}.

make_event(SrvId, Event, #actor{id=ActorId}=InvolvedActor) ->
    #actor_id{domain=Domain, group=Group, resource=Res, name=Name} = ActorId,
    EvDomain = case {Group, Res} of
        {?GROUP_CORE, ?RES_CORE_DOMAINS} ->
            nkdomain_register:actor_id_to_managed_domain(ActorId);
        _ ->
            Domain
    end,
    Related1 = maps:get(related, Event, #{}),
    Labels = maps:fold(
        fun(RelType, UID, Acc) -> Acc#{<<"event.related:", RelType/binary>> => UID} end,
        #{},
        Related1),
    Rand = nklib_util:uid(),
    EvActor = #actor{
        id = #actor_id{
            domain = EvDomain,
            group = ?GROUP_CORE,
            vsn = ?GROUP_CORE_V1A1,
            resource = ?RES_CORE_EVENTS,
            name = <<Res/binary, $., Name/binary, $., Rand/binary>>
        },
        data = make_event_data(SrvId, Event, InvolvedActor),
        metadata = #{<<"labels">> => Labels}
    },
    nkservice_actor_util:put_create_fields(EvActor).


%% @doc
make_event_data(SrvId, Event, InvolvedActor) ->
    #actor{id=ActorId, metadata=Meta, hash=Hash} = InvolvedActor,
    #actor_id{
        domain = InvolvedActorDomain,
        name = Name,
        group = Group,
        vsn = Vsn,
        resource = Res,
        uid = UID
    } = ActorId,
    EventType = case maps:get(type, Event, normal) of
        normal ->
            <<"Normal">>;
        warning ->
            <<"Warning">>
    end,
    Time = nklib_date:now_3339(msecs),
    {ok, #{camel:=Kind}} = nkdomain_actor_util:get_config(SrvId, Group, Res),
    Hash2 = case is_binary(Hash) andalso Hash /= <<>> of
        true ->
            Hash;
        false ->
            <<>>
    end,
    InvolvedObj1 = #{
        <<"apiVersion">> => <<Group/binary, $/, Vsn/binary>>,
        <<"kind">> => Kind,
        <<"name">> => Name,
        <<"domain">> => InvolvedActorDomain,
        <<"uid">> => UID,
        <<"resourceVersion">> => Hash2
    },
    InvolvedObj2 = case maps:get(<<"subtype">>, Meta, <<>>) of
        <<>> ->
            InvolvedObj1;
        SubType ->
            InvolvedObj1#{<<"subtype">> => SubType}
    end,
    #{
        <<"kind">> => ?KIND_CORE_EVENT,
        <<"type">> => EventType,
        <<"reason">> => maps:get(reason, Event),
        <<"message">> => maps:get(message, Event, <<>>),
        <<"involvedObject">> => InvolvedObj2,
        <<"source">> => #{
            <<"component">> => <<"nkdomain">>,
            <<"host">> => nklib_util:to_binary(node())
        },
        <<"body">> => maps:get(body, Event, <<>>),
        <<"firstTimestamp">> => Time,
        <<"lastTimestamp">> => Time,
        <<"count">> => 1,
        <<"eventTime">> => null,
        <<"reportingComponent">> => <<>>,
        <<"reportingInstance">> => <<>>
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @private
wait_for_save() ->
    gen_server:call(?MODULE, wait_for_save, infinity).


%% @doc
-spec start_link(nkservice:id()) ->
    {ok, pid()} | {error, term()}.

start_link(SrvId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SrvId], []).




% ===================================================================
%% gen_server behaviour
%% ===================================================================


-record(state, {
    srv :: nkservice:id(),
    to_save = #{} :: #{Hash::integer() => #actor_id{}},
    wait_save = []
}).



%% @private
-spec init(term()) ->
    {ok, tuple()} | {ok, tuple(), timeout()|hibernate} |
    {stop, term()} | ignore.

init([SrvId]) ->
    self() ! do_save,
    ets:new(?MODULE, [named_table, public, {keypos, 2}]),
    {ok, #state{srv=SrvId}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(wait_for_save, From, #state{wait_save=Wait}=State) ->
    {noreply, State#state{wait_save=[From|Wait]}};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast({new_event, Hash, Event}, #state{to_save=ToSave}=State) ->
    ToSave2 = ToSave#{Hash => Event},
    {noreply, State#state{to_save=ToSave2}};

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info(do_save, #state{srv=SrvId, to_save=ToSave, wait_save=Wait}=State) ->
    remove_old_hashes(?MAX_REMEMBER_TIME div 1000),
    case maps:values(ToSave) of
        [] ->
            ok;
        Actors ->
            do_save(SrvId, Actors, ?MAX_RETRIES)
    end,
    lists:foreach(
        fun(From) -> gen_server:reply(From, ok) end,
        lists:reverse(Wait)),
    erlang:send_after(?TIME_TO_SAVE, self(), do_save),
    {noreply, State#state{wait_save=[], to_save = #{}}};

handle_info(Info, State) ->
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(_Reason, _State) ->
    ok.



% ===================================================================
%% Internal
%% ===================================================================

-record(hash, {
    hash,
    counter,
    uid,
    name,
    first_time,
    created_time
}).


%% @private
get_hash(#actor{id=ActorId, data=Data, metadata=Meta}=Actor) ->
    #actor_id{uid=UID, name=Name} = ActorId,
    Data2 = Data#{
        <<"firstTimestamp">> := <<>>,
        <<"lastTimestamp">> := <<>>
    },
    0 = maps:get(<<"generation">>, Meta),
    Meta2 = Meta#{
        <<"creationTime">> := <<>>,
        <<"updateTime">> := <<>>
    },
    Actor2 = Actor#actor{
        id = ActorId#actor_id{uid = <<>>, name = <<>>},
        data = Data2,
        metadata = Meta2,
        hash = <<>>
    },
    #hash{
        hash = erlang:phash2(Actor2),
        counter = 0,
        uid = UID,
        name = Name,
        first_time = maps:get(<<"firstTimestamp">>, Data),
        created_time = maps:get(<<"creationTime">>, Meta)
    }.


%% @private
update_counter(#actor{id=ActorId, data=Data, metadata=Meta}=Actor) ->
    #hash{hash=Hash}=HashInfo = get_hash(Actor),
    Counter2 = ets:update_counter(?MODULE, Hash, {#hash.counter, 1}, HashInfo),
    case Counter2 of
        1 ->
            {created, Actor, Hash};
        _ ->
            [HashInfo2] = ets:lookup(?MODULE, Hash),
            #hash{uid=UID, name=Name, first_time=First, created_time=Create} = HashInfo2,
            Data2 = Data#{
                <<"count">> := Counter2,
                <<"firstTimestamp">> := First
            },
            Meta2 = Meta#{<<"creationTime">> := Create, <<"generation">>:=Counter2-1},
            Actor2 = Actor#actor{
                id = ActorId#actor_id{uid = UID, name = Name},
                data = Data2,
                metadata = Meta2
            },
            {updated, Actor2, Hash}
    end.


%% @private
%% Remove hashes with firstTime older that Secs
remove_old_hashes(Secs) ->
    Max = nklib_date:epoch(usecs) - Secs*1000000,
    {ok, Time} = nklib_date:to_3339(Max, usecs),
    {Time, ets:select_delete(?MODULE, [{{hash, '$1','$2','$3','$4','$5','$6'},[],[{'<','$5',Time}]}])}.


%% @private
do_save(SrvId, Actors, Tries) when Tries > 0 ->
    case ?CALL_SRV(SrvId, actor_db_update, [SrvId, Actors]) of
        {ok, _Meta} ->
            ok;
        {error, Error} ->
            ?LLOG(warning, "could not save events: ~p (~p tries left)", [Error, Tries]),
            timer:sleep(?RETRY_WAIT_TIME),
            do_save(SrvId, Actors, Tries-1)
    end;

do_save(_SrvId, _Actors, _Tries) ->
    error.




%% ===================================================================
%% EUnit tests
%% ===================================================================

test() ->
    remove_old_hashes(0),
    Base = nklib_date:epoch(secs) - 6,
    {ok, Time1} = nklib_date:to_3339(Base*1000, msecs),
    Data1 = #{
        <<"type">> => <<"type1">>,
        <<"reason">> => <<"reason1">>,
        <<"involvedObject">> => <<"obj1">>,
        <<"source">> => <<"src1">>,
        <<"message">> => <<"msg1">>,
        <<"count">> => 1,
        <<"firstTimestamp">> => Time1,
        <<"lastTimestamp">> => Time1
    },
    Meta1 = #{
        <<"creationTime">> => Time1,
        <<"updateTime">> => Time1,
        <<"generation">> => 0,
        <<"field1">> => <<"value1">>
    },
    Actor1 = #actor{
        id = #actor_id{uid = <<"uid1">>, name = <<"name1">>},
        data = Data1,
        metadata = Meta1,
        hash = <<"0">>
    },

    % This is the first message in the series
    {created, Actor1, Hash1} = update_counter(Actor1),

    % Second message, a second later
    {ok, Time2} = nklib_date:to_3339(Base*1000+1, msecs),
    Data2 = Data1#{
        <<"firstTimestamp">> => Time2,
        <<"lastTimestamp">> => Time2,
        <<"count">> => 1
    },
    Meta2 = Meta1#{
        <<"creationTime">> => Time2,
        <<"updateTime">> => Time2
    },
    Actor2 = #actor{
        id = #actor_id{uid = <<"uid2">>, name = <<"name2">>},
        data = Data2,
        metadata = Meta2,
        hash = <<"2">>
    },
    {updated, Actor3, Hash1} = update_counter(Actor2),
    Data3 = Data1#{<<"count">>:=2, <<"firstTimestamp">>:=Time1, <<"lastTimestamp">>:=Time2},
    Meta3 = Meta2#{<<"creationTime">>:=Time1, <<"generation">>:=1},
    #actor{
        id = #actor_id{uid = <<"uid1">>, name = <<"name1">>},
        data = Data3,
        metadata = Meta3
    } = Actor3,

    % Third, in the same second
    Actor4 = Actor2#actor{
        id = #actor_id{uid = <<"uid4">>, name = <<"name4">>}
    },
    {updated, Actor5, Hash1} = update_counter(Actor4),
    Data5 = Data3#{<<"count">>:=3},
    Meta5 = Meta3#{<<"generation">>:=2},
    #actor{
        id = #actor_id{uid = <<"uid1">>, name = <<"name1">>},
        data = Data5,
        metadata = Meta5
    } = Actor5,


    % If we change something, it is another message
    Actor6 = Actor4#actor{
        id = #actor_id{uid = <<"uid6">>, name = <<"name6">>},
        data = Data1,
        metadata = Meta1#{<<"field1">> => <<"value2">>}
    },
    {created, Actor6, Hash2} = update_counter(Actor6),
    true = Hash2 /= Hash1,


    % We send the previous again, a second later
    {ok, Time7} = nklib_date:to_3339(Base*1000+2, msecs),
    Data7 = Data1#{<<"firstTimestamp">>:=Time7, <<"lastTimestamp">>:=Time7},
    Meta7 = Meta1#{
        <<"creationTime">> => Time7,
        <<"updateTime">> => Time7
    },
    Actor7 = #actor{
        id = #actor_id{uid = <<"uid7">>, name = <<"name7">>},
        data = Data7,
        metadata = Meta7,
        hash = <<"3">>
    },
    {ProcT1, {updated, Actor8, Hash1}} = timer:tc(fun() -> update_counter(Actor7) end),
    io:format("Update1 time: ~pusecs\n", [ProcT1]),
    Data8 = Data1#{<<"count">>:=4, <<"firstTimestamp">>:=Time1, <<"lastTimestamp">>:=Time7},
    Meta8 = Meta7#{<<"creationTime">>:=Time1, <<"generation">>:=3},
    #actor{
        id = #actor_id{uid = <<"uid1">>, name = <<"name1">>},
        data = Data8,
        metadata = Meta8
    } = Actor8,


    % We reset timers older than 1 sec, counter is 1 again
    {_, 2} = remove_old_hashes(0),
    {ProcT2, {created, Actor7, Hash1}} = timer:tc(fun() -> update_counter(Actor7) end),
    {_, 1} = remove_old_hashes(0),
    io:format("Update2 time: ~pusecs\n", [ProcT2]),
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    test().

-endif.

