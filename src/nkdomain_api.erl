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

%% @doc NkDomain API processing
-module(nkdomain_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/2]).
-export([api_object_to_actor/3, actor_to_external/2, actor_to_external/3]).
-export([set_debug/1, status/2]).
-export_type([verb/0, group/0, vsn/0, api_vsn/0, kind/0, resource/0, subresource/0, params/0]).
-export_type([request/0, response/0, api_event/0]).

-include("nkdomain.hrl").
-include("nkdomain_api.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
%%-include_lib("nkpacket/include/nkpacket.hrl").

-define(MAX_BODY_SIZE, 100000).


%% ===================================================================
%% Types
%% ===================================================================

-type verb() ::
    create | delete | deletecollection | get | list | patch | update | watch | atom().

-type group() :: nkservice_actor:group().      %% "core"

-type vsn() :: nkservice_actor:vsn().        %% "v1b1"

-type api_vsn() :: binary().        %% "core/v1b1"

-type kind() :: binary().           %% "User"

-type resource() :: binary().       %% "users"

-type domain() :: nkdomain:domain().     %% "root", "a.b.c"

-type subresource() :: [binary()].

-type params() :: #{atom() => term()}.

-type request() ::
    #{
        verb => verb(),
        group => group(),
        vsn => vsn(),
        domain => domain(),
        resource => resource(),
        name => nkservice_actor:name(),
        subresource => subresource(),
        params => #{binary() => binary()},
        body => term(),
        auth => map(),
        callback => module(),           % Implementing nkdomain_api behaviour
        url => binary(),                % External url to use in callbacks
        srv => nkservice:id(),          % Service that received the request
        start_time => nklib_date:epoch(usecs),
        trace => [{Time::integer(), Op::term(), Meta::map()}],
        meta => map()
    }.

-type response() ::
    ok | {ok, map()} | {ok, map(), request()} |
    {created, map()} |
    {status, nkservice:msg()} | {status, nkservice:msg(), request()} |  %% See status/2
    {error, nkservice:msg()} | {error, nkservice:msg(), request()}.


-type reply() ::
    {ok|created|binary|error, map(), request()}.


-type api_event() ::
    #{
        type => normal | warning,   % Default normal
        reason => binary(),
        message => binary(),
        body => map(),
        related => #{Type::binary() => nkservice_actor:uid()}
    }.

%% ===================================================================
%% API Invocation
%% ===================================================================



%% @doc Launches an API
%% SrvId will be the service processing the request
-spec request(nkservice:id(), map()) ->
    reply().

request(SrvId, ApiReq) ->
    % Gets group and vsn from request or body
    case nkdomain_api_lib:parse_api_request(ApiReq) of
        {ok, #{group:=Group, vsn:=Vsn}=ApiReq2} ->
            set_debug(SrvId),
            ApiReq3 = ApiReq2#{
                start_time => nklib_date:epoch(usecs),
                trace => []
            },
            ApiReq4 = add_trace(api_start, none, ApiReq3),
            Reply = ?CALL_SRV(SrvId, nkdomain_api_request, [SrvId, Group, Vsn, ApiReq4]),
            case reply(SrvId, Reply, ApiReq4) of
                {raw, {CT, Bin}} ->
                    ?API_DEBUG("reply raw: ~p", [CT], ApiReq4),
                    {raw, {CT, Bin}, add_trace(api_reply, raw, ApiReq4)};
                {Status, Body, ApiReq5} ->
                    ?API_DEBUG("reply: ~p", [Reply], ApiReq4),
                    {Status, Body, add_trace(api_reply, {Status, Body}, ApiReq5)}
            end
    end.


%% @doc
reply(SrvId, Term, ApiReq) ->
    case Term of
        Op when Op==ok; Op==created ->
            {Op, #{}, ApiReq};
        {Op, Reply} when Op==ok; Op==created ->
            {Op, Reply, ApiReq};
        {Op, Reply, ApiReq2} when Op==ok; Op==created ->
            {Op, Reply, ApiReq2};
        {error, Msg} ->
            {error, status(SrvId, Msg), ApiReq};
        {error, Msg, ApiReq2} ->
            {error, status(SrvId, Msg), ApiReq2};
        {status, Msg} ->
            {ok, status(SrvId, Msg), ApiReq};
        {status, Msg, Api2} ->
            {ok, status(SrvId, Msg), Api2};
        {raw, {CT, Body}} ->
            {raw, {CT, Body}};
        Other ->
            ?API_LLOG(error, "Invalid API response: ~p", [Other]),
            {error, status(SrvId, internal_error), ApiReq}
    end.


%% @doc 
add_trace(Op, Meta, ApiReq) ->
    #{trace:=Trace} = ApiReq,
    Now = nklib_date:epoch(usecs),
    Trace2 = case Trace of
        [] ->
            [{undefined, Op, Meta, Now}];
        [{undefined, LastOp, LastMeta, LastTime}|Rest] ->
            [
                {undefined, Op, Meta, Now},
                {LastTime-Now, LastOp, LastMeta, LastTime} |
                Rest
            ]
    end,
    ApiReq#{trace:=Trace2}.



%% ===================================================================
%% Utilities
%% ===================================================================




%% @doc
set_debug(SrvId) when is_atom(SrvId) ->
    Debug = nkservice_util:get_debug(SrvId, nkdomain, single, {api, erlang})==true,
    put(nkdomain_debug, Debug),
    ?API_DEBUG("debug started", []).


%% @doc
%% Fixed values: apiVersion, kind, domain, name
%% If present in fixed, should be the same
%% Parses metadata
api_object_to_actor(SrvId, ActorId, Obj) ->
    % 'data' has everything except fields kind, apiVersion and metadata,
    % kind is extracted and added after parsing
    % apiVersion is extracted to #actor_id{}
    % metadata has its own field
    Data = maps:without([<<"apiVersion">>, <<"kind">>, <<"metadata">>], Obj),
    Meta = maps:get(<<"metadata">>, Obj, #{}),
    MetaSyntax = nkservice_actor_syntax:meta_syntax(),
    case nklib_syntax:parse(Meta, MetaSyntax, #{path=><<"metadata">>}) of
        {ok, Meta2, _} ->
            % Fields 'uid', 'name' and 'domain' are represented in #actor_id{}
            Meta3 = maps:without([<<"uid">>, <<"name">>, <<"domain">>, <<"resourceVersion">>], Meta2),
            Actor1 = #actor{
                id = ActorId#actor_id{
                    uid = maps:get(<<"uid">>, Meta, undefined)
                },
                data = Data,
                metadata = Meta3,
                hash = maps:get(<<"resourceVersion">>, Meta, <<>>)
            },
            case nkdomain_api_lib:process_links(SrvId, Actor1) of
                {ok, Actor2} ->
                    {ok, Actor2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Called to generate an API actor from an internal actor
%% SrvId is the processing service, must have plugins to process this Actor
-spec actor_to_external(nkservice:id(), nkservice_actor:actor()) ->
    {ok, ApiActor::map()} | {error, term()}.

actor_to_external(SrvId, Actor) ->
    actor_to_external(SrvId, Actor, undefined).


%% @doc Generates an external representation of the Actor, corresponding to Vsn
-spec actor_to_external(nkservice:id(), nkservice_actor:actor(), nkdomain_api:vsn()|undefined) ->
    {ok, ApiActor::map()} | {error, term()}.

actor_to_external(SrvId, Actor, Vsn) ->
    try
        #actor{
            id = #actor_id{
                domain = Domain,
                uid =  UID,
                group = Group,
                resource = Resource,
                name =  Name
            },
            hash = Hash
        } = Actor,
        Config = case catch nkdomain_actor_util:get_config(SrvId, Group, Resource) of
            {ok, Config0} ->
                Config0;
            {error, Error} ->
                throw({error, Error})
        end,
        Actor2 = nkdomain_actor:make_external(SrvId, Actor, Config, Vsn),
        #actor{id=#actor_id{vsn=Vsn2}, data=Data, metadata=Meta} = Actor2,
        case Vsn /= undefined andalso Vsn /= Vsn2 of
            true ->
                ?API_LLOG(warning, "returning incompatible external actor ~p (~p)", [Actor2, Vsn]);
            false ->
                ok
        end,
        ApiVsn = <<Group/binary, $/, Vsn2/binary>>,
        #{camel:=Kind} = Config,
        Self = <<
            "/apis/", ApiVsn/binary, "/domains/", Domain/binary,
            $/, Resource/binary, $/, Name/binary
        >>,
        ApiActor = Data#{
            <<"apiVersion">> => ApiVsn,
            <<"kind">> => Kind,
            <<"metadata">> => Meta#{
                <<"name">> => Name,
                <<"uid">> => UID,
                <<"domain">> => Domain,
                <<"selfLink">> => Self,
                <<"resourceVersion">> => Hash
            },
            <<"status">> => maps:get(<<"status">>, Data, #{})
        },
        {ok, ApiActor}
    catch
        throw:Throw ->
            Throw
    end.


%% @doc Reports an standardized error
% https://github.com/kubernetes/community/blob/master/contributors/devel/api-conventions.md#response-status-kind
-spec status(nkservice:id(), nkservice_msg:msg()) ->
    map().

status(_, #{<<"kind">> := <<"Status">>, <<"code">> := _} = Status) ->
    Status;

status(SrvId, Msg) ->
    {HttpCode, Details} = case ?CALL_SRV(SrvId, status, [Msg]) of
        unknown ->
            lager:warning("Unknown DOMAIN HTTP API Response: ~p", [Msg]),
            {500, #{}};
        {HttpCode0, Status0} ->
            {HttpCode0, Status0}
    end,
    Status = case HttpCode < 400 of
        true ->
            <<"Success">>;
        false ->
            <<"Faillure">>
    end,
    {Reason, StatusMsg} = nkservice_msg:msg(SrvId, Msg),
    #{
        <<"apiVersion">> => <<"v1">>,
        <<"kind">> => <<"Status">>,
        <<"metadata">> => #{},
        <<"status">> => Status,
        <<"message">> => StatusMsg,
        <<"reason">> => Reason,
        <<"details">> => Details,
        <<"code">> => HttpCode
    }.


