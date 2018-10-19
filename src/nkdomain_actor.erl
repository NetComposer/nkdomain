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

%% @doc NkDomain Actor behaviour
%%
%% Steps to create a new actor
%% ---------------------------
%%
%% - Create a callback module implementing behaviour nkdomain_actor
%% - Create type 'run_status' for actor's run_status (if used)
%% - Create type 'event' for raised (erlang) events
%% - If possible, document also (for each supported version)
%%      - Format for 'spec' (documented in parse/2)
%%      - Format for 'status' and any other fist-level field
%%      - Sync and Async operations (documented in sync_op/3 and async_op/2)
%%      - APIs (documented in request/5)
%% - To convert some Erlang events to API events, implement event/2 and
%%   call nkdomain_actor_util:api_event/2.
%% - Add the callback module to the actorModules key in the Domain package config
%%   (if core, to nkdomain_plugin:core_actor_modules/0)
%% - Export a function config/0 in the callback module
%%      - See nkdomain:actor_config()
%%      - Set at least group, versios, type, verbs
%%      - Set camel, singular or they will be generated automatically
%% - Optionally:
%%      - export a function parse/1 with the syntaxis for the type
%%        It will be called when creating or updating the actor
%%          - Use camelCase and binaries for fields
%%          - Date fields in RFC3339 must end in "Time"
%%      - Export a function sync_op/3 or async_op/2 to support special operations
%%      - Export a function request/5 to support special API operations
%%      - Export make_external/2 to change default external API representation of actor
%%        (for example, to show some 'status' info, remove a field, etc.)
%%      - Export a funciontion update/2 to check and allow updates (see file.provider sample)
%% - Document initialization operations over init/1 (if used)
%% - Write a test to check specific operations
%%
%%
%% Add GraphQL support
%% ---------------------------
%% - Create a callback module implementing behaviour nkservice_graphql_schema
%% - Add the callback module to the graphqlActorModules key in the Domain package config
%%   (if core, to nkdomain_plugin:core_grahql_modules/0)
%% - Export a function config/0 in the callback module
%%      - Set at least type, actor_group, actor_resource
%% - Export a function schema/2 with the schema definition and execute to get fields
%% - Optional, export connections, query, mutation
%% - Write a GraphQL test to check all fields, filter fields and sort fields
%%
%%
%% Actor behaviour
%% ---------------
%%
%% Any time an actor is to activated, actor_activate/2 is called
%% - The domain actor for the activating actor's domain is activated
%%   It will start the service if not already started (see nkdomain_actor_domain)
%% - The config is read from the config callback
%% - The actor is loaded
%% - If the domain actor is disabled, it won't start the service, and
%%   no leader will be found, son the actor cannot be activated
%%
%%
%% To add fields to an actor (spec or status keys)
%% -----------------------------------------------
%%
%% - Document them in actor's callback module
%% - Add them to actor's syntax parse/2
%% - If you can filter or sort on it,
%%   add it to keys filter_fields, sort_fields and field_type in config/1
%% - Add schema to actor's graphql callback module
%% - Add execute to actor's graphql callback module
%%
%%
%% To set a new metadata field:
%% ----------------------------
%%
%% - Document it in nkservice_actor_srv
%% - Add it to nkservice_actor_syntax
%% - If you can filter or sort on it,
%%   add it to filter_fields(), sort_fields(), field_type() here
%% - Add schema to nkdomain_graphql_schema
%% - Add execute to nkdomain_graphql_execute
%%
%%
%% Error codes
%% -----------
%% To set a new error code, add it to msg/1 in callback module, and
%% to status/1 to get a status response on API requests
%% Error should sort meaningfully, for example 'url_unknown' instead of 'unknown_url'
%% Text should be human-good-sounding though
%%


-module(nkdomain_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([config/1, parse/4, request/4, make_external/4]).
-export([actor_srv_init/2, actor_srv_sync_op/3, actor_srv_async_op/2,
         actor_srv_heartbeat/1, actor_srv_enabled/2, actor_srv_update/2,
         actor_srv_handle_call/3, actor_srv_handle_cast/2, actor_srv_handle_info/2,
         actor_srv_event/2, actor_srv_stop/2, actor_srv_terminate/2]).
-export([actor_create/3, actor_activate/3, actor_register/2]).
-export([filter_fields/0, sort_fields/0, field_type/0, field_trans/0]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Actor: "++Txt, Args)).



%% ===================================================================
%% Types
%% ===================================================================

-type actor_st() :: #actor_st{}.
-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


%% @doc Called to get the actor's config
-callback config() -> nkdomain:actor_config().


%% @doc Called to parse an actor of this type
%% Can return:
%% - {actor_spec, Syntax}: this Syntax will be applied to spec field
%%
%% If not implemented, anything is allowed in spec
-callback parse(nkservice:id(), nkservice_actor:actor(), nkdomain_api:request()) ->
    {actor_spec, nklib_syntax:syntax()}.     % Return syntax for our spec


%% @doc Called to process an incoming API
%% SrvId will be the service supporting the domain in ApiReq
%% If not implemented, or 'continue' is returned, standard processing will apply
-callback request(nkservice:id(), nkdomain_api:verb(), #actor_id{},
                  nkdomain:actor_config(), nkdomain_api:api_req()) ->
    nkdomain_api:response() | continue.


%% @doc Called to change the external representation of an actor,
%% for example to change date's format. Vsn will be the current ApiVsn asking for it
-callback make_external(nkservice:id(), nkservice:actor(), nkdomain_api:vsn()) ->
    {ok, nkservice:actor()} | continue.


%% @doc Called when a new actor starts
-callback init(actor_st()) ->
    {ok, actor_st()} | {error, Reason::term()}.


%% @doc Called to process sync operations
-callback update(nkservice:actor(), actor_st()) ->
    {ok, nkservice:actor(), actor_st()} | {error, nkservice:msg(), actor_st()}.


%% @doc Called to process sync operations
-callback sync_op(term(), {pid(), reference()}, actor_st()) ->
    {reply, Reply::term(), actor_st()} | {reply_and_save, Reply::term(), actor_st()} |
    {noreply, actor_st()} | {noreply_and_save, actor_st()} |
    {stop, Reason::term(), Reply::term(), actor_st()} |
    {stop, Reason::term(), actor_st()} |
    continue().


%% @doc Called to process async operations
-callback async_op(term(), actor_st()) ->
    {noreply, actor_st()} | {noreply_and_save, actor_st()} |
    {stop, Reason::term(), actor_st()} |
    continue().


%%  @doc Called when an actor is sent inside the actor process
%%  Can be used to launch API events, calling
-callback event(term(), actor_st()) ->
    {ok, actor_st()} | continue().


%% @doc Called when an event is sent, for each registered process to the session
%% The events are 'erlang' events (tuples usually)
-callback link_event(nklib:link(), term(), nkservice_actor_srv:event(), actor_st()) ->
    {ok, actor_st()} | continue().


%% @doc Called when an object is enabled/disabled
-callback enabled(boolean(), actor_st()) ->
    {ok, actor_st()} | continue().


%% @doc Called on actor heartbeat (5 secs)
-callback heartbeat(actor_st()) ->
    {ok, actor_st()} | {error, nkservice_msg:msg(), actor_st()} | continue().


%% @doc
-callback handle_call(term(), {pid(), term()}, actor_st()) ->
    {reply, term(), actor_st()} | {noreply, actor_st()} |
    {stop, Reason::term(), Reply::term(), actor_st()} |
    {stop, Reason::term(), actor_st()} | continue().


%% @doc
-callback handle_cast(term(), actor_st()) ->
    {noreply, actor_st()} | {stop, term(), actor_st()} | continue().


%% @doc
-callback handle_info(term(), actor_st()) ->
    {noreply, actor_st()} | {stop, term(), actor_st()} | continue().


%% @doc
-callback stop(Reason::term(), actor_st()) ->
    {ok, actor_st()}.


%% @doc
-callback terminate(Reason::term(), actor_st()) ->
    {ok, actor_st()}.


%% @doc
-optional_callbacks([
    request/5, parse/3, make_external/3,
    init/1, update/2, sync_op/3, async_op/2, enabled/2, heartbeat/1,
    event/2, link_event/4, handle_call/3, handle_cast/2, handle_info/2,
    stop/2, terminate/2]).



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Called from nkdomain_callbacks:actor_create()
actor_create(SrvId, Actor, Opts) ->
    do_actor_activate(SrvId, Actor, true, Opts).


%% @doc Called from nkdomain_callbacks:actor_activate()
actor_activate(SrvId, Actor, Opts) ->
    do_actor_activate(SrvId, Actor, false, Opts).


%% @doc Called from nkdomain_callbacks for actor registration
actor_register(SrvId, ActorSt) ->
    #actor_st{actor=#actor{id=ActorId}} = ActorSt,
    case ActorId of
        #actor_id{domain=?ROOT_DOMAIN, group=?GROUP_CORE, resource=?RES_CORE_DOMAINS, name=?ROOT_DOMAIN} ->
            case nkservice_master:get_leader_pid(SrvId) of
                Pid when is_pid(Pid) ->
                    {ok, Pid, ActorSt};
                undefined ->
                    {error, service_leader_not_available}
            end;
        _ ->
            case nkdomain_domain:register_actor(SrvId, ActorId) of
                {ok, Pid} ->
                    {ok, Pid, ActorSt};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% ===================================================================
%% Actor Proxy
%% ===================================================================


%% @doc Used to call the 'config' callback on an actor's module
%% You normally will use nkdomain_util:get_config/2, as its has a complete set of fields
config(Module) ->
    case erlang:function_exported(Module, config, 0) of
        true ->
            Module:config();
        false ->
            not_exported
    end.


%% @doc Used to parse an actor, trying the module callback first
%% Must set also vsn
%% @see task actor for an example of direct parsing
parse(SrvId, Actor, #{module:=Module}, ApiReq) ->
    SynSpec = case erlang:function_exported(Module, parse, 3) of
        true ->
            apply(Module, parse, [SrvId, Actor, ApiReq]);
        false ->
            {syntax, #{}}
    end,
    case SynSpec of
        {ok, #actor{}=Actor2} ->
            {ok, Actor2};
        {syntax, Syntax} when is_map(Syntax) ->
            nkdomain_actor_util:parse_actor(Actor, Syntax, ApiReq);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Used to call the 'request' callback on an actor's module
request(SrvId, ActorId, #{module:=Module}=Config, #{verb:=Verb}=ApiReq) ->
    case erlang:function_exported(Module, request, 5) of
        true ->
            Module:request(SrvId, Verb, ActorId, Config, ApiReq);
        false ->
            continue
    end;

request(_SrvId, _ActorId, _Config, _ApiReq) ->
    continue.


%% @doc Used to update the external API representation of an actor
%% to match a version and also to filter and populate 'data' (specially, data.status)
%% If Vsn is 'undefined' actor can use it's last version
-spec make_external(nkservice:id(), nkdomain:config(), #actor{}, nkdomain_api:vsn()|undefined) ->
    #actor{}.

make_external(SrvId, #{module:=Module}, Actor, Vsn) ->
    Actor3 = case erlang:function_exported(Module, make_external, 3) of
        true ->
            case Module:make_external(SrvId, Actor, Vsn) of
                continue ->
                    Actor;
                {ok, Actor2} ->
                    Actor2
            end;
        false ->
            Actor
    end,
    #actor{id=#actor_id{pid=Pid}, data=Data} = Actor3,
    case is_pid(Pid) of
        true ->
            Status1 = maps:get(<<"status">>, Data, #{}),
            Status2 = Status1#{<<"isActivated">> => true},
            Data3 = Data#{<<"status">> => Status2},
            Actor3#actor{data=Data3};
        false ->
            Actor3
    end.


%% @doc Called from nkdomain_callbacks for actor initialization
actor_srv_init(_StartOpts, ActorSt) ->
    call_actor(init, [ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for actor heartbeat
actor_srv_heartbeat(ActorSt) ->
    call_actor(heartbeat, [ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks after enable change
actor_srv_enabled(Enabled, ActorSt) ->
    call_actor(enabled, [Enabled, ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks before terminating an update
actor_srv_update(Actor, ActorSt) ->
    call_actor(update, [Actor, ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for sync operations
actor_srv_sync_op(Op, From, ActorSt) ->
    call_actor(sync_op, [Op, From, ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for async operations
actor_srv_async_op(Op, ActorSt) ->
    call_actor(async_op, [Op,  ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks when a event is sent
actor_srv_event(Op, ActorSt) ->
    call_actor(event, [Op,  ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for handle_call
actor_srv_handle_call(Msg, From, ActorSt) ->
    call_actor(handle_call, [Msg, From, ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for handle_cast
actor_srv_handle_cast(Msg, ActorSt) ->
    call_actor(handle_cast, [Msg, ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for handle_info
actor_srv_handle_info(Msg, ActorSt) ->
    call_actor(handle_info, [Msg, ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for actor termination
actor_srv_stop(Reason, ActorSt) ->
    call_actor(stop, [Reason,  ActorSt], ActorSt).


%% @doc Called from nkdomain_callbacks for actor termination
actor_srv_terminate(Reason, ActorSt) ->
    call_actor(terminate, [Reason,  ActorSt], ActorSt).


%% ===================================================================
%% Common fields
%% ===================================================================


%% @doc Default filter fields
%% Must be sorted!
filter_fields() ->
    [
        <<"apiGroup">>,
        <<"domain">>,
        <<"groups">>,
        <<"group+resource">>,           % Maps to special group + resource
        <<"kind">>,
        <<"metadata.createdBy">>,
        <<"metadata.creationTime">>,
        <<"metadata.domain">>,
        <<"metadata.expiresTime">>,
        <<"metadata.generation">>,
        <<"metadata.isEnabled">>,
        <<"metadata.isInAlarm">>,
        <<"metadata.name">>,
        <<"metadata.resourceVersion">>,
        <<"metadata.subtype">>,
        <<"metadata.uid">>,
        <<"metadata.updateTime">>,
        <<"metadata.updatedBy">>,
        <<"name">>,
        <<"path">>,
        <<"resource">>,
        <<"srv">>,
        <<"uid">>,
        <<"vsn">>
    ].


%% @doc Default sort fields
%% Must be sorted!
sort_fields() ->
    [
        <<"apiGroup">>,
        <<"domain">>,
        <<"group">>,
        <<"group+resource">>,
        <<"kind">>,
        <<"metadata.createdBy">>,
        <<"metadata.creationTime">>,
        <<"metadata.domain">>,
        <<"metadata.expiresTime">>,
        <<"metadata.generation">>,
        <<"metadata.isEnabled">>,
        <<"metadata.isInAlarm">>,
        <<"metadata.name">>,
        <<"metadata.subtype">>,
        <<"metadata.updateTime">>,
        <<"metadata.updatedBy">>,
        <<"name">>,
        <<"path">>,
        <<"srv">>
    ].


%% @doc
field_trans() ->
    #{
        <<"apiGroup">> => <<"group">>,
        <<"kind">> => <<"data.kind">>,
        <<"metadata.uid">> => <<"uid">>,
        <<"metadata.name">> => <<"name">>
    }.


%% @doc Field value, applied after trans
field_type() ->
    #{
        <<"metadata.generation">> => integer,
        <<"metadata.isEnabled">> => boolean,
        <<"metadata.isInAlarm">> => boolean
    }.




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
call_actor(Fun, Args, #actor_st{module=Module}) ->
    case erlang:function_exported(Module, Fun, length(Args)) of
        true ->
            apply(Module, Fun, Args);
        false ->
            continue
    end;

call_actor(_Fun, _Args, _ActorSt) ->
    continue.


%% @private
do_actor_activate(SrvId, Actor, IsNew, Opts) ->
    #actor{id=ActorId, metadata=Meta} = Actor,
    #actor_id{domain=Domain, group=Group, resource=Resource, name=Name} = ActorId,
    case {Domain, Group, Resource, Name} of
        {?ROOT_DOMAIN, ?GROUP_CORE, ?RES_CORE_DOMAINS, ?ROOT_DOMAIN} ->
            do_load_actor(SrvId, Actor, IsNew, Opts);
        _ ->
            % Start the domain object
            case Meta of
                #{<<"links">>:=#{?RES_CORE_DOMAINS:=DomUID}} ->
                    % Activate the domain, if not active
                    case nkservice_actor_db:activate(SrvId, DomUID, Opts) of
                        {ok, #actor_id{}, _} ->
                            do_load_actor(SrvId, Actor, IsNew, Opts);
                        {error, Error} ->
                            ?LLOG(warning, "Cannot activate service ~s: ~p", [SrvId, Error]),
                            {error, Error}
                    end;
                _ ->
                    {error, actor_is_not_activable}
            end
    end.


%% @private
do_load_actor(SrvId, Actor, IsNew, Opts) ->
    #actor{id=#actor_id{group=Group, resource=Resource}} = Actor,
    {ok, Config1} = nkdomain_actor_util:get_config(SrvId, Group, Resource),
    Config2 = case Opts of
        #{ttl:=TTL} ->
            Config1#{ttl=>TTL};
        _ ->
            Config1
    end,
    LoadOpts = #{config => Config2, module => maps:get(module, Config2)},
    Fun = case IsNew of
        true -> create;
        false -> start
    end,
    Node = node(),
    case rpc:call(Node, nkservice_actor_srv, Fun, [SrvId, Actor, LoadOpts], 15000) of
        {ok, #actor_id{pid=Pid}=ActorId} when is_pid(Pid) ->
            {ok, ActorId};
        {badrpc, Reason} ->
            lager:error("NKLOG RPC ERROR ~p", [Reason]),
            {error, {badrpc, Reason}};
        {error, Error} ->
            {error, Error}
    end.


