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
%%      - Export a function request/3 to support special API operations
%%      - Export make_external/2 to change default external API representation of actor
%%        (for example, to show some 'status' info, remove a field, etc.)
%%      - Export a funciontion update/2 to check and allow updates (see file.provider sample)
%% - Document initialization operations over init/2 (if used)
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
-export([actor_create/3, actor_activate/3, actor_register/2]).
-export([parse/4, make_external/4]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Actor: "++Txt, Args)).




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Called from nkdomain_callbacks:actor_create()
actor_create(SrvId, Actor, ExtraConfig) ->
    do_actor_activate(SrvId, Actor, true, ExtraConfig).


%% @doc Called from nkdomain_callbacks:actor_activate()
actor_activate(SrvId, Actor, ExtraConfig) ->
    do_actor_activate(SrvId, Actor, false, ExtraConfig).


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
            case nkdomain_register:register_actor(SrvId, ActorId) of
                {ok, Pid} ->
                    {ok, Pid, ActorSt};
                {error, Error} ->
                    {error, Error}
            end
    end.



%% @doc Used to parse an actor, trying the module callback first
%% Vsn must be already set in actor_id.
%% Kind is no expected in 'data' (so that modules don't wait for it) and will be added
%% @see task actor for an example of direct parsing
parse(SrvId, Actor, #{module:=Module, camel:=Kind}, ApiReq) ->
    case nkservice_actor:parse(Module, SrvId, Actor, ApiReq) of
        {ok, #actor{data=Data2}=Actor2} ->
            {ok, Actor2#actor{data=Data2#{<<"kind">> => Kind}}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Used to update the external API representation of an actor
%% to match a version and also to filter and populate 'data' (specially, data.status)
%% If Vsn is 'undefined' actor can use it's last version
-spec make_external(nkservice:id(), #actor{}, #{module:=module()}, nkdomain_api:vsn()|undefined) ->
    #actor{}.

make_external(SrvId, Actor, #{module:=Module}, Vsn) ->
    Actor2 = nkservice_actor:make_external(Module, SrvId, Actor, Vsn),
    #actor{id=#actor_id{pid=Pid}, data=Data} = Actor2,
    case is_pid(Pid) of
        true ->
            Status1 = maps:get(<<"status">>, Data, #{}),
            Status2 = Status1#{<<"isActivated">> => true},
            Data3 = Data#{<<"status">> => Status2},
            Actor2#actor{data=Data3};
        false ->
            Actor2
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_actor_activate(SrvId, Actor, IsNew, ExtraConfig) ->
    #actor{id=ActorId, metadata=Meta} = Actor,
    #actor_id{domain=Domain, group=Group, resource=Resource, name=Name} = ActorId,
    case {Domain, Group, Resource, Name} of
        {?ROOT_DOMAIN, ?GROUP_CORE, ?RES_CORE_DOMAINS, ?ROOT_DOMAIN} ->
            do_load_actor(SrvId, Actor, IsNew, ExtraConfig);
        _ ->
            % Start the domain object
            DomainLink = nkdomain_actor_util:link_key2(?GROUP_CORE, ?LINK_CORE_DOMAIN),
            case Meta of
                #{<<"links">>:=#{DomainLink:=DomUID}} ->
                    % Activate the domain, if not active
                    case nkservice_actor:activate({SrvId, DomUID}, #{}) of
                        {ok, #actor_id{}, _} ->
                            do_load_actor(SrvId, Actor, IsNew, ExtraConfig);
                        {error, Error} ->
                            ?LLOG(warning, "Cannot activate service ~s: ~p", [SrvId, Error]),
                            {error, Error}
                    end;
                _ ->
                    {error, actor_is_not_activable}
            end
    end.


%% @private
do_load_actor(SrvId, Actor, IsNew, ExtraConfig) ->
    Fun = case IsNew of
        true -> create;
        false -> start
    end,
    Node = node(),
    case rpc:call(Node, nkservice_actor_srv, Fun, [SrvId, Actor, ExtraConfig], 15000) of
        {ok, #actor_id{pid=Pid}=ActorId} when is_pid(Pid) ->
            {ok, ActorId};
        {badrpc, Reason} ->
            lager:error("NKLOG RPC ERROR ~p", [Reason]),
            {error, {badrpc, Reason}};
        {error, Error} ->
            {error, Error}
    end.


