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

%% @doc NkDomain service callback module
-module(nkdomain_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([msg/1, status/1]).
-export([nkdomain_api_request/4, nkdomain_get_paths/2, nkdomain_api_get_groups/1,
         nkdomain_api_get_resources/3]).
-export([actor_find_registered/2, actor_get_config/3, actor_activate/3, actor_create/3,
         actor_is_managed/2, actor_external_event/3]).
-export([actor_srv_register/2, actor_srv_event/2, actor_srv_link_event/4]).
-export([actor_db_get_query/4]).
-export([service_master_leader/4]).
-export([nkservice_rest_http/4]).
-export([nkservice_openapi_get_spec/1]).
-export([nkservice_graphql_core_schema/3,
         nkservice_graphql_get_uid/2,
         nkservice_graphql_query/5,
         nkservice_graphql_mutation/5,
         nkservice_graphql_execute/5]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN SRV Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").

%%-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%-type obj_id() :: nkdomain:obj_id().
%-type type() :: nkdomain:type().
%-type path() :: nkdomain:path().
%%-type continue() :: continue | {continue, list()}.
%-type state() :: #obj_state{}.



%% ===================================================================
%% Error
%% ===================================================================


%% @doc
msg({actor_invalid, A})                 -> {"Invalid actor '~s'", [A]};
msg({api_group_unknown, G})             -> {"Unknown API Group '~s'", [G]};
msg({api_incompatible, A})              -> {"Incompatible API '~s'", [A]};
msg({api_unknown, A})                   -> {"Unknown API '~s'", [A]};
msg({body_too_large, Size, Max})        -> {"Body too large (size is ~p, max is ~p)", [Size, Max]};
msg({could_not_load_parent, Id})        -> {"Object could not load parent '~s'", [Id]};
msg({could_not_load_domain, Id})        -> {"Object could not load domain '~s'", [Id]};
msg({could_not_load_user, Id})          -> {"Object could not load user '~s'", [Id]};
msg(content_type_invalid)               -> "ContentType is invalid";
msg(db_not_defined)                     -> "Object database not defined";
msg({domain_unknown, D})                -> {"Unknown domain '~s'", [D]};
msg(domain_invalid)                     -> "Invalid domain";
msg({domain_is_disabled, D})            -> {"Domain '~s' is disabled", [D]};
msg(domains_name_cannot_change)         -> "ObjName cannot be updated for domains";
msg(download_server_error)              -> "Download server error";
msg({email_duplicated, E})              -> {"Duplicated email '~s'", [E]};
msg(element_action_unknown)             -> "Unknown element action";
msg({file_not_found, F})                -> {"File '~s' not found", [F]};
msg(file_is_invalid)                    -> "File is invalid";
msg(file_too_large)                     -> "File is too large";
msg(group_unknown)                      -> "Invalid Group";
msg(invalid_content_type)               -> "Invalid Content-Type";
msg({invalid_name, N})                  -> {"Invalid name '~s'", [N]};
msg(invalid_sessionn)                   -> "Invalid session";
msg(kind_unknown)                       -> "Invalid kind";
msg({kind_unknown, K})                  -> {"Invalid kind '~s'", [K]};
msg(member_already_present)             -> "Member is already present";
msg(member_not_found)                   -> "Member not found";
msg(member_invalid)                     -> "Invalid member";
msg(multiple_ids)                       -> "Multiple matching ids";
msg(missing_auth_header)                -> "Missing authentication header";
msg({module_failed, Module})            -> {"Module '~s' failed", [Module]};
msg(object_access_not_allowed)          -> "Object access is not allowed";
msg(object_already_exists)              -> "Object already exists";
msg({object_already_exists, ObjIdOrP})  -> {"Object already exists: ~s", [ObjIdOrP]};
msg(object_clean_process)               -> "Object cleaned (process stopped)";
msg(object_clean_expire)                -> "Object cleaned (expired)";
msg(object_consumed)                    -> "Object is consumed";
msg({object_consumed, R})               -> {"Object is consumed: ~s", [R]};
msg(object_deleted) 		            -> "Object removed";
msg(object_expired) 		            -> "Object expired";
msg(object_has_childs) 		            -> "Object has childs";
msg({object_load_error, Error}) 		-> {"Object load error: '~p'", [Error]};
msg(object_is_already_loaded)           -> "Object is already loaded";
msg(object_is_disabled) 		        -> "Object is disabled";
msg(object_is_stopped) 		            -> "Object is stopped";
msg(object_not_found) 		            -> "Object not found";
msg(object_not_started) 		        -> "Object is not started";
msg(object_path_invalid)                -> "Invalid object path";
msg({object_path_invalid, P})           -> {"Invalid object path '~s'", [P]};
msg(object_parent_conflict) 	        -> "Object has conflicting parent";
msg(object_stopped) 		            -> "Object stopped";
msg(operation_invalid) 	                -> "Invalid operation";
msg(operation_token_invalid) 	        -> "Operation token is invalid";
msg({parameter_invalid, Txt})      	    -> {"Invalid parameter '~s'", [Txt]};
msg({parameter_missing, Txt})      	    -> {"Missing parameter '~s'", [Txt]};
msg(parent_is_disabled) 		        -> "Parent is disabled";
msg(parent_not_found) 		            -> "Parent not found";
msg(parent_stopped) 		            -> "Parent stopped";
msg(parse_error)   		                -> "Object parse error";
msg(password_valid)                     -> "Password is valid";
msg(password_invalid) 	                -> "Password is not valid";
msg(provider_class_unknown)             -> "Provider class is unknown";
msg(request_body_invalid)               -> "The request body is invalid";
msg(resource_invalid)                   -> "Invalid resource";
msg({resource_invalid, R})              -> {"Invalid resource '~s'", [R]};
msg({resource_invalid, G, R})           -> {"Invalid resource '~s' (~s)", [R, G]};
msg(service_down)                       -> "Service is down";
msg(session_already_present)            -> "Session is already present";
msg(session_not_found)                  -> "Session not found";
msg(session_is_disabled)                -> "Session is disabled";
msg({srv_id_invalid, S})                -> {"Invalid service ~s", [S]};
msg(sso_not_available)                  -> "SSO is not available";
msg(status_not_defined)                 -> "Status is not defined";
msg(store_id_invalid)                   -> "Invalid Store Id";
msg(store_id_missing)                   -> "Missing Store Id";
msg(token_invalid)                      -> "Invalid token";
msg(token_invalid_ttl)                  -> "Invalid token TTL";
msg(token_down)                         -> "Token process is down";
msg(task_max_tries_reached)             -> "Task max tries reached";
msg(task_max_time_reached)              -> "Task max time reached";
msg(uid_not_found)      		        -> "Unknown UID";
msg(upload_server_error)                -> "Upload server error";
msg(url_unknown)      		            -> "Unknown url";
msg(user_is_disabled) 		            -> "User is disabled";
msg(user_unknown)                       -> "Unknown user";
msg(verb_not_allowed)                   -> "Verb is not allowed";
msg(watch_stop)                         -> "Watch stopped";
msg(_)   		                        -> continue.



%% @private
status(ok) -> {200, #{}};
status(password_valid) -> {200, #{}};
status(password_invalid) -> {200, #{}};
status(actor_updated) -> {200, #{}};
status(actor_deleted) -> {200, #{}};
status({actors_deleted, N}) -> {200, #{<<"deleted">>=>N}};
status(watch_stop) -> {200, #{}};
status(redirect) -> {307, #{}};
status({actor_invalid, Map}) when is_map(Map) -> {400, Map};
status(data_value_invalid) -> {400, #{}};
status({field_missing, Field}) -> {400, #{<<"field">>=>Field}};
status({field_invalid, Field}) -> {400, #{<<"field">>=>Field}};
status(content_type_invalid) -> {400, #{}};
status(file_too_large) -> {400, #{}};
status({field_unknown, _}) -> {400, #{}};
status(file_is_invalid) -> {400, #{}};
status({kind_unknown, K}) -> {400, #{<<"kind">> => K}};
status({linked_actor_unknown, Id}) -> {400, #{<<"link">>=>Id}};
status({parameter_invalid, Param}) -> {400, #{<<"parameter">>=>Param}};
status({parameter_missing, Param}) -> {400, #{<<"parameter">>=>Param}};
status(provider_class_unknown) -> {400, #{}};
status({provider_unknown, Name}) -> {400, #{<<"provider">>=>Name}};
status(request_body_invalid) -> {400, #{}};
status({syntax_error, Error}) -> {400, #{<<"error">>=>Error}};
status(ttl_missing) -> {400, #{}};
status(utf8_error) -> {400, #{}};
status(bad_request) -> {400, #{}};
status(unauthorized) -> {401, #{}};
status(forbidden) -> {403, #{}};
status({api_unknown, API}) -> {404, #{<<"api">>=>API}};
status({api_group_unknown, API}) -> {404, #{<<"group">>=>API}};
status({domain_unknown, Domain}) -> {404, #{<<"domain">>=>Domain}};
status(domain_invalid) -> {404, #{}};
status(resource_invalid) -> {404, #{}};
status({resource_invalid, Res}) -> {404, #{<<"resource">>=>Res}};
status({resource_invalid, Group, Path}) -> {404, #{<<"group">>=>Group, <<"resource">>=>Path}};
status(actor_not_found) -> {404, #{}};
status({actor_not_found, A}) -> {404, #{<<"actor">> => A}};
status(not_found) -> {404, #{}};
status(verb_not_allowed) -> {405, #{}};
status(method_not_allowed) -> {405, #{}};
status(uid_not_allowed) -> {409, #{}};
status(linked_actor_unknown) -> {409, #{}};
status(uniqueness_violation) -> {409, #{}};
status(conflict) -> {409, #{}};
status(gone) -> {410, #{}};
status({actor_invalid, A}) -> {422, #{<<"actor">> => A}};
status(actor_is_not_activable) -> {422, #{}};
status(actor_has_linked_actors) -> {422, #{}};
status({api_incompatible, A}) -> {422, #{<<"api">> => A}};
status({domain_is_disabled, D}) -> {422, #{<<"domain">> => D}};
status(nxdomain) -> {422, #{}};
status({service_not_available, Srv}) -> {422, #{<<"service">>=>Srv}};
status(storage_class_invalid) -> {422, #{}};
status(task_max_tries_reached) -> {422, #{}};
status(too_many_records) -> {422, #{}};
status({updated_invalid_field, F}) -> {422, #{field=>F}};
status(unprocessable) -> {422, #{}};
status(too_many_requests) -> {429, #{}};
status(internal_error) -> {500, #{}};
status(service_unavailable) -> {503, #{}};
status(timeout) -> {504, #{}};
status(_) -> {500, #{}}.


%%%% @doc
%%i18n(SrvId, Key, Lang) ->
%%    case nklib_i18n:get(SrvId, Key, Lang) of
%%        <<>> when SrvId == ?NKROOT ->
%%            <<>>;
%%        <<>> ->
%%            nklib_i18n:get(?NKROOT, Key, Lang);
%%        Text ->
%%            Text
%%    end.

%% ===================================================================
%% API Interface
%% ===================================================================


%% @doc Called to add info about all supported APIs
%% Must be implemented by new APIs
-spec nkdomain_api_get_groups(#{nkdomain_api:group() => [nkdomain_api:vsn()]}) ->
    {continue, #{nkdomain_api:group() => [nkdomain_api:vsn()]}}.

nkdomain_api_get_groups(GroupsAcc) ->
    GroupsAcc#{
        ?GROUP_CORE => [?GROUP_CORE_V1A1]
    }.


%% @doc Called to process and incoming API
%% SrvId is service receiving the request
%% Must be implemented by new APIs
-spec nkdomain_api_request(nkservice:id(), nkdomain_api:group(), nkdomain_api:vsn(), nkdomain_api:request()) ->
    nkdomain_api:response().

nkdomain_api_request(SrvId, ?GROUP_CORE, ?GROUP_CORE_V1A1, ApiReq) ->
    nkdomain_api_core_v1:request(SrvId, ApiReq);

nkdomain_api_request(SrvId, ?GROUP_SEARCH, ?GROUP_CORE_V1A1, ApiReq) ->
    nkdomain_core_search:search(SrvId, ApiReq);

nkdomain_api_request(_SrvId, Group, _Vsn, _ApiReq) ->
    {error, {api_unknown, Group}}.


%%%% @doc Called to generate the API Group for an object
%%-spec nkdomain_actor_to_api_group(nkservice:id(), nkdomain_api:group()|undefined,
%%    nkservice_actor:actor()) ->
%%    Group::binary() | continue().
%%
%%nkdomain_actor_to_api_group(_SrvId, _Group, #actor{id=#actor_id2{class=?DOMAIN_CORE}}) ->
%%    ?DOMAIN_CORE_V1A1.


%%%% @doc Called to:
%%%% - Generate apiVersion, Kind and Resource
%%%% - Set any modification on an actor sent to API
%%%% Group is the calling API Group asking for the actor, or 'undefined'
%%%% if there is none
%%-spec nkdomain_actor_to_api(nkservice:id(), nkservice_actor:actor(), nkdomain:actor_config(),
%%    ApiActor::map()) ->
%%    ApiActor::map() | continue.
%%
%%nkdomain_actor_to_api(_SrvId, Actor, Config, ApiActor) ->
%%    #{module:=Module} = Config,
%%    nkdomain_actor:make_external(Module, Actor, ApiActor).



%% @doc Called when the list of base paths of the server is requested
-spec nkdomain_get_paths(nkservice:id(), [binary()]) ->
    [binary()].

nkdomain_get_paths(SrvId, Acc) ->
    nkdomain_lib:get_paths(SrvId, Acc).


%% @doc Called to add info about all supported APIs
-spec nkdomain_api_get_resources(nkservice:id(), nkdomain_api:group(), nkdomain_api:vsn()) ->
    [map()] | undefined.

nkdomain_api_get_resources(SrvId, ApiGroup, ApiVsn) ->
    nkdomain_lib:get_api_resources(SrvId, ApiGroup, ApiVsn).


%% ===================================================================
%% Actor processing
%% ===================================================================

%% @see Implements alternate registrar (called only when actor not cached)
actor_find_registered(_SrvId, Id) ->
    nkdomain_register:find_registered(Id).


%% @see nkservice_callbacks:actor_create/4
actor_create(SrvId, Actor, Config) ->
    nkdomain_actor:actor_create(SrvId, Actor, Config).


%% @see nkservice_callbacks:actor_activate/3
actor_activate(SrvId, Actor, LoadOpts) ->
    nkdomain_actor:actor_activate(SrvId, Actor, LoadOpts).


%% @doc Called when activating an actor to get it's config and module
actor_get_config(SrvId, ActorId, Config) ->
    #actor_id{group=Group, resource=Resource} = ActorId,
    case nkdomain_actor_util:get_config(SrvId, Group, Resource) of
        {ok, #{module:=Module}=BaseConfig} ->
            {ok, Module, maps:merge(BaseConfig, Config)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc This plugin manages actors except for "." domains
actor_is_managed(_SrvId, #actor_id{domain = <<$., _/binary>>}) ->
    false;

actor_is_managed(_SrvId, _Term) ->
    true.


%% @doc Called for out-of-server events (actor is not active)
%% from nkservice_actor_util:send_external_event/3
%% (If it was active, actor_srv_event/2 will be called)
actor_external_event(_SrvId, _Type, #actor{id=#actor_id{group=?GROUP_CORE, resource=?RES_CORE_EVENTS}}) ->
    continue;

actor_external_event(SrvId, created, Actor) ->
    case nkdomain_api:actor_to_external(SrvId, Actor) of
        {ok, ApiActor} ->
            Ev = #{
                reason => <<"ActorCreated">>,
                body => #{<<"actor">> => ApiActor}
            },
            EvActor = nkdomain_api_events:make_event(SrvId, Ev, Actor),
            _ = nkdomain_api_events:send_event(SrvId, EvActor);
        {error, Error} ->
            ?LLOG(warning, "could not generated event for ~p: ~p", [Actor, Error])
    end,
    ok;

actor_external_event(SrvId, deleted, #actor{}=Actor) ->
    Ev = #{reason=><<"ActorDeleted">>},
    EvActor = nkdomain_api_events:make_event(SrvId, Ev, Actor),
    _ = nkdomain_api_events:send_event(SrvId, EvActor),
    ok;

actor_external_event(_SrvId, _Event, _Actor) ->
    continue.


%% @doc
actor_srv_register(SrvId, ActorSt) ->
    nkdomain_actor:actor_register(SrvId, ActorSt).


%% @doc Called when an actor generates and event
%% Some of them are intercepted here and translated into API event
actor_srv_event(test_api, #actor_st{srv=SrvId, actor=Actor}=ActorSt) ->
    %% Special test event, accepted by any actor
    Ev = #{reason=><<"TestAPI">>},
    EvActor = nkdomain_api_events:make_event(SrvId, Ev, Actor),
    {ok, nkdomain_api_events:send_event_st(EvActor, ActorSt)};

% We received an 'nkdomain_api_event', from nkdomain_api_events, send to up-domains
actor_srv_event({nkdomain_api_event, Op, EvActor},
    #actor_st{actor=#actor{id=#actor_id{group=?GROUP_CORE, resource=?RES_CORE_DOMAINS}=ActorId}}) ->
    case ActorId of
        #actor_id{domain=?ROOT_DOMAIN, name=?ROOT_DOMAIN} ->
            ok;
        #actor_id{domain=Domain} ->
            case nkdomain_register:is_domain_active(Domain) of
                {true, Pid} ->
                    % We scale the api event to our father, so that possible watchers
                    % there can read it
                    Ev = {nkdomain_api_event, Op, EvActor},
                    nkservice_actor_srv:async_op(Pid, {send_event, Ev});
                false ->
                    lager:error("NKLOG EVENR SCALATE ERROR: ~s", [Domain])
            end
    end,
    continue;

%% Core (created, updated, deleted) events are processed here for all 'core' class actors
actor_srv_event(Event, ActorSt) when Event==created; element(1,Event)==updated; Event==deleted ->
    {ok, nkdomain_lib:core_api_event(Event, ActorSt)};

actor_srv_event(Event, ActorSt) ->
    nkservice_actor:actor_srv_event(Event, ActorSt).


%% @doc Called when an actor generates and event, for each linked process
%% from nkservice_actor_srv:do_event_link/2
actor_srv_link_event({nkdomain_core_v1_watch, Ref, Pid}, Data, {nkdomain_api_event, Op, EvActor}, _ActorSt) ->
    %% There is a core_v1 watcher for events of type 'api_event'
    #actor{data=EvData} = EvActor,
    #{<<"involvedObject">>:=#{<<"kind">>:=EvKind}} = EvData,
    case Data of
        #{filters:=#{kind:=Kind}} when Kind /= EvKind ->
            ok;
        _ ->
            Pid ! {nkdomain_core_v1_watch, Ref, Op, EvActor, Data}
    end,
    continue;

actor_srv_link_event(_Link, _Data, _Event, _ActorSt) ->
    continue.


%% ===================================================================
%% Actor DB
%% ===================================================================



%% @private
actor_db_get_query(SrvId, pgsql, SearchType, Opts) ->
    case nkdomain_queries_pgsql:get_query(SrvId, SearchType, Opts) of
        continue ->
            continue;
        Other ->
            Other
    end.



%% ===================================================================
%% REST Interface
%% ===================================================================

%% @doc


% /openapi
nkservice_rest_http(?DOMAIN_PKG_ID_API, <<"GET">>, [<<"openapi">>|Rest], Req) ->
    % lager:error("NKLOG OA ~p", [Rest]),
    nkservice_openapi:rest_http(Rest, Req);

nkservice_rest_http(?DOMAIN_PKG_ID_API, <<"GET">>, [<<"openapi.json">>], Req) ->
    nkservice_openapi:rest_http([<<"openapi.json">>], Req);

nkservice_rest_http(?DOMAIN_PKG_ID_API, Verb, Path, #{srv:=SrvId}=Req) ->
    Path2 = case lists:reverse(Path) of
        [<<>>|Rest] ->
            lists:reverse(Rest);
        _ ->
            Path
    end,
    nkdomain_api_http:rest_api(SrvId, Verb, Path2, Req);

nkservice_rest_http(?DOMAIN_PKG_ID_GRAPHIQL, Verb, Path, Req) ->
    nkservice_graphiql_server:http(Verb, Path, Req);

nkservice_rest_http(_Id, _Method, _Path, _Req) ->
    continue.



%% ===================================================================
%% Service Callbacks
%% ===================================================================


%% @doc We check periodically if the domain object is alive
service_master_leader(?ROOT_SRV, _IsLeader, _Pid, User) ->
    {ok, User};

service_master_leader(SrvId, true, _, User) ->
    #actor_id{}=ActorId = nkdomain_plugin:get_domain_cache(SrvId, actor_id),
    case nkservice_actor:is_enabled({SrvId, ActorId}) of
        {ok, true} ->
            ok;
        {ok, false} ->
            ?LLOG(notice, "service '~s' belongs to a disabled domain. Stopping.", [SrvId]),
            spawn_link(fun() -> nkservice:stop(SrvId) end);
        {error, Error} ->
            ?LLOG(warning, "service '~s' error ~p accessing domain. Stopping.", [SrvId, Error]),
            spawn_link(fun() -> nkservice:stop(SrvId) end)
    end,
    {ok, User};

service_master_leader(_SrvId, false, _Pid, User) ->
    {ok, User}.



%% ===================================================================
%% OpenAPI
%% ===================================================================


nkservice_openapi_get_spec(SrvId) ->
    nkdomain_openapi:make_spec(SrvId).




%% ===================================================================
%% GraphQL
%% ===================================================================


%% @private
nkservice_graphql_core_schema(SrvId, SchemaType, Schema) ->
    DomainSchema = nkdomain_graphql_schema:core_schema(SrvId, SchemaType),
    Schema2 = maps:merge(Schema, DomainSchema),
    {continue, [SrvId, SchemaType, Schema2]}.


%% @private
nkservice_graphql_get_uid(SrvId, UID) ->
    nkdomain_graphql_search:get_uid(SrvId, UID).


%% @private
nkservice_graphql_query(_SrvId, _Name, _Params, _Meta, _Ctx) ->
    continue.


%% @private
nkservice_graphql_mutation(_SrvId, _Name, _Params, _Meta, _Ctx) ->
    continue.


%% @private
nkservice_graphql_execute(SrvId, Field, {nkdomain, _}=Obj, _Meta, Args) ->
    nkdomain_graphql_execute:execute(SrvId, Field, Obj, Args);

nkservice_graphql_execute(_SrvId, _Field, _SchObj, _Meta, _Args) ->
    continue.

