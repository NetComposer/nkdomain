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

%% @doc NkDomain core class API processing
%% https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/
%% https://github.com/kubernetes/community/blob/master/contributors/devel/api-conventions.md

-module(nkdomain_api_core).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/2]).
-export([default_request/5, list/4, get/4, pre_create/4, create/5, pre_update/4, update/5]).
-export([get_watches/0, set_activate_opts/2]).

-include("nkdomain.hrl").
-include("nkdomain_api.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-define(DELETE_COLLECTION_SIZE, 10000).

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Incoming API request called from nkdomain_api in callbacks
%% SrvId is the service receiving the request
%% SrvId must be able to understand the resource, so it must have the
%% corresponding plugins applied. Same for response codes.
%% 'verb', 'group' and 'vsn' are known to be available
%% 'resource', 'domain', 'name' etc. are not
-spec request(nkservice:id(), nkdomain_api:request()) ->
    nkdomain_api:response().

request(SrvId, #{resource:=<<"_uids">>, group:=Group, vsn:=Vsn, name:=UID}=ApiReq) ->
    case UID of
        <<>> ->
            {error, actor_not_found};
        _ ->
            ?API_DEBUG("reading UID: ~s", [UID]),
            case nkservice_actor:find({SrvId, UID}) of
                {ok, #actor_id{domain=Domain, group=Group2, vsn=Vsn2, resource=Res, name=Name}, _} ->
                    case nkdomain_register:get_domain_data(SrvId, Domain) of
                        {ok, DomSrvId, _DomUID} ->
                            Vsn3 = case Group==Group2 of
                                true ->
                                    Vsn;
                                false ->
                                    Vsn2
                            end,
                            ApiReq2 = ApiReq#{
                                domain => Domain,
                                group := Group2,
                                vsn := Vsn3,
                                resource:=Res,
                                name := Name
                            },
                            ?API_DEBUG("UID resolved", [], ApiReq2),
                            request(DomSrvId, ApiReq2);
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end;

request(SrvId, #{verb:=Verb, group:=Group, vsn:=Vsn}=ApiReq) ->
    try
        % Group and Vsn are taken from api request or body if not there
        % Now complete resource, domain and name (optional) from body if not in ApiReq
        ApiReq2 = case nkdomain_api_core_lib:get_res_domain_name(SrvId, ApiReq) of
            {ok, ExpandedReq} ->
                ExpandedReq;
            {error, CheckError} ->
                throw({error, CheckError})
        end,
        #{domain:=Domain, resource:=Resource} = ApiReq2,
        ?API_DEBUG("incoming '~p' ~s (~p)", [Verb, Resource, ApiReq2]),
        ActorSrvId = case nkdomain_register:get_domain_data(SrvId, Domain) of
            {ok, DomSrvId, _DomUID} ->
                DomSrvId;
            {error, {domain_is_disabled, DisabledDomain}} ->
                ?API_LLOG(notice, "working with disabled domain '~s'", [DisabledDomain]),
                SrvId;
            {error, DomError} ->
                throw({error, DomError})
        end,
        Config = case catch nkdomain_actor_util:get_config(ActorSrvId, Group, Resource) of
            {ok, Config0} ->
                Config0;
            {error, ConfigError} ->
                throw({error, ConfigError})
        end,
        #{verbs:=Verbs, versions:=Versions, module:=Module} = Config,
        ActorId = #actor_id{
            domain = Domain,
            group = Group,
            vsn = Vsn,
            resource = Resource,
            name = maps:get(name, ApiReq2, undefined)
        },
        case lists:member(Verb, Verbs) of
            true ->
                ok;
            false ->
                throw({error, verb_not_allowed})
        end,
        case lists:member(Vsn, Versions) of
            true ->
                ok;
            false ->
                throw({error, {api_incompatible, <<Group/binary, $/, Vsn/binary>>}})
        end,
        % Parses the 'params' key in ApiReq
        ApiReq3 = case nkdomain_api_core_lib:parse_params(Verb, ApiReq2) of
            {ok, ParsedApiReq} ->
                ParsedApiReq;
            {error, ParseError} ->
                throw({error, ParseError})
        end,
        % At this point, the Verb, API Group and version are compatible with the
        % Actor configuration
        #{verb:=Verb} = ApiReq3,
        case nkservice_actor:request(Module, ActorSrvId, ActorId, ApiReq3) of
            continue ->
                ?API_DEBUG("processing default", [], ApiReq3),
                default_request(Verb, ActorSrvId, ActorId, Config, ApiReq3);
            {continue, #{resource:=Resource2}=ApiReq4} when Resource2 /= Resource ->
                ?API_DEBUG("updated request", [], ApiReq4),
                request(ActorSrvId, ApiReq4);
            Other ->
                ?API_DEBUG("specific processing", [], ApiReq3),
                Other
        end
    catch
        throw:Throw ->
            Throw
    end.


%% @private
%% A specific actor resource has been identified in Config
%% Maybe it is a final actor or a resource
default_request(get, SrvId, #actor_id{name=Name}=ActorId, Config, #{subresource:=[]}=ApiReq)
        when is_binary(Name) ->
    get(SrvId, ActorId, Config, ApiReq);

default_request(list, SrvId, #actor_id{name=undefined}=ActorId, Config, #{subresource:=[]}=ApiReq) ->
    list(SrvId, ActorId, Config, ApiReq);

default_request(create, SrvId, ActorId, Config, #{subresource:=[]}=ApiReq) ->
    Actor2 = pre_create(SrvId, ActorId, Config, ApiReq),
    create(SrvId, ActorId, Config, Actor2, ApiReq);

default_request(update, SrvId, #actor_id{name=Name}=ActorId, Config, #{subresource:=[]}=ApiReq)
        when is_binary(Name) ->
    Actor2 = pre_update(SrvId, ActorId, Config, ApiReq),
    update(SrvId, ActorId, Config, Actor2, ApiReq);

default_request(delete, SrvId, #actor_id{name=Name}=ActorId, Config, #{subresource:=[]}=ApiReq)
    when is_binary(Name) ->
    delete(SrvId, ActorId, Config, ApiReq);

default_request(deletecollection, SrvId, #actor_id{name=undefined}=ActorId, Config, #{subresource:=[]}=ApiReq) ->
    delete_collection(SrvId, ActorId, Config, ApiReq);

default_request(watch, SrvId, ActorId, Config, #{subresource:=[]}=ApiReq) ->
    watch(SrvId, ActorId, Config, ApiReq);

default_request(_Verb, _SrvId, ActorId, _Config, _ApiReq) ->
    ?API_LLOG(warning, "Invalid resource.1 (~p)", [{_Verb, _SrvId, ActorId, _Config, _ApiReq}]),
    {error, resource_invalid}.



%% ===================================================================
%% Default Operations
%% ===================================================================


%% @doc Standard get
get(SrvId, ActorId, Config, ApiReq) ->
    #actor_id{
        domain = Domain,
        group = Group,
        resource = Res,
        name = Name,
        vsn = Vsn
    } = ActorId,
    Opts = set_activate_opts(Config, ApiReq),
    ?API_DEBUG("processing read ~p (~p)", [ActorId, Opts]),
    case nkservice_actor_db:read({SrvId, ActorId}, Opts) of
        {ok, #actor{id=ActorId2}=Actor, _Meta} ->
            #actor_id{domain=Domain, group=Group, resource=Res, name=Name} = ActorId2,
            case nkdomain_api:actor_to_external(SrvId, Actor, Vsn) of
                {ok, ApiActor} ->
                    {ok, ApiActor};
                {error, ApiActorError} ->
                    {error, ApiActorError}
            end;
        {error, ReadError} ->
            {error, ReadError}
    end.


%% @doc
list(SrvId, ActorId, Config, ApiReq) ->
    #actor_id{vsn=Vsn} = ActorId,
    #{camel:=ListKind} = Config,
    #{params:=Params} = ApiReq,
    case nkdomain_core_search:type_search(SrvId, ActorId, Config, Params) of
        {ok, ActorList, Meta} ->
            nkdomain_api_lib:make_actors_list(SrvId, Vsn, ListKind, ActorList, Meta);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
pre_create(SrvId, ActorId, Config, ApiReq) ->
    % We can create new objects only if it service is active
    % We are activating the domain this actor is living on
    % For example, for user1@a, we are activating "a" (a.nkdomain-root)
    % ActorSrvId would be 'a.nkdomain-root' at it is guaranteed to be running
    % If creating domain c.b.a, we are activating "b.a" (b.a.nkdomain-root)
    % ActorSrvId would be 'b.a.nkdomain-root'
    % In this case, the moment it starts, it will start srv c.b.a.nkdomain-root.

    % Check fields in body (group, vsn, domain, name) are not different and expand links
    Obj = case nkdomain_api_core_lib:check_request_obj(SrvId, ActorId, ApiReq) of
        {ok, CheckedObj} ->
            CheckedObj;
        {error, ObjError} ->
            throw({error, ObjError})
    end,
    Actor1 = case nkdomain_api:api_object_to_actor(SrvId, ActorId, Obj) of
        {ok, ApiActor} ->
            ApiActor;
        {error, ApiActorError} ->
            throw({error, ApiActorError})
    end,
    Actor2 = nkservice_actor_util:put_create_fields(Actor1),
    % Make sure it is a valid actor, parse data
    case nkdomain_actor:parse(SrvId, Actor2, Config, ApiReq) of
        {ok, Actor3} ->
            Actor3;
        {error, ParseError} ->
            throw({error, ParseError})
    end.


%% @doc
create(SrvId, ActorId, Config, Actor, ApiReq) ->
    Opts = set_activate_opts(Config, ApiReq),
    % Actor will not have uid, so it will be generated
    ?API_DEBUG("Creating actor ~p (~p)", [Actor, Opts]),
    #actor_id{vsn=Vsn} = ActorId,
    Actor2 = case nkservice_actor_db:create(SrvId, Actor, Opts) of
        {ok, ActorCreated, _Meta} ->
            ActorCreated;
        {error, CreateError} ->
            throw({error, CreateError})
    end,
    case nkdomain_api:actor_to_external(SrvId, Actor2, Vsn) of
        {ok, Actor3} ->
            {created, Actor3};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
%% See create for detailed explanation
pre_update(SrvId, ActorId, Config, ApiReq) ->
    % Checks that fields group, vsn, domain, name are not different in obj
    Obj = case nkdomain_api_core_lib:check_request_obj(SrvId, ActorId, ApiReq) of
        {ok, CheckedObj} ->
            CheckedObj;
        {error, ObjError} ->
            throw({error, ObjError})
    end,
    Actor1 = case nkdomain_api:api_object_to_actor(SrvId, ActorId, Obj) of
        {ok, ApiActor} ->
            ApiActor;
        {error, ApiActorError} ->
            throw({error, ApiActorError})
    end,
    case nkdomain_actor:parse(SrvId, Actor1, Config, ApiReq) of
        {ok, Actor2} ->
            Actor2;
        {error, ParseError} ->
            throw({error, ParseError})
    end.


%% @doc
update(SrvId, ActorId, Config, Actor, ApiReq) ->
    Opts1 = set_activate_opts(Config, ApiReq),
    %% Allow to update only spec, and leave status
    Opts2 = Opts1#{update_opts=>#{data_fields=>[<<"spec">>]}},
    ?API_DEBUG("Updating actor ~p", [Actor]),
    #actor_id{vsn=Vsn} = ActorId,
    case nkservice_actor_db:update(SrvId, Actor, Opts2) of
        {ok, Actor2, _} ->
            case nkdomain_api:actor_to_external(SrvId, Actor2, Vsn) of
                {ok, ApiActor2} ->
                    {ok, ApiActor2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, actor_not_found} ->
            default_request(create, SrvId, ActorId, Config, ApiReq);
        {error, {field_missing, <<"name">>}} ->
            default_request(create, SrvId, ActorId, Config, ApiReq);
        {error, UpdateError} ->
            {error, UpdateError}
    end.


%% @doc
delete(SrvId, ActorId, _Config, ApiReq) ->
    #{params:=Params} = ApiReq,
    Opts = #{cascade => maps:get(cascade, Params, false)},
    ?API_DEBUG("processing delete ~p (~p)", [ActorId, Opts]),
    case nkservice_actor_db:delete({SrvId, ActorId}, Opts) of
        {ok, _ActorIds, _Meta} ->
            {status, actor_deleted};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
delete_collection(SrvId, ActorId, Config, ApiReq) ->
    #{params:=Params} = ApiReq,
    Params2 = maps:merge(#{size=>?DELETE_COLLECTION_SIZE}, Params),
    case nkdomain_core_search:search_params_id(SrvId, ActorId, Config, Params2) of
        {ok, ActorIds, _Meta} ->
            do_delete(SrvId, ActorIds, 0);
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_delete(SrvId, ActorIds, Num) ->
    case length(ActorIds) > 100 of
        true ->
            {Set1, Set2} = lists:split(100, ActorIds),
            case nkdomain_core_search:do_delete(SrvId, Set1) of
                {status, {actors_deleted, Num2}} ->
                    do_delete(SrvId, Set2, Num+Num2);
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            case nkdomain_core_search:do_delete(SrvId, ActorIds) of
                {status, {actors_deleted, Num2}} ->
                    {status, {actors_deleted, Num+Num2}};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
watch(SrvId, #actor_id{name=undefined, domain=Domain}=ActorId, Config, ApiReq) ->
    % We are watching on a domain
    {Name2, Domain2} = case binary:split(Domain, <<".">>) of
        [DomName, DomRest] ->
            {DomName, DomRest};
        [DomName] ->
            {DomName, ?ROOT_DOMAIN}
    end,
    ActorId2 = ActorId#actor_id{
        domain = Domain2,
        group = ?GROUP_CORE,
        resource = ?RES_CORE_DOMAINS,
        name = Name2
    },
    #{camel:=DomKind} = Config,
    % The 'kind' parameter will be added to the link as a filter,
    % so that only events reaching the domain actor of this Kind will be
    % sent to the watcher
    % @see actor_srv_link_event in nkdomain_callbacks
    #{domain:=Domain, params:=Params} = ApiReq,
    Params2 = Params#{kind=>DomKind},
    nkdomain_api_watch:start(SrvId, ActorId2, Config, ApiReq#{params:=Params2});

watch(SrvId, ActorId, Config, ApiReq) ->
    % We are watching on a regular object
    nkdomain_api_watch:start(SrvId, ActorId, Config, ApiReq).


%% @private
set_activate_opts(#{activable:=false}, _ApiReq) ->
    #{activate=>false};

set_activate_opts(_Config, #{params:=Params}) ->
    Activate = maps:get(activate, Params, true),
    Opts1 = case Activate of
        true ->
            #{activate=>true, consume=>maps:get(consume, Params, false)};
        false ->
            #{activate=>false}
    end,
    Opts2 = case maps:find(ttl, Params) of
        {ok, TTL} ->
            Opts1#{actor_config=>#{ttl=>TTL}};
        error ->
            Opts1
    end,
    Opts2.


%% @private
get_watches() ->
    nklib_proc:values(core_v1_watches).


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).
