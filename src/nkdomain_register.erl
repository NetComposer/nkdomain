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

%% @doc NkDomain Actor utilities
%% @see also nkservice_actor and nkdomain_domain_actor
%%
%% NkDOMAIN provides an alternative actor registrar that base NkSERVICE
%% - 'Domain' actors are registered globally, and their data (SrvId and UUID)
%%   is cached at all nodes
%% - Actors register with their domain, and also data is cached at the node asking for it



-module(nkdomain_register).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([is_domain_active/1, get_domain_data/1, get_domain_data/2, 
         register_actor/2, find_registered/1]).
-export([get_all_counters/1, get_group_counters/2, get_resource_counter/3,
         get_all_groups/1, get_all_resources/2, get_all_actors/1]).
-export([actor_id_to_managed_domain/1, managed_domain_to_actor_id/1]).


-include("nkdomain.hrl").
%%-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkservice/include/nkservice_actor_debug.hrl").



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
%% Gets the pid of a domain, if loaded
-spec is_domain_active(nkservice_actor:domain()) -> 
    {true, pid()} | false.

is_domain_active(Domain) ->
    ManagedDomain = to_bin(Domain),
    case global:whereis_name({nkdomain_domain, ManagedDomain}) of
        Pid when is_pid(Pid) ->
            {true, Pid};
        undefined ->
            false
    end.


%% @doc Gets SrvId and UID for an activated domain, and populates cache
-spec get_domain_data(nkservice_actor:domain()) ->
    {ok, nkservice:id(), nkservice_actor:uid()} | {error, term()}.

get_domain_data(Domain) ->
    ManagedDomain = to_bin(Domain),
    case nklib_proc:values({nkdomain_cache, domain_data, ManagedDomain}) of
        [{{DomSrvId, UID}, _Pid}] ->
            {ok, DomSrvId, UID};
        [] ->
            case is_domain_active(ManagedDomain) of
                {true, Pid} ->
                    do_get_domain_data(ManagedDomain, Pid);
                false ->
                    {error, {domain_not_active, ManagedDomain}}
            end
    end.


%% @doc
%% Gets SrvId and UID from cache if available, otherwise tries to start it
%% Caution: since it calls to domain's actor, do not call it from inside it!
%% Also populates the cache for the domain
-spec get_domain_data(nkservice:id(), nkservice_actor:domain()) ->
    {ok, nkservice:id(), nkservice_actor:uid()} | {error, term()}.

get_domain_data(SrvId, Domain) ->
    ManagedDomain = to_bin(Domain),
    case get_domain_data(ManagedDomain) of
        {ok, DomSrvId, UID} ->
            {ok, DomSrvId, UID};
        {error, {domain_not_active, _}} ->
            case start_domain(SrvId, ManagedDomain) of
                {ok, Pid} ->
                    do_get_domain_data(ManagedDomain, Pid);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_get_domain_data(Domain, Pid) when is_pid(Pid) ->
    case nkservice_actor_srv:sync_op(Pid, nkdomain_get_data) of
        {ok, DomSrvId, UID} ->
            nklib_proc:reg({nkdomain_cache, domain_data, Domain}, {DomSrvId, UID}, Pid),
            {ok, DomSrvId, UID};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Registers an actor with its domain actor
-spec register_actor(nkservice:id(), #actor_id{}) ->
    {ok, Enabled::boolean(), pid()} | {error, already_registered|term()}.

register_actor(SrvId, #actor_id{domain=Domain}=ActorId) ->
    case start_domain(SrvId, Domain) of
        {ok, Pid} ->
            case nkservice_actor_srv:sync_op(Pid, {nkdomain_register_actor, ActorId}) of
                ok ->
                    {ok, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Implements alternate registrar based on domain
-spec find_registered(nkservice_actor:id()) ->
    {true, #actor_id{}} | false.

find_registered(#actor_id{}=ActorId) ->
    #actor_id{domain=Domain} = ActorId,
    % Root domain will reply for itself
    case domain_sync_op(Domain, {nkdomain_find_actor, ActorId}) of
        {ok, ActorId2} ->
            {true, ActorId2};
        {error, {domain_not_active, _}} ->
            false;
        {error, actor_not_registered} ->
            false;
        {error, Error} ->
            ?ACTOR_LOG(warning, "error calling nkdomain_find_actor for ~s: ~p", [Error]),
            false
    end;

find_registered(Id) ->
    case nkservice_actor_util:is_actor_id(Id) of
        {true, #actor_id{}=ActorId} ->
            find_registered(ActorId);
        false ->
            false
    end.


%% @doc
get_all_counters(Domain) ->
    domain_sync_op(Domain, nkdomain_get_all_counters).


%% @doc
get_group_counters(Domain, Group) ->
    domain_sync_op(Domain, {nkdomain_get_group_counters, to_bin(Group)}).


%% @doc
get_resource_counter(Domain, Group, Type) ->
    domain_sync_op(Domain, {nkdomain_get_resource_counter, to_bin(Group), to_bin(Type)}).


%% @doc
get_all_groups(Domain) ->
    domain_sync_op(Domain, nkdomain_get_all_groups).


%% @doc
get_all_resources(Domain, Group) ->
    domain_sync_op(Domain, {nkdomain_get_all_resources, to_bin(Group)}).


%% @private (Using ETS)
get_all_actors(Domain) ->
    domain_sync_op(Domain, nkdomain_get_actors).



%% @private
managed_domain_to_actor_id(ManagedDomain) ->
    {Name, Domain} = case binary:split(to_bin(ManagedDomain), <<".">>) of
        [Name0, Domain0] ->
            {Name0, Domain0};
        [?ROOT_DOMAIN] ->
            {?ROOT_DOMAIN, ?ROOT_DOMAIN};
        [Name0] ->
            {Name0, ?ROOT_DOMAIN}
    end,
    #actor_id{
        domain = Domain,
        group = ?GROUP_CORE,
        resource = ?RES_CORE_DOMAINS,
        name = Name
    }.


%% @private
actor_id_to_managed_domain(ActorId) ->
    #actor_id{group=?GROUP_CORE, resource=?RES_CORE_DOMAINS, domain=Domain, name=Name} = ActorId,
    case {Name, Domain} of
        {?ROOT_DOMAIN, ?ROOT_DOMAIN} ->
            ?ROOT_DOMAIN;
        {_, ?ROOT_DOMAIN} ->
            Name;
        _ ->
            <<Name/binary, $., Domain/binary>>
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @doc
start_domain(SrvId, Domain) ->
    ManagedDomain = to_bin(Domain),
    case is_domain_active(ManagedDomain) of
        {true, Pid} ->
            {ok, Pid};
        false ->
            ActorId = managed_domain_to_actor_id(ManagedDomain),
            case nkservice_actor:activate({SrvId, ActorId}) of
                {ok, #actor_id{pid=Pid}, _} ->
                    {ok, Pid};
                {error, actor_not_found} ->
                    {error, {domain_unknown, ManagedDomain}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc
domain_sync_op(Domain, Msg) ->
    domain_sync_op(Domain, Msg, 5000).

%% @doc
domain_sync_op(Domain, Op, Timeout) ->
    case is_domain_active(Domain) of
        {true, DomPid} ->
            case nkservice_actor_srv:sync_op(DomPid, Op, Timeout) of
                {error, process_not_found} ->
                    {error, {domain_not_active, Domain}};
                Other ->
                    Other
            end;
        false ->
            {error, {domain_not_active, Domain}}
    end.



%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).