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

%% @doc NkDomain Domain Actor
%% @see also nkdomain_actor
%% The Domain actor is a very special actor, with a different behavior than any other
%% - It's srv it's not the service it represents, but its father
%% - When it is loaded, it will try to start the service it represents
%%   (or it won't start)
%% - If it fails
%%   - all actors started on that domain/service will unload
%%   - we will try to restart it, if we cannot, we stop
%% - On unload, we will stop the service
%% - Periodically, it will update the 'services' table
%%   if it detects that it is not the stored version, it will stop
%% - If disabled, will stop the service it represents
%% - Service master periodically checks the actor too
%%   (nkdomain_callbacks:service_master_leader/4)

-module(nkdomain_domain_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behavior(nkservice_actor).

-export([config/0, parse/3]).
-export([init/2, heartbeat/1, terminate/2, enabled/2, sync_op/3, async_op/2, handle_info/2, stop/2]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkservice/include/nkservice_actor_debug.hrl").

%-include_lib("nkpacket/include/nkpacket.hrl").

-define(POOL_MAX_CONNECTIONS, 10).      % Update openapi if changed
-define(POOL_TIMEOUT, 30*60*1000).      % Update openapi if changed


%% ===================================================================
%% Types
%% ===================================================================



% Stores all registered information by class and resource
-type class_types() :: #{
    nkservice_actor:group() => #{
        nkservice_actor:resource() => #{
            nkservice_actor:name() => nkservice_actor:uid()}}}.

% Stores counters for registered actors
% Local information will have srv_id '<<>>'
-type counters() :: #{
    nkservice_actor:group() => #{
        nkservice_actor:resource() => #{nkservice_actor:id()|<<>> => integer()}}}.


-record(run_state, {
    is_root :: boolean(),
    managed_domain :: binary(),
    register_ets :: ets:id(),
    actor_uids = #{} :: #{nkservice_actor:id() => boolean()},
    actor_childs = #{} :: #{nkservice:domain() => reference()},
    actor_group_types = #{} :: class_types(),
    counters = #{} :: counters()
}).



%% ===================================================================
%% Internal
%% ===================================================================





%% ===================================================================
%% Behavior callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        resource => ?RES_CORE_DOMAINS,
        camel => ?KIND_CORE_DOMAIN,
        permanent => true,
        short_names => [d],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        % Domain search is used also as 'all type' search
        filter_fields => [<<"group">>],
        sort_fields => [<<"group+resource">>]
    }.


%% @doc
parse(_SrvId, _Actor, _ApiReq) ->
    Syntax = #{
        <<"spec">> => #{
            <<"httpPool">> => #{
                <<"maxConnections">> => {integer, 1, 1000000},
                <<"timeout">> => {integer, 1, none}
            }
        }
    },
    {syntax, Syntax}.




%% ===================================================================
%% Actor callbacks
%% ===================================================================

%% @private
init(_Op, #actor_st{actor=#actor{id=ActorId}}=ActorSt) ->
    ManagedDomain = nkdomain_register:actor_id_to_managed_domain(ActorId),
    case global:register_name({nkdomain_domain, ManagedDomain}, self()) of
        yes ->
            ?ACTOR_LOG(notice, "registered global domain ~s", [ManagedDomain], ActorSt),
            set_http_pool(ActorSt),
            RunState = #run_state{
                is_root = ManagedDomain == ?ROOT_DOMAIN,
                managed_domain = ManagedDomain,
                register_ets = ets:new(nkdomain_actor, [])
            },
            {ok, ActorSt#actor_st{run_state=RunState}};
        no ->
            ?ACTOR_LOG(warning, "could not register global for domain ~s", [ManagedDomain]),
            {error, already_registered}
    end.


%% @private
heartbeat(ActorSt) ->
    {ok, ActorSt}.
%%    update_service_db(ActorSt).


%% @private
enabled(Enabled, ActorSt) ->
    case Enabled of
        false ->
            do_stop_all_actors(ActorSt);
        true ->
            ok
    end,
    continue.


%% @private
sync_op(nkdomain_get_data, _From, #actor_st{srv=SrvId, actor=Actor}=State) ->
    #actor{id=#actor_id{uid=UID}} = Actor,
    {reply, {ok, SrvId, UID}, State};

sync_op({nkdomain_register_actor, _ActorId}, _From, #actor_st{is_enabled=false}=State) ->
    #actor_st{run_state=#run_state{managed_domain=ManagedDomain}} = State,
    {reply, {error, {domain_is_disabled, ManagedDomain}}, State};

sync_op({nkdomain_register_actor, ActorId}, _From, State) ->
    #actor_st{run_state=#run_state{managed_domain=ManagedDomain}} = State,
    case ActorId of
        #actor_id{domain=ManagedDomain} ->
            do_register_actor(ActorId, State);
        #actor_id{domain=OtherDomain} ->
            {reply, {error, {domain_unknown, OtherDomain}}, State}
    end;

sync_op({nkdomain_find_actor, ActorId}, _From, State) ->
    #actor_st{run_state=#run_state{is_root=IsRoot}} = State,
    case ActorId of
        #actor_id{
            domain = ?ROOT_DOMAIN,
            group = ?GROUP_CORE,
            resource = ?RES_CORE_DOMAINS,
            name = ?ROOT_DOMAIN
        } when IsRoot ->
            #actor_st{actor=#actor{id=#actor_id{uid=UID, vsn=Vsn}}} = State,
            {reply, {ok, ActorId#actor_id{uid=UID, pid=self(), vsn=Vsn}}, State};
        _ ->
            case do_find_actor_id(ActorId, State) of
                {ok, ActorId2} ->
                    {reply, {ok, ActorId2}, State};
                actor_not_found ->
                    {reply, {error, actor_not_registered}, State}
            end
    end;

sync_op(nkdomain_get_actors, _From, #actor_st{run_state=RunState}=State) ->
    #run_state{register_ets = Ets} = RunState,
    List = [
        {Group, Type, Name, UID, Pid} ||
        {{uid, UID}, Group, Type, Name, Pid} <-  ets:tab2list(Ets)
    ],
    {reply, List, State};

sync_op(nkdomain_get_all_counters, _From, #actor_st{run_state=RunState}=State) ->
    #run_state{counters=Counters} = RunState,
    Data = lists:foldl(
        fun(Group, Acc) ->
            GroupCounters = maps:from_list(do_get_class_counters(Group, State)),
            Acc#{Group => GroupCounters}
        end,
        #{},
        maps:keys(Counters)),
    {reply, {ok, Data}, State};

sync_op({nkdomain_get_group_counters, Group}, _From, State) ->
    {reply, {ok, maps:from_list(do_get_class_counters(Group, State))}, State};

sync_op({nkdomain_get_resource_counter, Group, Type}, _From, State) ->
    {reply, {ok, do_get_type_counter(Group, Type, State)}, State};

sync_op(nkdomain_get_all_groups, _From, #actor_st{run_state=RunState}=State) ->
    #run_state{actor_group_types=GroupResources} = RunState,
    {reply, {ok, maps:keys(GroupResources)}, State};

sync_op({nkdomain_get_all_resources, Group}, _From, #actor_st{run_state=RunState}=State) ->
    #run_state{actor_group_types=GroupResources} = RunState,
    Types = maps:get(Group, GroupResources, #{}),
    {reply, {ok, maps:keys(Types)}, State};

sync_op(_Op, _From, _State) ->
    continue.


%% @private
async_op({nkdomain_updated_child_counter, ChildDomain, ChildPid, Group, Type, Counter}, State) ->
    State2 = do_updated_child_counter(ChildDomain, ChildPid, Group, Type, Counter, State),
    {noreply, State2};

async_op(_Op, _State) ->
    continue.


%% @private
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    case do_remove_actor(Pid, State) of
        {true, State2} ->
            {noreply, State2};
        false ->
            #actor_st{run_state=RunState} = State,
            #run_state{actor_childs = Childs} = RunState,
            case lists:keyfind(Ref, 2, maps:to_list(Childs)) of
                {ChildDomain, Ref} ->
                    Childs2 = maps:remove(ChildDomain, Childs),
                    RunState2 = RunState#run_state{actor_childs = Childs2},
                    State3 = do_remove_child(ChildDomain, State#actor_st{run_state=RunState2}),
                    {noreply, State3};
                _ ->
                    continue
            end
    end;

handle_info(_Msg, _State) ->
    continue.


%% @private
stop(_Reason, ActorSt) ->
    do_stop_all_actors(ActorSt),
    {ok, ActorSt}.


%% @private
terminate(_Reason, ActorSt) ->
    stop_http_pool(ActorSt),
    {ok, ActorSt}.





%% ===================================================================
%% Internal
%% ===================================================================


%%%% @private
%%update_service_db(#actor_st{srv=SrvId}=ActorSt) ->
%%    case nkdomain_app:get(serviceDbHeartbeat) of
%%        true ->
%%            MaxTime = nkdomain_app:get(serviceDbMaxHeartbeatTime, ?SERVICE_MAX_UPDATED),
%%            case nkservice_actor_db:check_service(SrvId, SrvId, <<"main">>, MaxTime) of
%%                ok ->
%%                    {continue, [ActorSt]};
%%                {alternate_service, Info} ->
%%                    ?ACTOR_LOG(warning, "domain is started at another node: ~p. Stopping.", [Info], ActorSt),
%%                    {error, {alternate_service, Info}, ActorSt};
%%                {error, Error} ->
%%                    ?ACTOR_LOG(warning, "domain problem in database update: ~p. Stopping.", [Error], ActorSt),
%%                    {error, {service_update_error, Error}, ActorSt}
%%            end;
%%        _ ->
%%            {continue, [ActorSt]}
%%    end.


%% @private
set_http_pool(#actor_st{actor=Actor}) ->
    #actor{id=#actor_id{domain=Domain}, data=Data} = Actor,
    Spec = maps:get(<<"spec">>, Data, #{}),
    Pool = maps:get(<<"httpPool">>, Spec, #{}),
    Max = maps:get(<<"maxConnections">>, Pool, ?POOL_MAX_CONNECTIONS),
    Timeout = maps:get(<<"timeout">>, Pool, ?POOL_TIMEOUT),
    ok = hackney_pool:start_pool(Domain, []),
    ok = hackney_pool:set_max_connections(Domain, Max),
    ok = hackney_pool:set_timeout(Domain, Timeout),
    Max = hackney_pool:max_connections(Domain),
    Timeout = hackney_pool:timeout(Domain),
    ?ACTOR_LOG(notice, "started Hackney Pool ~s (~p, ~p)", [Domain, Max, Timeout]),
    ok.


%% @private
stop_http_pool(#actor_st{actor=Actor}) ->
    #actor{id=#actor_id{domain=Domain}} = Actor,
    hackney_pool:stop_pool(Domain).



%% ===================================================================
%% Register & Counters
%% ===================================================================

do_register_actor(ActorId, State) ->
    #actor_id{group=Group, resource=Res, name=Name, uid=UID, pid=Pid} = ActorId,
    #actor_st{run_state=#run_state{register_ets=Ets}} = State,
    case do_find_actor_id(ActorId, State) of
        actor_not_found ->
            Ref = monitor(process, Pid),
            Objs = [
                {{name, Group, Res, Name}, ActorId},
                {{pid, Pid}, {name, Group, Res, Name}, UID, Ref}
            ],
            ets:insert(Ets, Objs),
            State2 = do_rm_actor_counters(ActorId, State),
            State3 = do_add_actor_counters(ActorId, State2),
            State4 = send_counter_to_parent(Group, Res, State3),
            %?ACTOR_LOG(debug, "Actor ~p registered", [ActorId], State4),
            {reply, ok, State4};
        {ok, _OldActorId} ->
            % ?ACTOR_LOG(error, "Actor is already registered: ~p (had ~p)", [ActorId, OldActorId]),
            {reply, {error, actor_already_registered}, State}
    end.


%% @private
do_find_actor_id(ActorId, State) ->
    #actor_st{run_state=#run_state{register_ets=Ets}} = State,
    #actor_id{group=Group, resource=Res, name=Name} = ActorId,
    case ets:lookup(Ets, {name, Group, Res, Name}) of
        [{_, ActorId2}] ->
            {ok, ActorId2};
        [] ->
            actor_not_found
    end.


%% @private
do_remove_actor(Pid, #actor_st{run_state=RunState}=State) ->
    #run_state{register_ets = Ets} = RunState,
    case ets:lookup(Ets, {pid, Pid}) of
        [{{pid, Pid}, {name, Group, Res, Name}, UID, Ref}] ->
            nklib_util:demonitor(Ref),
            ets:delete(Ets, {pid, Pid}),
            ets:delete(Ets, {name, Group, Res, Name}),
            ActorId = #actor_id{
                uid = UID,
                group = Group,
                resource = Res,
                name = Name
            },
            State2 = do_rm_actor_counters(ActorId, State),
            {true, send_counter_to_parent(Group, Res, State2)};
        [] ->
            false
    end.


%% @private
do_stop_all_actors(#actor_st{run_state=RunState}=State) ->
    #run_state{register_ets = Ets} = RunState,
    Stopped = ets:foldl(
        fun
            ({{pid, Pid}, {name, _Group, _Res, _Name}, _UID, _Ref}, Acc) ->
                nkservice_actor_srv:async_op(Pid, {raw_stop, father_stopped}),
                Acc+1;
            (_Term, Acc) ->
                Acc
        end,
        0,
        Ets),
    ?ACTOR_LOG(warning, "domain stopped ~p childs", [Stopped], State).


%% @private
do_add_actor_counters(ActorId, #actor_st{run_state=RunState}=State) ->
    #actor_id{uid=UID, group=Group, resource=Res, name=Name} = ActorId,
    #run_state{actor_uids=UIDs, actor_group_types=GroupResources, counters=Counters} = RunState,
    Resources1 = maps:get(Group, GroupResources, #{}),
    Names1 = maps:get(Res, Resources1, #{}),
    Names2 = Names1#{Name => UID},
    Resources2 = Resources1#{Res => Names2},
    GroupResources2 = GroupResources#{Group => Resources2},
    Counters2 = case maps:is_key(UID, UIDs) of
        false ->
            GroupCounters1 = maps:get(Group, Counters, #{}),
            ResCounters1 = maps:get(Res, GroupCounters1, #{}),
            OldCounter = maps:get(<<>>, ResCounters1, 0),
            % lager:error("NKLOG C1 ~p", [{Type, Counters, GroupCounters1, OldCounter+1}]),
            ResCounters2 = ResCounters1#{<<>> => OldCounter+1},
            GroupCounters2 = GroupCounters1#{Res => ResCounters2},
            Counters#{Group => GroupCounters2};
        true ->
            Counters
    end,
    RunState2 = RunState#run_state{
        actor_uids = UIDs#{UID => true},
        actor_group_types = GroupResources2,
        counters = Counters2
    },
    State#actor_st{run_state=RunState2}.


%% @private
do_rm_actor_counters(ActorId, #actor_st{run_state=RunState}=State) ->
    #actor_id{uid=UID, group=Group, resource=Res, name=Name} = ActorId,
    #run_state{actor_uids=UIDs, actor_group_types=GroupResources, counters=Counters} = RunState,
    Resources1 = maps:get(Group, GroupResources, #{}),
    Names1 = maps:get(Res, Resources1, #{}),
    Names2 = maps:remove(Name, Names1),
    Resources2 = case map_size(Names2) of
        0 ->
            maps:remove(Res, Resources1);
        _ ->
            Resources1#{Res => Names2}
    end,
    GroupResources2 = GroupResources#{Group => Resources2},
    Counters2 = case maps:is_key(UID, UIDs) of
        true ->
            GroupCounters1 = maps:get(Group, Counters),
            ResCounters1 = maps:get(Res, GroupCounters1),
            OldCounter = maps:get(<<>>, ResCounters1),
            ResCounters2 = ResCounters1#{<<>> => OldCounter-1},
            GroupCounters2 = GroupCounters1#{Res => ResCounters2},
            Counters#{Group => GroupCounters2};
        false ->
            Counters
    end,
    RunState2 = RunState#run_state{
        actor_uids = maps:remove(UID, UIDs),
        actor_group_types = GroupResources2,
        counters = Counters2
    },
    State#actor_st{run_state=RunState2}.


%% @private
do_remove_child(ChildDomain, #actor_st{run_state=RunState}=State) ->
    #run_state{counters=Counters} = RunState,
    Counters2 = lists:foldl(
        fun({Group, Resources}, Acc) ->
            Resources2 = lists:foldl(
                fun({Type, Entries}, Acc2) ->
                    Acc2#{Type=>maps:remove(ChildDomain, Entries)}
                end,
                Acc,
                maps:to_list(Resources)),
            Acc#{Group=>Resources2}
        end,
        #{},
        maps:to_list(Counters)),
    RunState2 = RunState#run_state{counters = Counters2},
    State#actor_st{run_state=RunState2}.


%% @private
do_updated_child_counter(ChildDomain, ChildPid, Group, Res, Counter, State) ->
    #actor_st{run_state=RunState} = State,
    #run_state{actor_childs = Childs} = RunState,
    State2 = case maps:is_key(ChildDomain, Childs) of
        true ->
            State;
        false ->
            Childs2 = Childs#{ChildDomain => monitor(process, ChildPid)},
            RunState2 = RunState#run_state{actor_childs = Childs2},
            State#actor_st{run_state = RunState2}
    end,
    do_child_counter(ChildDomain, Group, Res, Counter, State2).


%% @private
do_child_counter(ChildDomain, Group, Res, Counter, #actor_st{run_state=RunState}=State) ->
    #run_state{counters=Counters} = RunState,
    GroupCounters1 = maps:get(Group, Counters, #{}),
    ResCounters1 = maps:get(Res, GroupCounters1, #{}),
    ResCounters2 = ResCounters1#{ChildDomain => Counter},
    GroupCounters2 = GroupCounters1#{Res => ResCounters2},
    Counters2 = Counters#{Group => GroupCounters2},
    RunState2 = RunState#run_state{counters=Counters2},
    State2 = State#actor_st{run_state = RunState2},
    send_counter_to_parent(Group, Res, State2).



%% @private
send_counter_to_parent(_Group, _Res, #actor_st{run_state=#run_state{is_root=true}}=State) ->
    State;

send_counter_to_parent(Group, Res, State) ->
    #actor_st{actor=#actor{id=ActorId}, father_pid=FatherPid} = State,
    #actor_id{domain=Domain} = ActorId,
    Value = do_get_type_counter(Group, Res, State),
    ?ACTOR_DEBUG("sent to parent ~p: ~p, ~p, ~p", [FatherPid, Group, Res, Value], State),
    Op = {nkdomain_updated_child_counter, Domain, self(), Group, Res, Value},
    nkservice_actor_srv:async_op(FatherPid, Op),
    State.


%% @private
do_get_class_counters(Group, #actor_st{run_state=RunState}=State) ->
    #run_state{counters=Counters} = RunState,
    Resources = maps:get(Group, Counters, #{}),
    [{Type, do_get_type_counter(Group, Type, State)} || Type <- maps:keys(Resources)].


%% @private
do_get_type_counter(Group, Res, #actor_st{run_state=RunState}) ->
    #run_state{counters=Counters} = RunState,
    GroupCounters = maps:get(Group, Counters, #{}),
    ResCounters = maps:get(Res, GroupCounters, #{}),
    lists:foldl(fun(Counter, Acc) -> Acc+Counter end, 0, maps:values(ResCounters)).




%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).