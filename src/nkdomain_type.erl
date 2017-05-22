%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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
%%
%% @doc A process of this type is started at each node for each registered type

-module(nkdomain_type).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([register/2, get_counters/1, get_counters/2, get_global_counters/1, get_global_counters/2]).
-export([get_global_nodes/1, get_global_nodes/2, get_objs/1]).
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Types "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec register(module(), #obj_id_ext{}) ->
    ok | {error, term()}.

register(Module, #obj_id_ext{pid=ObjPid}=ObjIdExt) when is_pid(ObjPid) ->
    master_cast(Module, {register, ObjIdExt}).


%% @doc
-spec get_counters(module()) ->
    {ok, #{Domain::binary() => integer()}}.

get_counters(Module) ->
    gen_server:call(Module, get_counters).


%% @doc
-spec get_counters(module(), nkdomain:domain()) ->
    {ok, integer()}.

get_counters(Module, Domain) ->
    gen_server:call(Module, {get_counters, to_bin(Domain)}).


%% @doc
-spec get_global_counters(module()) ->
    {ok, #{Domain::binary() => integer()}}.

get_global_counters(Module) ->
    master_call(Module, get_global_counters).


%% @doc
-spec get_global_counters(module(), nkdomain:domain()) ->
    {ok, integer()}.

get_global_counters(Module, Domain) ->
    master_call(Module, {get_global_counters, to_bin(Domain)}).


%% @doc
-spec get_global_nodes(module()) ->
    {ok, #{Domain::binary() => #{node() => integer()}}}.

get_global_nodes(Module) ->
    master_call(Module, get_global_nodes).


%% @doc
-spec get_global_nodes(module(), nkdomain:domain()) ->
    {ok, #{node() => integer()}}.

get_global_nodes(Module, Domain) ->
    master_call(Module, {get_global_nodes, to_bin(Domain)}).


%% @doc
-spec get_objs(module()) ->
    {ok, map()}.

get_objs(Module) ->
    gen_server:call(Module, get_objs).


% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start_link(Module) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module], []).

-record(obj, {
    pid :: pid(),
    srv_id :: nkservice:id(),
    path :: binary(),
    domains :: [binary()]
}).


-record(state, {
    module :: module(),
    type :: nkdomain:type(),
    master :: pid(),
    objs = #{} :: #{nkdomain:obj_id() => #obj{}},
    pids = #{} :: #{pid() => nkdomain:obj_id()},
    counters = #{} :: #{Domain::binary() => integer()},
    master_counters = #{} :: #{Domain::binary() => #{node() => integer()}},
    dom_cache = #{} :: #{Domain::binary() => [Domains::binary()]}
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init([Module]) ->
    #{type:=Type} = Module:object_get_info(),
    ok = nkdist:register(master, nkdomain_types, Type, #{meta=>#{module=>Module}}),
    {ok, #state{module=Module, type=Type, master=self()}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(get_counters, _From, #state{counters=Counters}=State) ->
    {reply, {ok, Counters}, State};

handle_call({get_counters, Domain}, _From, #state{counters=Counters}=State) ->
    {reply, {ok, maps:get(Domain, Counters, 0)}, State};

handle_call(get_global_counters, _From, #state{master_counters=Counters}=State) ->
    Domains = maps:keys(Counters),
    List = [{Domain, do_get_global_counters(Domain, State)} || Domain <- Domains],
    {reply, {ok, maps:from_list(List)}, State};

handle_call({get_global_counters, Domain}, _From, State) ->
    {reply, {ok, do_get_global_counters(Domain, State)}, State};

handle_call(get_global_nodes, _From, #state{master_counters=Counters}=State) ->
    {reply, {ok, Counters}, State};

handle_call({get_global_nodes, Domain}, _From, #state{master_counters=Counters}=State) ->
    {reply, {ok, maps:get(Domain, Counters, #{})}, State};

handle_call(get_master, _From, #state{master=Master}=State) ->
    {reply, {ok, Master}, State};

handle_call(get_objs, _From, #state{objs=Objs}=State) ->
    {reply, {ok, Objs}, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast({register, ObjIdExt}, State) ->
    #obj_id_ext{srv_id=SrvId, obj_id=ObjId, path=Path, type=Type, pid=Pid} = ObjIdExt,
    #state{type=Type, objs=Objs, pids=Pids, counters=Counters} = State,
    {DomainList, State2} = make_domains(Type, Path, State),
    monitor(process, Pid),
    Obj = #obj{path=Path, srv_id=SrvId, pid=Pid, domains=DomainList},
    Objs2 = Objs#{ObjId => Obj},
    Pids2 = Pids#{Pid => ObjId},
    Counters2 = update_counters(DomainList, 1, Counters, State),
    State3 = State2#state{objs=Objs2, pids=Pids2, counters=Counters2},
    {noreply, State3};

handle_cast({counter_updated, Node, Domain, Count}, #state{master_counters=Counters}=State) ->
    Counters2 = update_master_counter(Node, Domain, Count, Counters),
    State2 = State#state{master_counters=Counters2},
    send_event(Domain, State2),
    {noreply, State2};

handle_cast({all_counters_updated, Node, NodeCounters}, #state{master_counters=Counters}=State) ->
    Counters2 = update_master_all_counters(Node, NodeCounters, Counters),
    State2 = State#state{master_counters=Counters2},
    lists:foreach(fun(Domain) -> send_event(Domain, State2) end, maps:keys(Counters2)),
    {noreply, State2};

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info({nkdist, {master, Pid}}, State) when Pid==self() ->
    #state{counters=NodeCounters} = State,
    Counters = update_master_all_counters(node(), NodeCounters, #{}),
    {noreply, State#state{master=Pid, master_counters=Counters}};

handle_info({nkdist, {master, Pid}}, #state{counters=Counters}=State) ->
    gen_server:cast(Pid, {all_counters_updated, node(), Counters}),
    {noreply, State#state{master=Pid, master_counters=#{}}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    #state{objs=Objs, pids=Pids, counters=Counters}=State,
    case maps:find(Pid, Pids) of
        {ok, ObjId} ->
            #obj{pid=Pid, domains=DomainList} = maps:get(ObjId, Objs),
            Pids2 = maps:remove(Pid, Pids),
            Objs2 = maps:remove(ObjId, Objs),
            Counters2 = update_counters(DomainList, -1, Counters, State),
            State2 = State#state{objs=Objs2, pids=Pids2, counters=Counters2},
            {noreply, State2};
        error ->
            lager:warning("Module ~p received unexpected DOWN: ~p", [?MODULE, Pid]),
            {noreply, State}
    end;

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



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
master_cast(Module, Msg) ->
    case gen_server:call(Module, get_master) of
        {ok, Pid} ->
            gen_server:cast(Pid, Msg);
        _ ->
            {error, no_master}
    end.


%% @private
master_call(Module, Msg) ->
    case gen_server:call(Module, get_master) of
        {ok, Pid} ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, no_master}
    end.


%% @private
make_domains(Type, Path, #state{dom_cache=Cache}=State) ->
    {ok, Domain, _ObjName} = nkdomain_util:get_parts(Type, Path),
    case maps:find(Domain, Cache) of
        {ok, DomList} ->
            {DomList, State};
        error ->
            DomainList = case Domain of
                <<"/">> ->
                    [<<"/">>];
                _ ->
                    Terms = binary:split(Domain, <<"/">>, [global]),
                    do_make_domains(Terms, [], [])
            end,
            Cache2 = Cache#{Domain => DomainList},
            {DomainList, State#state{dom_cache=Cache2}}
    end.


%% @private
do_make_domains([], _Acc1, Acc2) ->
    Acc2;

do_make_domains([Pos|Rest], Acc1, Acc2) ->
    Acc1B = [Pos|Acc1],
    Domain = case nklib_util:bjoin(lists:reverse([Pos|Acc1]), <<"/">>) of
        <<>> -> <<"/">>;
        Domain0 -> Domain0
    end,
    Acc2B = [Domain | Acc2],
    do_make_domains(Rest, Acc1B, Acc2B).


%% @private
update_counters([], _N, Counters, _State) ->
    Counters;

update_counters([Domain|Rest], N, Counters, #state{master=Master}=State) ->
    Count = maps:get(Domain, Counters, 0) + N,
    Counters2 = case Count of
        0 -> maps:remove(Domain, Counters);
        _ -> Counters#{Domain => Count}
    end,
    gen_server:cast(Master, {counter_updated, node(), Domain, Count}),
    update_counters(Rest, N, Counters2, State).


%% @private
update_master_counter(Node, Domain, Count, Counters) ->
    Nodes1 = maps:get(Domain, Counters, #{}),
    Nodes2 = Nodes1#{Node => Count},
    Counters#{Domain => Nodes2}.


%% @private
update_master_all_counters(Node, NodeCounters, Counters) ->
    lists:foldl(
        fun({Domain, Count}, Acc) -> update_master_counter(Node, Domain, Count, Acc) end,
        Counters,
        maps:to_list(NodeCounters)).


%% @private
do_get_global_counters(Domain, #state{master_counters=Counters}) ->
    Nodes = maps:get(Domain, Counters, #{}),
    lists:foldl(
        fun({_Node, SubCount}, Acc) -> Acc + SubCount end,
        0,
        maps:to_list(Nodes)).


%% @private
send_event(Domain, #state{type=Type}=State) ->
    Count = do_get_global_counters(Domain, State),
    Event = #nkevent{
        srv_id = root,
        class = ?DOMAIN_EVENT_CLASS,
        subclass = Type,
        type = counter_updated,
        obj_id = Domain,
        domain = Domain,
        body = #{counter => Count}
    },
    % lager:error("NKLOG SEND EVENT ~p", [lager:pr(Event, ?MODULE)]),
    nkevent:send(Event).


%% @private
to_bin(K) when is_binary(K) -> K;
to_bin(K) -> nklib_util:to_binary(K).