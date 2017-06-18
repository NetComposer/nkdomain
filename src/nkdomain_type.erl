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
%% - For each type, a counter of started objects of this type is kept for
%%   each domain and parent domain the object has
%% - A master is elected for each type
%% - When an object instance is registered, it is sent to the master, that keeps a master
%%   counter for each type, and sends it to every slave


-module(nkdomain_type).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([register/2, get_counter/3]).
-export([start_link/1, master_call/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Types "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


-define(THROTTLE, 5).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec register(module(), #obj_id_ext{}) ->
    {ok, pid()} | {error, term()}.

register(Module, #obj_id_ext{pid=ObjPid}=ObjIdExt) when is_pid(ObjPid) ->
    master_call(Module, {register, ObjIdExt}).


%% @doc
-spec get_counter(nkservice:id(), module(), nkdomain:domain()) ->
    {ok, integer()}.

get_counter(SrvId, Module, Domain) ->
    master_call(Module, {get_global_counters, SrvId, to_bin(Domain)}).





% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start_link(Module) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module], []).


-record(obj, {
    pid :: pid(),
    path :: nkdomain:domain()
}).


-record(state, {
    module :: module(),
    type :: nkdomain:type(),
    master :: pid(),
    pids = #{} :: #{pid() => {nkdomain:srv_id(), nkdomain:obj_id()}},
    counters = #{} :: #{{nkservice:id(), Domain::binary()} => integer()},
    objs = #{} :: #{{nkservice:id(), nkdomain:obj_id()} => #obj{}},
    dom_cache = #{} :: #{Domain::binary() => [Domains::binary()]}
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init([Module]) ->
    #{type:=Type} = Module:object_info(),
    ok = nkdist:register(master, nkdomain_types, Type, #{meta=>#{module=>Module}}),
    {ok, #state{module=Module, type=Type, master=self()}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call({get_counters, SrvId, Domain}, _From, #state{counters=Counters}=State) ->
    {reply, {ok, maps:get({SrvId, Domain}, Counters, 0)}, State};

handle_call(get_master, _From, #state{master=Master}=State) ->
    {reply, {ok, Master}, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({register, ObjIdExt}, _From, State) ->
    #obj_id_ext{srv_id=SrvId, obj_id=ObjId, path=Path, type=Type, pid=Pid} = ObjIdExt,
    #state{type=Type, objs=Objs, pids=Pids, counters=Counters} = State,
    case maps:is_key(Pid, Pids) of
        true ->
            {reply, {ok, self()}, State};
        false ->
            Obj = #obj{path=Path, pid=Pid},
            Objs2 = Objs#{{SrvId, ObjId} => Obj},
            Pids2 = Pids#{Pid => {SrvId, ObjId}},
            monitor(process, Pid),
            {DomainList, State2} = make_domains(Path, State),
            Counters2 = update_counters(SrvId, DomainList, 1, Counters, State2),
            State3 = State2#state{objs=Objs2, pids=Pids2, counters=Counters2},
            {reply, {ok, self()}, State3}
    end;

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info({nkdist, {master, Pid}}, State) when Pid==self() ->
    % We are the new master
    % The old master should have told everyone to register with us now
    {noreply, State#state{master=Pid}};

handle_info({nkdist, {master, Pid}}, #state{master=OldMaster, pids=Pids}=State) ->
    % There is a new master
    case OldMaster == self() of
        true ->
            % We were master, we must tell everyone to re-register
            % We keep the records
            lists:foreach(
                fun(ObjPid) -> nkdomain_obj:new_type_master(ObjPid) end,
                maps:keys(Pids)),
            {noreply, State#state{master=Pid}};
        false ->
            % We were slave
            {noreply, State#state{master=Pid}}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    #state{objs=Objs, pids=Pids, counters=Counters}=State,
    case maps:find(Pid, Pids) of
        {ok, {SrvId, ObjId}} ->
            Pids2 = maps:remove(Pid, Pids),
            Objs2 = maps:remove({SrvId, ObjId}, Objs),
            #obj{pid=Pid, path=Path} = maps:get({SrvId, ObjId}, Objs),
            {DomainList, State2} = make_domains(Path, State),
            Counters2 = update_counters(SrvId, DomainList, -1, Counters, State2),
            State3 = State2#state{objs=Objs2, pids=Pids2, counters=Counters2},
            {noreply, State3};
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

%%%% @private
%%master_cast(Module, Msg) ->
%%    case gen_server:call(Module, get_master) of
%%        {ok, Pid} ->
%%            gen_server:cast(Pid, Msg);
%%        _ ->
%%            {error, no_master}
%%    end.


%% @private
master_call(Module, Msg) ->
    case gen_server:call(Module, get_master) of
        {ok, Pid} ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, no_master}
    end.


%% @private
make_domains(Path, #state{type=Type, dom_cache=Cache}=State) ->
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
update_counters(_SrvId, [], _N, Counters, _State) ->
    Counters;

update_counters(SrvId, [Domain|Rest], N, Counters, State) ->
    Count = maps:get({SrvId, Domain}, Counters, 0) + N,
    send_event(SrvId, Domain, Count, State),
    Counters2 = case Count of
        0 -> maps:remove({SrvId, Domain}, Counters);
        _ -> Counters#{{SrvId, Domain} => Count}
    end,
    update_counters(SrvId, Rest, N, Counters2, State).


%% @private
send_event(SrvId, Domain, Count, #state{type=Type}) ->
    Event = #nkevent{
        srv_id = SrvId,
        class = ?DOMAIN_EVENT_CLASS,
        subclass = Type,
        type = counter_updated,
        obj_id = Domain,
        domain = Domain,
        body = #{counter => Count}
    },
    % lager:notice("Counter '~s' updated for ~s~s: ~p", [Type, SrvId, Domain, Count]),
    nkevent:send(Event).



%% @private
to_bin(K) when is_binary(K) -> K;
to_bin(K) -> nklib_util:to_binary(K).