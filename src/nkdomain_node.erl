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

%% This module tries to register our 'node' object

-module(nkdomain_node).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([register_startup_fun/1, make_objs/1]).
-export([do_check/0, nkroot_started/0, get_node_obj_name/0]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Types "++Txt, Args)).

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-type startup_fun() :: fun(() -> ok | {error, term()}).


%% ===================================================================
%% Public
%% ===================================================================

-spec register_startup_fun(startup_fun()) ->
    ok.

register_startup_fun(Fun) ->
    gen_server:call(?MODULE, {register_startup_fun, Fun}).


%% @doc
make_objs([]) ->
    ok;

make_objs([#{<<"path">>:=Path} = Obj|Rest]) ->
    Obj2 = maps:remove(<<"path">>, Obj),
    make_objs([Obj2#{ path => Path}|Rest]);

make_objs([#{path:=Path} = Obj|Rest]) ->
    case nkdomain_obj_make:create(Obj) of
        {error, {object_already_exists, _ObjId}} ->
            case nkdomain:update(Path, Obj) of
                {ok, _} ->
                    lager:info("Object ~s updated", [Path]);
                Other ->
                    lager:warning("Object ~s NOT updated: ~p", [Path, Other])
            end,
            make_objs(Rest);
        {error, Error} ->
            lager:warning("Object ~s NOT created: ~p", [Path, Error]),
            error;
        {ok, #obj_id_ext{obj_id=ObjId}, _} ->
            lager:notice("Object ~s created (~s)", [Path, ObjId]),
            make_objs(Rest)
    end.



do_check() ->
    ?MODULE ! do_check.

nkroot_started() ->
    gen_server:cast(?MODULE, nkroot_started).


% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state,{
    nkroot_pid :: pid(),
    node_path :: binary(),
    node_obj_pid :: pid(),
    startup_funs = [] :: [fun()],
    timer :: reference()
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init([]) ->
    State = #state{node_path=get_node_path()},
    {ok, State}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.


handle_call({register_startup_fun, Fun}, _From, #state{startup_funs=Funs}=State) ->
    {reply, ok, State#state{startup_funs=Funs++[Fun]}};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(nkroot_started, State) ->
    start_services(),
    State2 = call_startup_funs(State),
    do_check(),
    {noreply, State2};

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info(do_check, State) ->
    case check_nkroot(State) of
        {true, State2} ->
            case check_node_obj(State2) of
                {true, State3} ->
                    {noreply, delayed_check(long, State3)};
                false ->
                    {stop, could_not_load_node_obj, State}
            end;
        false ->
            {noreply, delayed_check(short, State)}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{nkroot_pid=Pid}=State) ->
    State2 = State#state{nkroot_pid=undefined},
    {noreply, delayed_check(short, State2)};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{node_obj_pid=Pid}=State) ->
    State2 = State#state{node_obj_pid=undefined},
    {noreply, delayed_check(short, State2)};

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

%% @doc
get_node_obj_name() ->
    re:replace(to_bin(node()), "@", "+", [{return, binary}]).

%% @doc
get_node_path() ->
    <<"/nodes/", (get_node_obj_name())/binary>>.


%% @private
check_nkroot(#state{nkroot_pid=Pid}=State) when is_pid(Pid) ->
    {true, State};

check_nkroot(State) ->
    case erlang:whereis(?NKROOT) of
        Pid when is_pid(Pid) ->
            monitor(process, Pid),
            check_nkroot(State#state{nkroot_pid=Pid});
        undefined ->
            false
    end.


%% @private
check_node_obj(#state{node_obj_pid=Pid}=State) when is_pid(Pid) ->
    {true, State};

check_node_obj(#state{node_path=Path}=State) ->
    case nkdomain_db:load(Path) of
        {error, object_not_found} ->
            case nkdomain_node_obj:create() of
                {ok, _NodeId, Pid} when is_pid(Pid) ->
                    true = (node() == node(Pid)),
                    monitor(process, Pid),
                    check_node_obj(State#state{node_obj_pid=Pid});
                {error, Error} ->
                    ?LLOG(error, "could not create Node object ~s: ~p", [Path, Error]),
                    false
            end;
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            true = (node() == node(Pid)),
            monitor(process, Pid),
            {true, State#state{node_obj_pid=Pid}};
        {error, Error} ->
            ?LLOG(error, "could not load Node object ~s: ~p", [Path, Error]),
            false
    end.


%% @private
start_services() ->
    case ?CALL_NKROOT(config, []) of
        #{start_services:=Services} ->
            spawn_link(
                fun() ->
                    lists:foreach(
                        fun(Srv) ->
                            {BinMod, Data} = case binary:split(Srv, <<":">>) of
                                [M] ->
                                    {M, <<>>};
                                [M, D] ->
                                    {M, D}
                            end,
                            Mod = binary_to_atom(BinMod, latin1),
                            Mod:start_services(Data)
                        end,
                        Services)
                end);
        _ ->
            ok
    end.


%% @private
call_startup_funs(#state{startup_funs=[]}=State) ->
    State;

call_startup_funs(#state{startup_funs=[Fun|Rest]}=State) ->
    ok = Fun(),
    call_startup_funs(State#state{startup_funs=Rest}).


%% @private
delayed_check(Type, #state{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    Time = case Type of
        short -> 1000;
        long -> 20000
    end,
    Ref = erlang:send_after(Time, self(), do_check),
    State#state{timer=Ref}.



%% @private
to_bin(Term) -> nklib_util:to_binary(Term).
