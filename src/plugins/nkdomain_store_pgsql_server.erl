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

-module(nkdomain_store_pgsql_server).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_pool/1]).
-export([start_link/4]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


-define(CHECK_TIME, 5000).

-define(LLOG(Type, Txt, Args, State),
    lager:Type("NkDOMAIN Store PGSQL Server (~p) "++Txt, [State#state.srv_id|Args])).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


get_pool(SrvId) ->
    Name = SrvId:config_nkdomain_store_pgsql(),
    gen_server:call(Name, get_pool).



%% @doc
-spec start_link(nkservice:id(), atom(), binary(), list()) ->
    {ok, pid()} | {error, term()}.

start_link(SrvId, Name, Db, Conns) ->
    gen_server:start_link({local, Name}, ?MODULE, [SrvId, Name, Db, Conns], []).


%% Proc lib start:
%% proc_lib:start_link(?MODULE, do_init, [Arg1, Arg2])



% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    srv_id :: nkservice:id(),
    name :: atom(),
    db :: binary(),
    pool_pids = [] :: [{pid(), Ip::binary()}],
    pool_conns :: [list()]
}).



%% @private
-spec init(term()) ->
    {ok, tuple()} | {ok, tuple(), timeout()|hibernate} |
    {stop, term()} | ignore.

init([SrvId, Name, Db, Conns]) ->
    State = #state{
        srv_id = SrvId,
        name = Name,
        db = Db,
        pool_pids = [],
        pool_conns = Conns
    },
    process_flag(trap_exit, true),
    % self() ! start_pools,
    {ok, State}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(get_pool, _From, #state{pool_pids=Pids}=State) ->
    case Pids of
        [{Pid, _Ip}|_] ->
            {reply, {ok, Pid}, State};
        [] ->
            {reply, {error, no_pool_available}, State}
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

handle_info(start_pools, #state{pool_conns=Conns}=State) ->
    State2 = start_pools(Conns, State),
    erlang:send_after(?CHECK_TIME, self(), start_pools),
    {noreply, State2};

handle_info({'EXIT', Pid, _Reason}, #state{pool_pids=Pids}=State) ->
    case lists:keytake(Pid, 1, Pids) of
        {value, {Pid, Ip}, Pids2} ->
            ?LLOG(warning, "pool at ~s down", [Ip], State),
            {noreply, State#state{pool_pids=Pids2}};
        false ->
            lager:warning("Module ~p received unexpected down: ~p", [?MODULE, Pid]),
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



% ===================================================================
%% Internal
%% ===================================================================

%% @private
start_pools([], State) ->
    State;

start_pools([{[{undefined, tcp, Ip, Port}|_], Opts}|Rest], State) ->
    #state{name=Name, db=Db, pool_pids=PoolPids} = State,
    Ip2 = nklib_util:to_host(Ip),
    case lists:keymember(Ip2, 2, PoolPids) of
        true ->
            start_pools(Rest, State);
        false ->
            ConnOpts = [
                {database, Db},
                {host, nklib_util:to_host(Ip)},
                {port, Port},
                {user, maps:get(user, Opts, <<"root">>)},
                {password, maps:get(password, Opts, <<"">>)},
                %{connect_timeout, ConnectTimeout},
                {as_binary, true}
            ],
            Name1 = list_to_binary([nklib_util:to_binary(Name), "-", nklib_util:to_host(Ip)]),
            Name2 = binary_to_atom(Name1, utf8),
            PoolOpts = [
                {name, {local, Name2}},
                {worker_module, nkdomain_store_pgsql_worker},
                {size, 5},
                {max_overflow, 10}
            ],
            try poolboy:start_link(PoolOpts, ConnOpts) of
                {ok, Pid} ->
                    ?LLOG(notice, "connected to pool to ~s", [Ip2], State),
                    PoolPids2 = case Ip of
                        {127, _, _, _} ->
                            [{Pid, Ip2}|PoolPids];
                        _ ->
                            PoolPids ++ [{Pid, Ip2}]
                    end,
                    start_pools(Rest, State#state{pool_pids=PoolPids2});
                {error, Error} ->
                    ?LLOG(warning, "could not start pool to ~s: ~p", [Ip2, Error], State),
                    start_pools(Rest, State)
            catch
                error:Error ->
                    ?LLOG(warning, "could not start pool to ~s: ~p", [Ip2, Error], State),
                    start_pools(Rest, State)
            end
    end.

