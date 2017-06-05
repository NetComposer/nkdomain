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

-export([get_pool/1, get_pool/2]).
-export([start_link/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


-define(CHECK_TIME, 30000).

-define(LLOG(Type, Txt, Args, State),
    lager:Type("NkDOMAIN Store PGSQL Server (~p) "++Txt, [State#state.srv_id|Args])).

-define(DB, <<"nkobjects">>).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


get_pool(SrvId) ->
    get_pool(SrvId, <<"main">>).


get_pool(SrvId, Id) ->
    Name = SrvId:config_nkdomain_store_pgsql(),
    gen_server:call(Name, {get_pool, Id}).



%% @doc
-spec start_link(nkservice:id(), atom(), map()) ->
    {ok, pid()} | {error, term()}.

start_link(SrvId, Name, ConnMap) ->
    gen_server:start_link({local, Name}, ?MODULE, [SrvId, Name, ConnMap], []).



% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    srv_id :: nkservice:id(),
    name :: atom(),
    pools = #{} :: #{Id::binary() => pid()},
    conn_map = [] :: [{Id::binary(), Conn::list()}]
}).



%% @private
-spec init(term()) ->
    {ok, tuple()} | {ok, tuple(), timeout()|hibernate} |
    {stop, term()} | ignore.

init([SrvId, Name, ConnMap]) ->
    State = #state{
        srv_id = SrvId,
        name = Name,
        pools = #{},
        conn_map = maps:to_list(ConnMap)
    },
    process_flag(trap_exit, true),
    self() ! start_pools,
    {ok, State}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call({get_pool, Id}, _From, #state{pools=Pools}=State) ->
    case maps:find(Id, Pools) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        error ->
            {reply, {error, pool_not_available}, State}
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

handle_info(start_pools, #state{conn_map=ConnMap}=State) ->
    State2 = start_pools(ConnMap, State),
    erlang:send_after(?CHECK_TIME, self(), start_pools),
    {noreply, State2};

handle_info({'EXIT', Pid, _Reason}, #state{pools=Pools}=State) ->
    PoolList = maps:to_list(Pools),
    case lists:keytake(Pid, 2, PoolList) of
        {value, {Id, Pid}, PoolList2} ->
            ?LLOG(warning, "pool '~s' is down", [Id], State),
            {noreply, State#state{pools=maps:from_list(PoolList2)}};
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

start_pools([{Id, Conns}|Rest], #state{pools=Pools} = State) ->
    case maps:is_key(Id, Pools) of
        true ->
            start_pools(Rest, State);
        false ->
            SqlConns = get_conns(?DB, Conns, []),
            try
                case test_connect(SqlConns) of
                    ok ->
                        PoolOpts = [
                            {worker_module, nkdomain_store_pgsql_worker},
                            {size, 5},
                            {max_overflow, 10}
                        ],
                        {ok, Pid} = poolboy:start_link(PoolOpts, {Id, SqlConns}),
                        ?LLOG(notice, "started pool '~s'", [Id], State),
                        Pools2 = Pools#{Id => Pid},
                        start_pools(Rest, State#state{pools=Pools2});
                    {error, Error} ->
                        ?LLOG(warning, "could not start pool '~s': ~p", [Id, Error], State),
                        start_pools(Rest, State)
                end
            catch
                error:CError ->
                    ?LLOG(warning, "could not start pool '~s': ~p", [Id, CError], State),
                    start_pools(Rest, State)
            end
    end.



%% @private
get_conns(_Db, [], Acc) ->
    Acc;

get_conns(Db, [{[], _Opts}|Rest2], Acc) ->
    get_conns(Db, Rest2, Acc);

get_conns(Db, [{[{undefined, tcp, Ip, Port}|Rest1], Opts}|Rest2], Acc) ->
    ConnOpts = [
        {host, nklib_util:to_host(Ip)},
        {port, Port},
        {user, maps:get(user, Opts, <<"root">>)},
        {password, maps:get(password, Opts, <<"">>)},
        {database, Db},
        %{connect_timeout, ConnectTimeout},
        {as_binary, true}
    ],
    get_conns(Db, [{Rest1, Opts}|Rest2], [ConnOpts|Acc]).


%% @private
test_connect([]) ->
    {error, no_connections};

test_connect([Conn|Rest]) ->
    case pgsql_proto:start(Conn) of
        {ok, Pid} ->
            gen_server:call(Pid, terminate),
            ok;
        {error, Error} ->
            case Rest of
                [] ->
                    {error, Error};
                _ ->
                    test_connect(Rest)
            end
    end.
