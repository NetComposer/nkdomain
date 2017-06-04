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

%% @doc Elasticsearch plugin
-module(nkdomain_store_pgsql_worker).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([query/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%-define(LLOG(Type, Txt, Args),
%%    lager:Type("NkDOMAIN Store ES "++Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================

query(Pid, Sql) ->
    Start = nklib_util:l_timestamp(),
    Bin = iolist_to_binary(Sql),
    case gen_server:call(Pid, {squery, Bin}, 60000) of
        {ok, List} ->
            Time = (nklib_util:l_timestamp() - Start) / 1000,
            io:format("Q (~p): ~s\n", [Time, Bin]),
            lists:map(
                fun
                    ({error, Error}) ->
                        Code = nklib_util:get_value(code, Error, <<>>),
                        Msg = nklib_util:get_value(message, Error, <<>>),
                        {error, {Code, Msg}};
                    (Res) ->
                        {ok, Res}
                end,
                List);
        Other ->
            {error, Other}
    end.

%% ===================================================================
%% gen_server
%% ===================================================================


-record(state, {
    pid :: pid()
}).


%% @doc
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%% @private
init(Args) ->
    {ok, Pid} = pgsql_proto:start_link(Args),
    lager:info("NkDOMAIN STORE PSQL starting connection (~p)", [self()]),
    {ok, #state{pid=Pid}}.


%% @private
handle_call({squery, Sql}, _From, #state{pid=Pid}=State) ->
    Reply = pgsql:squery(Pid, Sql, 5000),
    {reply, Reply, State};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
handle_info(Info, State) ->
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
terminate(_Reason, _State) ->
    ok.


