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

-module(nkdomain_types).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_modules/0, register_type/1, is_path/1]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Types "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets all registered types
-spec get_modules() ->
    [module()].

get_modules() ->
    case ets:lookup(?MODULE, all_modules) of
        [] -> [];
        [{_, List}] -> List
    end.


%% @doc Gets the obj module for a type
-spec register_type(module()) ->
    ok.

register_type(Module) ->
    gen_server:call(?MODULE, {register_type, Module}).


%% @doc Normalizes a path
%% Valid paths either start with / or has '@' or '.'
-spec is_path(list()|binary()) ->
    {true, nkdomain:path()} | false.

is_path(Path) when is_list(Path) ->
    is_path(list_to_binary(Path));

is_path(<<"/", _/binary>>=Path) ->
    {true, Path};
is_path(Path) ->
    case binary:split(nklib_util:to_binary(Path), <<"@">>) of
        [Name, Path1] ->
            Path2 = binary:split(Path1, <<".">>, [global]),
            Path3 = nklib_util:bjoin(lists:reverse(Path2), <<"/">>),
            {true, <<"/", Path3/binary, "/", Name/binary>>};
        [Path1] ->
            case binary:split(Path1, <<".">>, [global]) of
                [_] ->
                    false;
                Path2 ->
                    Path3 = nklib_util:bjoin(lists:reverse(Path2), <<"/">>),
                    {true, <<"/", Path3/binary>>}
            end
    end.


% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-record(state, {
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init([]) ->
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    {ok, #state{}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call({register_type, Module}, _From, State) ->
    AllModules1 = get_modules(),
    AllModules2 = lists:usort([Module|AllModules1]),
    ets:insert(?MODULE, {all_modules, AllModules2}),
    %%    ets:insert(?MODULE, {{type, Type}, Module}),
    {reply, ok, State};

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
