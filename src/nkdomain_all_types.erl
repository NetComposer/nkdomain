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

-module(nkdomain_all_types).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_module/2, get_all_modules/0]).
-export([get_type/1, get_all_types/0]).
-export([get_service_types/1, get_all_services/0]).
-export([register/2]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Types "++Txt, Args)).

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds a type's module
-spec get_module(nkservice:id(), nkdomain:type()) ->
    module() | undefined.

get_module(SrvId, Type) ->
    SrvId2 = to_bin(SrvId),
    Type2 = to_bin(Type),
    case lookup({type, SrvId2, Type2}) of
        undefined ->
            case to_bin(?NKSRV) of
                SrvId2 ->
                    undefined;
                NkRoot ->
                    lookup({type, NkRoot, Type2})
            end;
        Module ->
            Module
    end.


%% @doc Gets all registered modules
-spec get_all_modules() ->
    [module()].

get_all_modules() ->
    lookup(all_modules, []).


%% @doc Finds a module's type
-spec get_type(module()) ->
    {nkservice:id(), nkdomain:type()} | undefined.

get_type(Module) ->
    lookup({module, Module}).


%% @doc Gets all registered types
-spec get_all_types() ->
    [nkdomain:type()].

get_all_types() ->
    lookup(all_types, []).


%% @doc Gets all services types
-spec get_service_types(nkservice:id()) ->
    [nkdomain:type()].

get_service_types(SrvId) ->
    lookup({service, to_bin(SrvId)}, []).


%% @doc Gets all services types
-spec get_all_services() ->
    [nkdomain:srv_id()].

get_all_services() ->
    lookup(all_services, []).


%% @doc Gets the obj module for a type
-spec register(nkservice:id(), module()) ->
    ok.

register(SrvId, Module) ->
    #{type:=Type} = Module:object_info(),
    Type2 = to_bin(Type),
    SrvId2 = to_bin(SrvId),
    _ = binary_to_atom(Type2, utf8),
    % Ensure we have the corresponding atoms loaded
    % We store the bin version of the service
    gen_server:call(?MODULE, {register_type, SrvId2, Type2, Module}).




% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {
    types = #{} :: #{nkdomain:type() => module()}
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

handle_call({register_type, SrvId, Type, Module}, _From, State) ->
    State2 = register_type(SrvId, Type, Module, State),
    {reply, ok, State2};

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

%% @private
lookup(Term) ->
    lookup(Term, undefined).


%% @private
lookup(Term, Default) ->
    case ets:lookup(?MODULE, Term) of
        [] -> Default;
        [{_, Val}] -> Val
    end.


%% @private
register_type(SrvId, Type, Module, #state{types=Types}=State) ->
    AtomSrv = binary_to_existing_atom(SrvId, utf8),
    AllModules1 = get_all_modules(),
    AllModules2 = lists:usort([Module|AllModules1]),
    AllTypes1 = get_all_types(),
    AllTypes2 = lists:usort([Type|AllTypes1]),
    AllServices1 = get_all_services(),
    AllServices2 = lists:usort([AtomSrv|AllServices1]),
    AllServiceTypes1 = get_service_types(AtomSrv),
    AllServiceTypes2 = lists:usort([Type|AllServiceTypes1]),
    ets:insert(?MODULE, [
        {all_modules, AllModules2},
        {all_types, AllTypes2},
        {all_services, AllServices2},
        {{service, SrvId}, AllServiceTypes2},
        {{type, SrvId, Type}, Module},
        {{module, Module}, {AtomSrv, Type}}
    ]),
    case maps:is_key(Type, Types) of
        false ->
            {ok, _} = nkdomain_types_sup:add_type(Module),
            Types2 = Types#{Type=>Module},
            State#state{types=Types2};
        true ->
            State
    end.


%% @private
to_bin(Term) -> nklib_util:to_binary(Term).