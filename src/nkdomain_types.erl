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


-export([get_types/0, get_callback/1, register_type/2, reload_types/1]).
-export([get_es_indices/0, get_es_mappings/2, get_es_aliases/1, load_es_obj/3, save_obj/2]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
    handle_cast/2, handle_info/2]).


-define(ES_INDEX, <<"nkobjects_v2">>).
-define(ES_ALIAS, <<"nkobjects">>).



%%-define(DEBUG(Txt, Args, State),
%%    case erlang:get(object_debug) of
%%        true -> ?LLOG(debug, Txt, Args, State);
%%        _ -> ok
%%    end).
%%
%%-define(LLOG(Type, Txt, Args, State),
%%    lager:Type(
%%        [
%%            {obj_id, State#state.obj_id},
%%            {type, State#state.type}
%%        ],
%%        "NkDOMAIN Obj (~s:~s) "++Txt,
%%        [State#state.obj_id, State#state.type | Args])).
%%
%%-define(SRV_DELAYED_DESTROY, 5000).
%%-define(DEF_SYNC_CALL, 5000).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets all registered types
-spec get_types() ->
    [nkdomain:type()].

get_types() ->
    case ets:lookup(?MODULE, all_types) of
        [] -> [];
        [{_, List}] -> List
    end.


%% @doc Gets the obj module for a type
-spec get_callback(nkdomain:type()) ->
    module() | undefined.

get_callback(Type) ->
    case ets:lookup(?MODULE, {type, Type}) of
        [] -> undefined;
        [{_, Mod}] -> Mod
    end.


%% @doc Gets the obj module for a type
-spec register_type(nkdomain:type(), module()) ->
    ok.

register_type(Type, Module) ->
    gen_server:call(?MODULE, {register_type, Type, Module}).


%% @doc Reload new types
%% Types are loaded automatically when root service starts
%% However, they can also be loaded by hand, for example to check errors.
-spec reload_types(nkservice:id()) ->
    ok | {error, term()}.

reload_types(SrvId) ->
    Mappings = get_es_mappings(?ES_INDEX, SrvId),
    lager:info("Types: ~p", [maps:keys(Mappings)]),
    reload_types(SrvId, maps:to_list(Mappings)).


%% @private
reload_types(_SrvId, []) ->
    ok;

reload_types(SrvId, [{Type, Data}|Rest]) ->
    case nkelastic_api:add_mapping(SrvId, ?ES_INDEX, Type, Data) of
        ok ->
            reload_types(SrvId, Rest);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Saves an object
save_obj(SrvId, #{obj_id:=ObjId, type:=Type}=Store) ->
    nkelastic_api:put(SrvId, ?ES_INDEX, Type, ObjId, Store).


%% ===================================================================
%% Util ES functions
%% ===================================================================

%% @doc Get ES indices
get_es_indices() ->
    #{
        ?ES_INDEX => #{
            number_of_replicas => 2
        }
    }.


%% @doc Gets all ES store mappings for all registered types
-spec get_es_mappings(binary(), nkservice:id()) -> map().

get_es_mappings(?ES_INDEX, SrvId) ->
    Types = get_types(),
    lager:info("Installed types: ~p", [Types]),
    Base = SrvId:domain_store_base_mapping(),
    lists:foldl(
        fun(Type, Acc) ->
            Mod = get_callback(Type),
            Map = Mod:object_get_mapping(),
            Acc#{Type => maps:merge(Map, Base)}
        end,
        #{},
        Types);

get_es_mappings(_Index, _SrvId) ->
    #{}.


%% @doc Get ES aliases
get_es_aliases(?ES_INDEX) ->
    #{?ES_ALIAS => #{}};

get_es_aliases(_Index) ->
    #{}.


%% @doc Gets an object from ES
-spec load_es_obj(nkservice:id(), nkdomain:type(), nkdomain:obj_id()) ->
    {ok, nkdomain:meta(), nkdomain:obj()} | {error, object_not_found|term()}.

load_es_obj(SrvId, Type, ObjId) ->
    case nkelastic_api:get(SrvId, ?ES_INDEX, Type, ObjId) of
        {ok, Data, Vsn} ->
            {ok, #{es_vsn=>Vsn}, Data};
        {error, object_not_found} ->
            not_found;
        {error, Error} ->
            {error, Error}
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

handle_call({register_type, Type, Module}, _From, State) ->
    AllTypes1 = get_types(),
    AllTypes2 = lists:usort([Type|AllTypes1]),
    ets:insert(?MODULE, {all_types, AllTypes2}),
    ets:insert(?MODULE, {{type, Type}, Module}),
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

