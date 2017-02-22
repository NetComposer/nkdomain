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

-module(nkdomain_obj_alias).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj).

-export([add_alias/2, remove_alias/2]).
-export([init/2, load/4, handle_info/3]).

-include("nkdomain.hrl").


-type alias() ::
    nkdomain_obj:base_opt() |
    #{
        aliases => [nkdomain:user_obj_id()]
    }.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec add_alias(nkdomain:obj_id(), nkdomain:user_obj_id()) ->
    ok | {error, term()}.

add_alias(AliasId, UserObjId) ->
    case nkdomain_obj:load(alias, AliasId, #{add_alias=>UserObjId}, #{}) of
        {error, Error} -> {error, Error};
        _ -> ok
    end.


%% @doc
-spec remove_alias(nkdomain:obj_id(), nkdomain:user_obj_id()) ->
    ok | {error, term()}.

remove_alias(AliasId, UserObjId) ->
    case nkdomain_obj:load(alias, AliasId, #{remove_alias=>UserObjId}, #{}) of
        {error, Error} -> {error, Error};
        _ -> ok
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(state, {
    id :: nkdomain:obj_id()
}).


%% @private
-spec init(nkdomain:obj_id(), alias()) ->
    {ok, nkdomain_obj:init_opts(), alias(), #state{}}.

init(AliasId, Alias) ->
    Timeout = nkdomain_app:get(alias_timeout),
    Alias1 = maps:merge(#{aliases=>[]}, Alias),
    {ok, #{timeout=>Timeout}, Alias1, #state{id=AliasId}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), alias(), #state{}) ->
    {ok, nkdomain:obj(), #state{}} | {removed, #state{}} | {error, term(), #state{}}.

load(Data, _Opts, Alias, State) ->
    do_load(maps:to_list(Data), Alias, State).


%% @private
-spec handle_info(term(), alias(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}} | remove.

handle_info(timeout, Alias, State) ->
    case Alias of
        #{aliases:=[]} -> 
            {removed, State};
        _ ->
            {stop, normal, State}
    end;

handle_info(Info, _Alias, State) -> 
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_load([], Alias, State) ->
    {ok, Alias, State};

do_load([{add_alias, Id}|Rest], Alias, State) ->
    OldList = maps:get(aliases, Alias),
    Alias1 = case lists:member(Id, OldList) of
        true -> Alias;
        false -> maps:put(aliases, lists:sort([Id|OldList]), Alias)
    end,
    do_load(Rest, Alias1, State);


do_load([{remove_alias, Id}|Rest], Alias, State) ->
    OldList = maps:get(aliases, Alias),
    Alias1 = case lists:member(Id, OldList) of
        true -> maps:put(aliases, lists:sort(OldList--[Id]), Alias);
        false -> Alias
    end,
    do_load(Rest, Alias1, State);

do_load([{Key, Val}|Rest], Alias, State) ->
    do_load(Rest, maps:put(Key, Val, Alias), State).




