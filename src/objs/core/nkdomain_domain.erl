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

%% @doc Domain Object

-module(nkdomain_domain).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').


-export([get_config/2, set_config/3, apply_config/3, get_default/2, set_default/3]).
-export([get_recursive_config/3]).
-export([find_path/1, find_path/2, unload_childs/1, get_childs_type/2]).
-export([get_all_counters/1, get_counter/2]).
-export([sync_op/2, async_op/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").




%% ===================================================================
%% Public
%% ===================================================================


%%%% @doc
%%search(Id, Spec) ->
%%    case nkdomain_db:find(Id) of
%%        #obj_id_ext{obj_id=ObjId} ->
%%            Filters1 = maps:get(filters, Spec, #{}),
%%            Filters2 = Filters1#{domain_id=>ObjId},
%%            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
%%            nkdomain:search(Spec2);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%search_all(Id, Spec) ->
%%    case nkdomain_db:find(Id) of
%%        #obj_id_ext{path=Path} ->
%%            Filters1 = maps:get(filters, Spec, #{}),
%%            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
%%            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
%%            nkdomain:search(Spec2);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%search_type(Id, Spec) ->
%%    case nkdomain_lib:find(Id) of
%%        #obj_id_ext{obj_id=ObjId} ->
%%            ?CALL_NKROOT(object_db_search_types, [ObjId, Spec]);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%search_all_types(Id, Spec) ->
%%    case nkdomain_lib:find(Id) of
%%        #obj_id_ext{path=Path} ->
%%            ?CALL_NKROOT(object_db_search_all_types, [Path, Spec]);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%search_childs(Id, Spec) ->
%%    case nkdomain_lib:find(Id) of
%%        #obj_id_ext{obj_id=ObjId} ->
%%            ?CALL_NKROOT(object_db_search_childs, [ObjId, Spec]);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%search_all_childs(Id, Spec) ->
%%    case nkdomain_lib:find(Id) of
%%        #obj_id_ext{path=Path} ->
%%            ?CALL_NKROOT(object_db_search_all_childs, [Path, Spec]);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc Gets a 'config' key
-spec get_config(nkdomain:id(), Key::binary()) ->
    {ok, map()} | {error, term()}.

get_config(DomainId, Key) ->
    sync_op(DomainId, {get_config, nklib_util:to_binary(Key)}).


%% @doc Sets a 'config' key
-spec set_config(nkdomain:id(), Key::binary(), Val::map()) ->
    {ok, map()} | {error, term()}.

set_config(DomainId, Key, Val) when is_map(Val) ->
    sync_op(DomainId, {set_config, nklib_util:to_binary(Key), Val}).


%% @doc Sets a 'config' key
-spec apply_config(nkdomain:id(), Key::binary(), fun((term()) -> {ok, term()} | {error, term()})) ->
    {ok, map()} | {error, term()}.

apply_config(DomainId, Key, Fun) when is_function(Fun, 1) ->
    sync_op(DomainId, {apply_config, nklib_util:to_binary(Key), Fun}).


%% @doc Get a 'defaults' key
-spec get_default(nkdomain:id(), Key::binary()) ->
    {ok, map()} | {error, term()}.

get_default(DomainId, Key) ->
    sync_op(DomainId, {get_default, nklib_util:to_binary(Key)}).


%% @doc Sets a 'defaults' key
-spec set_default(nkdomain:id(), Key::binary(), Val::map()) ->
    {ok, map()} | {error, term()}.

set_default(DomainId, Key, Val) when is_map(Val) ->
    sync_op(DomainId, {set_default, nklib_util:to_binary(Key), Val}).


%% @doc Get a key recursively from this domain to the root
-spec get_recursive_config(nkdomain:id(), Key::binary(), list()) ->
    {ok, list()} | {error, term()}.

get_recursive_config(<<"root">>, Key, Base) when is_list(Base) ->
    join_config_with(<<"root">>, Key, Base);

get_recursive_config(DomainId, Key, Base) when is_list(Base) ->
    {ok, NewBase} = join_config_with(DomainId, Key, Base),
    case nkdomain:get_obj(DomainId) of
        {ok, #{parent_id := ParentDomain}} ->
            get_recursive_config(ParentDomain, Key, NewBase);
        {error, Error} ->
            lager:warning("nkdomain_domain:get_recursive_config Couldn't load parent for ~p", [DomainId]),
            {error, Error}
    end.


%% @private
join_config_with(DomainId, Key, Base) when is_list(Base) ->
    case sync_op(DomainId, {get_config, nklib_util:to_binary(Key)}) of
        {ok, Map} when is_map(Map) ->
            List = maps:to_list(Map),
            {ok, List ++ Base};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds a child object with this path
%% Must be send to a domain that is part of the path (or root to be sure)
-spec find_path(binary()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} | {error, term()}.

find_path(Path) ->
    find_path(<<"root">>, Path).


%% @doc Finds a child object with this path
%% Must be send to a domain that is part of the path (or root to be sure)
-spec find_path(nkdomain:obj_id(), binary()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} | {error, term()}.

find_path(Id, Path) ->
    case nkdomain_util:get_parts(Path) of
        {ok, Base, Type, ObjName} ->
            sync_op(Id, {find_path, Base, Type, ObjName});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_childs_type(Id, Type) ->
    sync_op(Id, {get_childs_type, nklib_util:to_binary(Type)}).


%% @doc
get_counter(Id, Type) ->
    sync_op(Id, {get_counter, nklib_util:to_binary(Type)}).


%% @doc
get_all_counters(Id) ->
    sync_op(Id, get_all_counters).


%% @doc
unload_childs(Id) ->
    sync_op(Id, unload_childs).



%%%% @doc Makes a full path form a domain and a obj_name
%%-spec make_path(nkdomain:id(), nkdomain:type(), binary()) ->
%%    {ok, nkdomain:path()} | {error, term()}.
%%
%%make_path(Id, Type, Name) ->
%%    case nkdomain_db:find(Id) of
%%        #obj_id_ext{type=?DOMAIN_DOMAIN, path=Path} ->
%%            Class = nkdomain_util:class(Type),
%%            Path2 = nkdomain_util:append(Path, Class),
%%            Name2 = nkdomain_util:name(Name),
%%            Path3 = nkdomain_util:append(Path2, Name2),
%%            {ok, Path3};
%%        {error, object_not_found} ->
%%            {error, domain_not_found};
%%        {error, Error} ->
%%            {error, Error}
%%    end.






%% @private
sync_op(Domain, Op) ->
    nkdomain_obj:sync_op(Domain, {nkdomain_domain_obj, Op}).


%% @private
async_op(Domain, Op) ->
    nkdomain_obj:async_op(Domain, {nkdomain_domain_obj, Op}).
