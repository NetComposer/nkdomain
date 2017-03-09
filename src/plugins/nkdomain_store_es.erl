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
-module(nkdomain_store_es).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_syntax/0, plugin_defaults/0, plugin_config/2]).
-export([object_store_reload_types/1, object_store_read_raw/2, object_store_save_raw/3,
         object_store_find_path/2, object_store_find_childs/3]).
-export([elastic_get_indices/2, elastic_get_mappings/3, elastic_get_aliases/3]).
-export([reload_types/1, remove_index/1]).

-define(ES_INDEX, <<"nkobjects_v2">>).
-define(ES_ALIAS, <<"nkobjects">>).
-define(ES_TYPE, <<"objs">>).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

-record(es_config, {
    index,
    type
}).


%% ===================================================================
%% Plugin Callbacks
%% ===================================================================

plugin_deps() ->
    [nkdomain, nkelastic].


plugin_syntax() ->
    #{
        domain_elastic_url => binary,
        domain_elastic_user => binary,
        domain_elastic_pass => binary,
        domain_elastic_index =>  binary,
        domain_elastic_alias => binary,
        domain_elastic_obj_type => binary,
        domain_elastic_replicas => {integer, 0, 3}
    }.


plugin_defaults() ->
    #{
        domain_elastic_url => <<"http://localhost:9200">>,
        domain_elastic_index => ?ES_INDEX,
        domain_elastic_alias => ?ES_ALIAS,
        domain_elastic_obj_type => ?ES_TYPE,
        domain_elastic_replicas => 2
    }.


plugin_config(Config, _Service) ->
    #{
        domain_elastic_url := Url,
        domain_elastic_index := Index,
        domain_elastic_obj_type := Type
    } = Config,
    Cache = #es_config{
        index = Index,
        type = Type
    },
    Config2 = case Config of
        #{
            domain_elastic_user := User,
            domain_elastic_pass := Pass
        } ->
            Config#{
                elastic_url => Url,
                elastic_user => User,
                elastic_pass => Pass
            };
        _ ->
            Config#{elastic_url => Url}
    end,
    {ok, Config2, Cache}.



%% ===================================================================
%% Store callbacks
%% ===================================================================

%% @doc
object_store_reload_types(SrvId) ->
    reload_types(SrvId).


%% @doc
object_store_read_raw(SrvId, ObjId) ->
    read_obj(SrvId, ObjId).


%% @doc
object_store_save_raw(SrvId, ObjId, Map) ->
    save_obj(SrvId, Map#{obj_id:=ObjId}).


%% @doc
object_store_find_path(SrvId, Path) ->
    find_obj_path(SrvId, Path).


%% @doc
object_store_find_childs(SrvId, Path, Spec) ->
    find_obj_childs(SrvId, Path, Spec).




%% ===================================================================
%% NkElastic callbacks
%% ===================================================================


%% @private
elastic_get_indices(Acc, #{id:=SrvId}=Service) ->
    Indices = get_indices(SrvId),
    {continue, [maps:merge(Acc, Indices), Service]}.


%% @private
elastic_get_mappings(Index, Acc, #{id:=SrvId}=Service) ->
    Mappings = get_mappings(SrvId, Index),
    {continue, [Index, maps:merge(Acc, #{<<"objs">> => Mappings}), Service]}.


%% @private
elastic_get_aliases(Index, Acc, #{id:=SrvId}=Service) ->
    Aliases = get_aliases(SrvId, Index),
    {continue, [Index, maps:merge(Acc, Aliases), Service]}.



%% ===================================================================
%% Internal
%% ===================================================================


%% @doc Reload new types
%% Types are loaded automatically when root service starts
%% However, they can also be loaded by hand, for example to check errors.
-spec reload_types(nkservice:id()) ->
    ok | {error, term()}.

reload_types(SrvId) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    Mappings = get_mappings(SrvId, Index),
    lager:info("Types: ~p", [maps:keys(Mappings)]),
    nkelastic_api:add_mapping(SrvId, Index, IdxType, Mappings).


%% @private
remove_index(SrvId) ->
    #es_config{index=Index} = SrvId:config_nkdomain_store_es(),
    nkelastic_api:delete_index(SrvId, Index).



%% @doc Reads an object
read_obj(SrvId, ObjId) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    case nkelastic_api:get(SrvId, Index, IdxType, ObjId) of
        {ok, #{<<"type">>:=BType}=Data, Vsn} ->
            case catch binary_to_existing_atom(BType, utf8) of
                Type when is_atom(Type) ->
                    {ok, Type, Data#{'_store_vsn'=>Vsn}};
                _ ->
                    ?LLOG(notice, "invalud type loaded: ~p", [BType]),
                    {error, invalid_type}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Saves an object
save_obj(SrvId, #{obj_id:=ObjId}=Store) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    nkelastic_api:put(SrvId, Index, IdxType, ObjId, Store).


%% @doc Finds an object from its path
-spec find_obj_path(nkservice:id(), nkdomain:domain()) ->
    {ok, nkdomain:type(), nkdomain:obj_id()} | {error, object_not_found|term()}.

find_obj_path(SrvId, Path) ->
    case nkdomain_types:is_path(Path) of
        {true, Path2} ->
            case find_path(SrvId, Path2, #{}) of
                {ok, 1, [{Type, ObjId}]} ->
                    {ok, Type, ObjId};
                {ok, _, [{Type, ObjId}|_]} ->
                    ?LLOG(warning, "Multiple objects for path ~s", [Path]),
                    {ok, Type, ObjId};
                not_found ->
                    {error, object_not_found}
            end;
        false ->
            {error, invalid_path}
    end.


%% @doc Finds all objects on a path
-spec find_obj_childs(nkservice:id(), nkdomain:domain(), nkelastic_api:list_opts()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id()}]} |
    {error, object_not_found}.

find_obj_childs(SrvId, Path, Spec) ->
    case nkdomain_types:is_path(Path) of
        {true, Path2} ->
            case find_path(SrvId, list_to_binary([Path2, "*"]), Spec) of
                {ok, N, List} ->
                    {ok, N, List};
                not_found ->
                    {error, object_not_found}
            end;
        false ->
            {error, invalid_path}
    end.



%% ===================================================================
%% Util
%% ===================================================================

%% @doc Get ES indices
get_indices(SrvId) ->
    #{
        domain_elastic_index := Index,
        domain_elastic_replicas := Replicas
    } =
        SrvId:config(),
    #{
        Index => #{
            number_of_replicas => Replicas
        }
    }.


%% @doc Gets all ES store mappings for all registered types
-spec get_mappings(nkservice:id(), binary()) -> map().

get_mappings(SrvId, Index) ->
    case SrvId:config() of
        #{
            domain_elastic_index := Index
        } ->
            Types = nkdomain_types:get_types(),
            lager:info("Installed types: ~p", [Types]),
            Base = SrvId:object_base_mapping(),
            lists:foldl(
                fun(Type, Acc) ->
                    Obj = #{
                        type => object,
                        dynamic => false,
                        properties => Type:object_get_mapping()
                    },
                    Acc#{Type => Obj}
                end,
                Base,
                Types);

        _ ->
            #{}
    end.


%% @doc Get ES aliases
get_aliases(SrvId, Index) ->
    case SrvId:config() of
        #{
            domain_elastic_index := Index,
            domain_elastic_alias := Alias
        } ->
            #{Alias => #{}};
        _ ->
            #{}
    end.


%% @private
find_path(SrvId, Path, Spec) ->
    Spec2 = Spec#{
        fields => [<<"type">>],
        filter => #{<<"path">> => escape_url(nklib_util:to_list(Path), [])}
    },
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    case nkelastic_api:list(SrvId, Index, IdxType, Spec2) of
        {ok, 0, _} ->
            not_found;
        {ok, N, List} ->
            Data = lists:map(
                fun(#{<<"_id">>:=ObjId, <<"type">>:=BType}) ->
                    case catch binary_to_existing_atom(BType, utf8) of
                        {'EXIT', _} ->
                            ?LLOG(warning, "Invalid type in store: ~s", [BType]),
                            {BType, ObjId};
                        Type ->
                            {Type, ObjId}
                    end
                end,
                List),
            {ok, N, Data};
        _ ->
            not_found
    end.


%% @private
escape_url([], Acc) ->
    list_to_binary(lists:reverse(Acc));
escape_url([$/|Rest], Acc) ->
    escape_url(Rest, [$/, 92 | Acc]);
escape_url([Char|Rest], Acc) ->
    escape_url(Rest, [Char|Acc]).