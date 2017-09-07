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
-module(nkdomain_store_es_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([error/1]).
-export([object_es_mapping/0, object_es_mapping/1, object_es_mapping/2, object_es_parse/1, object_es_unparse/1]).
-export([object_db_init/1, object_db_read/1, object_db_save/1, object_db_delete/1,
         object_db_find_obj/1, object_db_search/1, object_db_search_alias/1,
         object_db_search_childs/2, object_db_search_all_childs/2,
         object_db_search_types/2, object_db_search_all_types/2, object_db_search_agg_field/4,
         object_db_delete_all_childs/2, object_db_clean/0]).
-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).


-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(_) -> continue.



%% ===================================================================
%% Plugin Callbacks
%% ===================================================================

plugin_deps() ->
    [nkdomain_nkroot, nkelastic].


%% Other plugins can also parse db_clusters
plugin_syntax() ->
    #{
        nkdomain => #{
            db_store => binary,
            db_clusters => {list, #{
                id => binary,
                class => atom,
                url => binary,
                pool_size => {integer, 1, none},
                pool_overflow => {integer, 1, none},
                replicas => {integer, 1, 5},
                database => binary,
                '__mandatory' => [class]
            }},
            '__mandatory' => [db_store]
        }
    }.


plugin_config(#{nkdomain:=NkDomain}=Config, _Service) ->
    #{db_store:=DbStore} = NkDomain,
    Clusters = maps:get(db_clusters, NkDomain, []),
    Config2 = parse_clusters(Clusters, DbStore, Config),
    {ok, Config2};

plugin_config(_Config, _Service) ->
    continue.



%% ===================================================================
%% Store callbacks
%% ===================================================================


%% @doc ES base base mapping
-spec object_es_mapping() ->
    map().

object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        obj_id => #{type => keyword},
        srv_id => #{type => keyword},
        type => #{type => keyword},
        path => #{type => keyword},
        obj_name => #{type => keyword},
        domain_id => #{type => keyword},
        parent_id => #{type => keyword},
        subtype => #{type => keyword},
        created_by => #{type => keyword},
        created_time => #{type => date},
        updated_by => #{type => keyword},
        updated_time => #{type => date},
        enabled => #{type => boolean},
        active => #{type => boolean},
        expires_time => #{type => date},
        destroyed => #{type => boolean},
        destroyed_time => #{type => date},
        destroyed_code => #{type => keyword},
        destroyed_reason => #{type => keyword},
        name => #{
            type => text,
            analyzer => standard,
            fields => #{keyword => #{type=>keyword}}
        },
        name_norm => #{type=>text},
        description => #{
            type => text,
            analyzer => standard,
            fields => #{keyword => #{type=>keyword}}
        },
        description_norm => #{type=>text},
        tags => #{type => keyword},
        aliases => #{type => keyword},
        icon_id => #{type => keyword}
    }.


%% @doc Must return the submapping for a type
-spec object_es_mapping(module()) ->
    map() | not_exported.

object_es_mapping(Module) when is_atom(Module) ->
    ?CALL_NKROOT(object_apply, [Module, object_es_mapping, []]).


%% @doc Must return the submapping for a type
-spec object_es_mapping(nkservice:id(), nkdomain:type()) ->
    map() | not_exported.

object_es_mapping(SrvId, Type) ->
    ?CALL_NKROOT(object_apply, [SrvId, Type, object_es_mapping, []]).


%% @doc Must parse an object
-spec object_es_parse(map()) ->
    {ok, nkdomain:obj(), Unknown::[binary()]} | {error, term()}.

object_es_parse(Map) ->
    ?CALL_NKROOT(object_parse, [load, Map]).


%% @doc Called to serialize an object to ES format
-spec object_es_unparse(nkdomain:obj()) ->
    map().

object_es_unparse(#{srv_id:=SrvId, type:=Type}=Obj) ->
    BaseKeys = maps:keys(?CALL_NKROOT(object_es_mapping, [])),
    BaseMap1 = maps:with(BaseKeys, Obj),
    BaseMap2 = case BaseMap1 of
        #{pid:=Pid} ->
            BaseMap1#{pid:=base64:encode(term_to_binary(Pid))};
        _ ->
            BaseMap1
    end,
    BaseMap3 = case BaseMap2 of
        #{name:=Name} ->
            BaseMap2#{name_norm=>nkdomain_store_es_util:normalize_multi(Name)};
        _ ->
            BaseMap2
    end,
    BaseMap4 = case BaseMap3 of
        #{description:=Desc} ->
            BaseMap3#{description_norm=>nkdomain_store_es_util:normalize_multi(Desc)};
        _ ->
            BaseMap3
    end,
    case ?CALL_NKROOT(object_es_mapping, [SrvId, Type]) of
        not_exported ->
            BaseMap4#{Type => #{}};
        not_indexed ->
            ModData = maps:get(Type, Obj, #{}),
            BaseMap4#{Type => ModData};
        Map when is_map(Map) ->
            case ?CALL_NKROOT(object_apply, [SrvId, Type, object_es_unparse, [Obj, BaseMap4]]) of
                not_exported ->
                    ModData = maps:get(Type, Obj, #{}),
                    ModKeys = maps:keys(Map),
                    ModMap = maps:with(ModKeys, ModData),
                    BaseMap4#{Type => ModMap};
                Value when is_map(Value) ->
                    Value
            end
    end.



%% ===================================================================
%% Implemented callbacks
%% ===================================================================

%% @doc Initializes database
-spec object_db_init(nkservice:state()) ->
    {ok, nkservice:state()} | {error, term()}.

object_db_init(State) ->
    case nkdomain_store_es_util:get_index_opts() of
        {ok, IndexOpts, EsOpts} ->
            case nkdomain_store_es_util:db_init(IndexOpts, EsOpts) of
                ok ->
                    {ok, State};
                {error, Error} ->
                    {error, {object_db_init, Error}}
            end;
        _ ->
            continue
    end.


%% @doc Called to get and parse an object
-spec object_db_read(nkdomain:obj_id()) ->
    {ok, nkdomain:obj(), Meta::map()} | {error, term()}.

object_db_read(ObjId) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:read_obj(ObjId, EsOpts);
        _ ->
            continue
    end.


%% @doc Saves an object to database
-spec object_db_save(nkdomain:obj()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_save(#{obj_id:=ObjId}=Obj) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:save_obj(ObjId, Obj, EsOpts);
        _ ->
            continue
    end.


%% @doc Deletes an object to database
-spec object_db_delete(nkdomain:obj_id()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_delete(ObjId) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:delete_obj(ObjId, EsOpts);
        _ ->
            continue
    end.


%% @doc Finds an object from its ID or Path
-spec object_db_find_obj(nkdomain:id()) ->
    {ok, Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

object_db_find_obj(Id) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:find_obj(Id, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search(nkdomain:search_spec()) ->
    {ok, Total::integer(), Objs::[map()], Aggs::map(), Meta::map()} |
    {error, term()}.

object_db_search(Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search(Spec, EsOpts);
        _ ->
            continue
    end.



%% @doc
-spec object_db_search_alias(nkdomain:alias()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], Meta::map()} |
    {error, term()}.

object_db_search_alias(Alias) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search_obj_alias(Alias, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_types(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), integer()}], Meta::map()} | {error, term()}.

object_db_search_types(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search_types(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_all_types(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_all_types(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search_all_types(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_childs(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], Meta::map()} |
    {error, term()}.

object_db_search_childs(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search_childs(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_all_childs(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], Meta::map()} |
    {error, term()}.

object_db_search_all_childs(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search_all_childs(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc Must stop loaded objects
-spec object_db_delete_all_childs(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer()} | {error, term()}.

object_db_delete_all_childs(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:delete_all_childs(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_check_active/3 for each 'active' object found
-spec object_db_clean() ->
    ok | {error, term()}.

object_db_clean() ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:clean(EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_agg_field(nkdomain:id(), binary(),
                                 nkdomain:search_spec(), SubChilds::boolean()) ->
    {ok, Total::integer(), [{nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_agg_field(Id, Field, Spec, SubChilds) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es:search_agg_field(Id, Field, Spec, SubChilds, EsOpts);
        _ ->
            continue
    end.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
parse_clusters([], _DbStore, Config) ->
    Config;

parse_clusters([#{class:=nkelastic}=Data|Rest], DbStore, Config) ->
    Id = maps:get(id, Data, <<"main">>),
    Previous = maps:get(nkelastic, Config, []),
    Data2 = maps:with([id, url, pool_size, pool_overflow, replicas, database], Data#{id=>Id}),
    Config2 = Config#{nkelastic => [Data2|Previous]},
    Config3 = case DbStore of
        Id ->
            IndexOpts = #{
                number_of_replicas => maps:get(replicas, Data2, 2)
            },
            Database = maps:get(database, Data, <<"nkobjects">>),
            EsOpts = #{
                srv_id => ?NKSRV,
                cluster_id => Id,
                index => Database,
                type => <<"objs">>,
                refresh => true
            },
            % nkdomain_store will be captured by nkdomain and generate cache
            Config2#{nkdomain_db_store=>{elastic, IndexOpts, EsOpts}};
        _ ->
            Config2
    end,
    parse_clusters(Rest, DbStore, Config3);

parse_clusters([_|Rest], DbStore, Config) ->
    parse_clusters(Rest, DbStore, Config).

