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

-export([plugin_deps/0, plugin_syntax/0, plugin_defaults/0, plugin_config/2]).
-export([object_store_reload_types/1, object_store_read_raw/2, object_store_save_raw/3,
         object_store_delete_raw/2]).
-export([object_store_find_obj/2,
         object_store_find_types/3, object_store_find_all_types/3,
         object_store_find_childs/3, object_store_find_all_childs/3,
         object_store_find_alias/2, object_store_delete_all_childs/3,
         object_store_find/2]).
-export([object_store_archive_find/2, object_store_archive_save_raw/3, object_store_clean/1]).
-export([elastic_get_indices/2, elastic_get_mappings/3, elastic_get_aliases/3,
         elastic_get_templates/2]).
-export([get_index/1, get_archive_index/1, get_mappings/2, get_templates/1]).


-define(ES_INDEX, <<"nkobjects_v2">>).
-define(ES_ALIAS, <<"nkobjects">>).
-define(ES_TYPE, <<"objs">>).
-define(ES_LOG_TEMPLATE, <<"nkdomain_objs">>).
-define(ES_ARCHIVE_INDEX, <<"nkarchive_v1">>).

%%-define(LLOG(Type, Txt, Args),
%%    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-record(es_config, {
    index,
    type,
    archive_index
}).


%% ===================================================================
%% Public
%% ===================================================================


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
        domain_elastic_replicas => {integer, 0, 3},
        domain_elastic_archive_index =>  binary
    }.


plugin_defaults() ->
    #{
        domain_elastic_url => <<"http://localhost:9200">>,
        domain_elastic_index => ?ES_INDEX,
        domain_elastic_alias => ?ES_ALIAS,
        domain_elastic_obj_type => ?ES_TYPE,
        domain_elastic_replicas => 2,
        domain_elastic_archive_index => ?ES_ARCHIVE_INDEX
    }.


plugin_config(Config, _Service) ->
    #{
        domain_elastic_url := Url,
        domain_elastic_index := Index,
        domain_elastic_obj_type := Type,
        domain_elastic_archive_index := ArchiveIndex
    } = Config,
    Cache = #es_config{
        index = Index,
        type = Type,
        archive_index = ArchiveIndex
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
    nkdomain_store_es:reload_types(SrvId).


%% @doc
object_store_read_raw(SrvId, ObjId) ->
    nkdomain_store_es:read_obj(SrvId, ObjId).


%% @doc
object_store_save_raw(SrvId, ObjId, Map) ->
    nkdomain_store_es:save_obj(SrvId, Map#{obj_id=>ObjId}).


%% @doc
object_store_delete_raw(SrvId, ObjId) ->
    nkdomain_store_es:delete_obj(SrvId, ObjId).


%% @doc
object_store_find_obj(SrvId, Id) ->
    nkdomain_store_es:find_obj(SrvId, Id).


%% @doc
object_store_find_alias(SrvId, Alias) ->
    nkdomain_store_es:find_obj_alias(SrvId, Alias).


%% @doc
object_store_find_types(SrvId, ObjId, Spec) ->
    nkdomain_store_es:find_types(SrvId, ObjId, Spec).


%% @doc
object_store_find_all_types(SrvId, ObjId, Spec) ->
    nkdomain_store_es:find_all_types(SrvId, ObjId, Spec).


%% @doc
object_store_find_childs(SrvId, ObjId, Spec) ->
    nkdomain_store_es:find_childs(SrvId, ObjId, Spec).


%% @doc
object_store_find_all_childs(SrvId, Path, Spec) ->
    nkdomain_store_es:find_all_childs(SrvId, Path, Spec).


%% @doc
object_store_delete_all_childs(SrvId, Path, Spec) ->
    nkdomain_store_es:delete_all_childs(SrvId, Path, Spec).


%% @doc
object_store_find(SrvId, Spec) ->
    nkdomain_store_es:find(SrvId, Spec).


%% @doc
object_store_archive_find(SrvId, Spec) ->
    nkdomain_store_es:archive_find(SrvId, Spec).


%% @doc
object_store_archive_save_raw(SrvId, ObjId, Map) ->
    nkdomain_store_es:archive_save_obj(SrvId, Map#{obj_id=>ObjId}).


%% @doc
object_store_clean(SrvId) ->
    nkdomain_store_es:clean(SrvId).


%% ===================================================================
%% NkElastic callbacks
%% ===================================================================


%% @private
elastic_get_indices(Acc, #{id:=SrvId}=Service) ->
    Indices = nkdomain_store_es:get_indices(SrvId),
    {continue, [maps:merge(Acc, Indices), Service]}.


%% @private
elastic_get_mappings(Index, Acc, #{id:=SrvId}=Service) ->
    Mappings = get_mappings(SrvId, Index),
    {continue, [Index, maps:merge(Acc, #{<<"objs">> => Mappings}), Service]}.


%% @private
elastic_get_aliases(Index, Acc, #{id:=SrvId}=Service) ->
    Aliases = nkdomain_store_es:get_aliases(SrvId, Index),
    {continue, [Index, maps:merge(Acc, Aliases), Service]}.


%% @private
elastic_get_templates(Acc, #{id:=SrvId}=Service) ->
    Templates = get_templates(SrvId),
    {continue, [maps:merge(Acc, Templates), Service]}.



%% ===================================================================
%% Private
%% ===================================================================

%% @private
-spec get_index(nkservice:id()) ->
    {Index::binary(), Type::binary()}.

get_index(SrvId) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    {Index, IdxType}.


%% @private
-spec get_archive_index(nkservice:id()) ->
    {Index::binary(), Type::binary()}.

get_archive_index(SrvId) ->
    #es_config{archive_index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    {Index, IdxType}.


%% @private Get ES indices
get_templates(SrvId) ->
    #{
        domain_elastic_index := Index,
        domain_elastic_archive_index := StoreIndex
    } =
        SrvId:config(),
    Mappings = get_mappings(SrvId, Index),
    #{
        ?ES_LOG_TEMPLATE => #{
            template => <<StoreIndex/binary, $*>>,
            mappings => #{
                ?ES_TYPE => #{
                    properties => Mappings
                }
            }
        }
    }.


%% @doc Gets all ES store mappings for all registered types
-spec get_mappings(nkservice:id(), binary()) -> map().

get_mappings(SrvId, Index) ->
    case SrvId:config() of
        #{
            domain_elastic_index := Index
        } ->
            lager:info("Installed types: ~p", [nkdomain_all_types:get_all_types()]),
            Modules = nkdomain_all_types:get_all_modules(),
            Base = SrvId:object_mapping(),
            lists:foldl(
                fun(Module, Acc) ->
                    #{type:=Type} = Module:object_get_info(),
                    Obj = get_object_mappings(Module),
                    Acc#{Type => Obj}
                end,
                Base,
                Modules);

        _ ->
            #{}
    end.


%% @private
get_object_mappings(Module) ->
    case Module:object_mapping() of
        Map when is_map(Map) ->
            #{
                type => object,
                dynamic => false,
                properties => Map
            };
        disabled ->
            #{
                enabled => false
            }
    end.