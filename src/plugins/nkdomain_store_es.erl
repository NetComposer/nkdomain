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
         object_store_delete_raw/2]).
-export([object_store_find_obj/2,
         %% object_store_find_obj_id/2, object_store_find_path/2,
         object_store_find_types/3, object_store_find_all_types/3,
         object_store_find_childs/3, object_store_find_all_childs/3,
         object_store_find_alias/2, object_store_delete_all_childs/3,
         object_store_find/2]).
-export([object_store_archive_find/2, object_store_archive_save_raw/3, object_store_clean/1]).
-export([elastic_get_indices/2, elastic_get_mappings/3, elastic_get_aliases/3,
         elastic_get_templates/2]).
-export([reload_types/1, remove_index/1]).

-define(ES_INDEX, <<"nkobjects_v2">>).
-define(ES_ALIAS, <<"nkobjects">>).
-define(ES_TYPE, <<"objs">>).
-define(ES_LOG_TEMPLATE, <<"nkdomain_objs">>).
-define(ES_ARCHIVE_INDEX, <<"nkarchive_v1">>).
-define(ES_ITER_SIZE, 100).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

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
    reload_types(SrvId).


%% @doc
object_store_read_raw(SrvId, ObjId) ->
    read_obj(SrvId, ObjId).


%% @doc
object_store_save_raw(SrvId, ObjId, Map) ->
    save_obj(SrvId, Map#{obj_id=>ObjId}).


%% @doc
object_store_delete_raw(SrvId, ObjId) ->
    delete_obj(SrvId, ObjId).


%% @doc
object_store_archive_find(SrvId, Spec) ->
    archive_find(SrvId, Spec).


%% @doc
object_store_archive_save_raw(SrvId, ObjId, Map) ->
    archive_save_obj(SrvId, Map#{obj_id=>ObjId}).


%% @doc
object_store_find_obj(SrvId, Id) ->
    find_obj(SrvId, Id).


%%%% @doc
%%object_store_find_obj_id(SrvId, ObjId) ->
%%    find_obj_id(SrvId, ObjId).
%%
%%
%%%% @doc
%%object_store_find_path(SrvId, Path) ->
%%    find_path(SrvId, Path).


%% @doc
object_store_find_alias(SrvId, Alias) ->
    find_obj_alias(SrvId, Alias).


%% @doc
object_store_find_types(SrvId, ObjId, Spec) ->
    find_types(SrvId, ObjId, Spec).


%% @doc
object_store_find_all_types(SrvId, ObjId, Spec) ->
    find_all_types(SrvId, ObjId, Spec).


%% @doc
object_store_find_childs(SrvId, ObjId, Spec) ->
    find_obj_childs(SrvId, ObjId, Spec).


%% @doc
object_store_find_all_childs(SrvId, Path, Spec) ->
    find_obj_all_childs(SrvId, Path, Spec).


%% @doc
object_store_delete_all_childs(SrvId, Path, Spec) ->
    delete_obj_all_childs(SrvId, Path, Spec).


%% @doc
object_store_find(SrvId, Spec) ->
    find(SrvId, Spec).


%% @doc
object_store_clean(SrvId) ->
    clean(SrvId).


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


%% @private
elastic_get_templates(Acc, #{id:=SrvId}=Service) ->
    Templates = get_templates(SrvId),
    {continue, [maps:merge(Acc, Templates), Service]}.


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
    lager:info("Mappings: ~s", [nklib_json:encode_pretty(Mappings)]),

    case nkelastic_api:add_mapping(SrvId, Index, IdxType, Mappings) of
        ok ->
            do_reload_templates(SrvId, maps:to_list(get_templates(SrvId)));
        {error, Error} ->
            {error, Error}
    end.

%% @private
do_reload_templates(_SrvId, []) ->
    ok;

do_reload_templates(SrvId, [{Name, Data}|Rest]) ->
    case nkelastic_api:create_template(SrvId, Name, Data) of
        ok ->
            do_reload_templates(SrvId, Rest);
        {error, Error} ->
            {error, Error}
    end.


%% @private
remove_index(SrvId) ->
    #es_config{index=Index} = SrvId:config_nkdomain_store_es(),
    nkelastic_api:delete_index(SrvId, Index).


%% @doc Reads an object
read_obj(SrvId, ObjId) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    case nkelastic_api:get(SrvId, Index, IdxType, ObjId) of
        {ok, Data, Vsn} ->
            {ok, Data#{'_store_vsn'=>Vsn}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Saves an object
save_obj(SrvId, #{obj_id:=ObjId}=Store) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    nkelastic_api:put_and_wait(SrvId, Index, IdxType, ObjId, Store).


%% @doc Removes an object
delete_obj(SrvId, ObjId) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    case find_obj_childs(SrvId, ObjId, #{size=>0}) of
        {ok, 0, []} ->
            nkelastic_api:delete_and_wait(SrvId, Index, IdxType, ObjId);
        {ok, _, _} ->
            {error, object_has_childs};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds an object from its ID or Path
-spec find_obj(nkservice:id(), nkdomain:id()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

find_obj(SrvId, Id) ->
    Filter = case nkdomain_util:is_path(Id) of
        {true, Path} ->
            #{term => #{path => Path}};
        false ->
            #{term => #{obj_id => nklib_util:to_binary(Id)}}
    end,
    case do_search_objs(SrvId, query_filter(Filter), #{}) of
        {ok, 0, []} ->
            {error, object_not_found};
        {ok, 1, [{Type, ObjId, ObjPath}]} ->
            {ok, Type, ObjId, ObjPath};
        {ok, _, [{Type, ObjId, ObjPath}|_]} ->
            ?LLOG(warning, "Multiple objects for path ~s", [ObjPath]),
            {ok, Type, ObjId, ObjPath}
    end.



%%%% @doc Finds an object from its path
%%-spec find_obj_id(nkservice:id(), nkdomain:obj_id()) ->
%%    {ok, nkdomain:type(), nkdomain:path()} | {error, object_not_found|term()}.
%%
%%find_obj_id(SrvId, ObjId) ->
%%    Query = query_filter(#{term => #{obj_id => ObjId}}),
%%    case do_search_objs(SrvId, Query, #{}) of
%%        {ok, 0, []} ->
%%            {error, object_not_found};
%%        {ok, 1, [{Type, _ObjId, Path}]} ->
%%            {ok, Type, Path};
%%        {ok, _, [{Type, _ObjId, Path}|_]} ->
%%            ?LLOG(warning, "Multiple objects for path ~s", [Path]),
%%            {ok, Type, Path}
%%    end.
%%
%%
%%%% @doc Finds an object from its path
%%-spec find_path(nkservice:id(), nkdomain:path()) ->
%%    {ok, nkdomain:type(), nkdomain:obj_id()} | {error, object_not_found|term()}.
%%
%%find_path(SrvId, Path) ->
%%    case nkdomain_util:is_path(Path) of
%%        {true, Path2} ->
%%            Query = query_filter(#{term => #{path => Path2}}),
%%            case do_search_objs(SrvId, Query, #{}) of
%%                {ok, 0, []} ->
%%                    {error, object_not_found};
%%                {ok, 1, [{Type, ObjId, _Path}]} ->
%%                    {ok, Type, ObjId};
%%                {ok, _, [{Type, ObjId, _Path}|_]} ->
%%                    ?LLOG(warning, "Multiple objects for path ~s", [Path]),
%%                    {ok, Type, ObjId}
%%            end;
%%        false ->
%%            {error, invalid_path}
%%    end.


%% @doc Finds types
-spec find_types(nkservice:id(), nkdomain:obj_id(), nkelastic_api:search_opts()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]}.

find_types(SrvId, ObjId, Opts) ->
    case query_filters(#{parent_id=>ObjId}, Opts) of
        {ok, Query} ->
            do_search_types(SrvId, Query, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all types
-spec find_all_types(nkservice:id(), nkdomain:path(), nkelastic_api:search_opts()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]}.

find_all_types(SrvId, Path, Opts) ->
    case query_filters(#{childs_of=>Path}, Opts) of
        {ok, Query} ->
            do_search_types(SrvId, Query, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec find_obj_childs(nkservice:id(), nkdomain:obj_id(),
                      nkelastic_api:search_opts() | #{type:=nkdomain:type()}) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]}.

find_obj_childs(SrvId, ParentId, Opts) ->
    case query_filters(#{parent_id=>ParentId}, Opts) of
        {ok, Query} ->
            do_search_objs(SrvId, Query, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec find_obj_all_childs(nkservice:id(), nkdomain:domain(), nkelastic_api:list_opts()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id()}]}.

find_obj_all_childs(SrvId, Path, Opts) ->
    QueryFilters = query_filters(#{childs_of=>Path}, Opts),
    case QueryFilters of
        {ok, Query} ->
            do_search_objs(SrvId, Query, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects having an alias
-spec find_obj_alias(nkservice:id(), binary()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id()}]} |
    {error, object_not_found}.

find_obj_alias(SrvId, Alias) ->
    case query_filters(#{aliases=>Alias}, #{}) of
        {ok, Query} ->
            do_search_objs(SrvId, Query, #{});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Generic find
-spec find(nkservice:id(), map()) ->
    {ok, integer(), [map()], map(), map()} |
    {error, object_not_found}.

find(SrvId, Spec) ->
    case query_filters(#{}, Spec) of
        {ok, Query} ->
            case do_search(SrvId, Query, Spec) of
                {ok, N, Data, _Aggs, Meta} ->
                    {ok, N, Data, Meta};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc Archive an object
archive_find(SrvId, Opts) ->
    case query_filters(#{}, Opts) of
        {ok, Query} ->
            case do_search_archive(SrvId, Query, Opts) of
                {ok, N, Data, _Aggs, Meta} ->
                    {ok, N, Data, Meta};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Archive an object
archive_save_obj(SrvId, #{obj_id:=ObjId}=Store) ->
    #es_config{archive_index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    {{Y,M,D}, {_H,_Mi,_S}} = calendar:universal_time(),
    Index2 = list_to_binary(io_lib:format("~s-~4..0B~2..0B~2..0B", [Index, Y,M,D])),
    case nkelastic_api:put(SrvId, Index2, IdxType, ObjId, Store) of
        {ok, _Vsn} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec delete_obj_all_childs(nkservice:id(), nkdomain:domain(), nkelastic_api:list_opts()) ->
    {ok, integer()} | {error, term()}.

delete_obj_all_childs(SrvId, Path, Opts) ->
    case query_filters(#{childs_of=>Path}, Opts) of
        {ok, Query} ->
            Opts2 = Opts#{fields=>[<<"path">>], sort=>[#{<<"path">> => #{order=>desc}}]},
            Fun = fun(#{<<"obj_id">>:=ObjId}, Acc) ->
                case nkdomain_store:delete(SrvId, ObjId) of
                    ok ->
                        {ok, Acc+1};
                    {error, object_not_found} ->
                        {ok, Acc};
                    {error, Error} ->
                        {error, Error}
                end
            end,
            iterate(SrvId, Query, Opts2, Fun, 0);
        {error, Error} ->
            {error, Error}
    end.


%% @private
clean(SrvId) ->
    Active = do_clean_active(SrvId),
    {ok, #{active=>Active}}.


%% @private
do_clean_active(SrvId) ->
    Query = query_filter(#{term => #{active => true}}),
    Opts = #{size=>?ES_ITER_SIZE, fields=>[<<"type">>]},
    Fun = fun(#{<<"obj_id">>:=ObjId, <<"type">>:=Type}, Acc) ->
        case SrvId:object_check_active(SrvId, Type, ObjId) of
            false ->
                {ok, Acc+1};
            true ->
                {ok, Acc}
        end
    end,
    iterate(SrvId, Query, Opts, Fun, 0).





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
            lager:info("Installed types: ~p", [nkdomain_types:get_types()]),
            Modules = nkdomain_types:get_modules(),
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


%% @doc Get ES indices
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



%% @private
query_filters(AddFilters, Opts) ->
    Filters1 = maps:get(filters, Opts, #{}),
    Filters2 = maps:merge(Filters1, AddFilters),
    case get_filters(maps:to_list(Filters2), []) of
        {ok, [EsFilter]} ->
            {ok, #{
                constant_score => #{
                    filter => EsFilter
                }
            }};
        {ok, EsFilters} ->
            {ok, #{
                bool => #{
                    filter => EsFilters
                }
            }};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_filters([], Acc) ->
    {ok, Acc};

get_filters([{Field, Value}|Rest], Acc) ->
    Filter = case Field of
        id ->
            case nkdomain_util:is_path(Value) of
                {true, Path} ->
                    #{term => #{path => Path}};
                false ->
                    term_filter(obj_id, Value)
            end;
        childs_of ->
            case nkdomain_util:is_path(Value) of
                {true, <<"/">>} ->
                    #{wildcard => #{path => <<"/?*">>}};
                {true, Path} ->
                    #{prefix => #{path => <<Path/binary, $/>>}};
                false ->
                    {error, invalid_path}
            end;
        _ ->
            term_filter(Field, Value)
    end,
    get_filters(Rest, [Filter|Acc]).


%% @private
term_filter(Key, {none, To}) ->
    #{range => #{Key => #{lte=>To}}};
term_filter(Key, {From, none}) ->
    #{range => #{Key => #{gte=>From}}};
term_filter(Key, {From, To}) ->
    #{range => #{Key => #{gte=>From, lte=>To}}};
term_filter(Key, Id) when is_atom(Id); is_binary(Id) ->
    #{term => #{Key => Id}};
term_filter(Key, [Num|_]=List) when is_integer(Num) ->
    term_filter(Key, list_to_binary(List));
term_filter(Key, List) when is_list(List) ->
    #{terms => #{Key => [nklib_util:to_binary(Val) || Val<-List]}}.


%% @private
query_filter([Single]) ->
    #{
        constant_score => #{
            filter => Single
        }
    };

query_filter(Filter) ->
    #{
        bool => #{                          % we want several filters
            filter => Filter
        }
    }.


%%%% @private
%%query_id(Id) ->
%%    Filter = case nkdomain_util:is_path(Id) of
%%        {true, Path} ->
%%            #{term => #{path => Path}};
%%        false ->
%%            #{term => #{obj_id => Id}}
%%    end,
%%    query_filter(Filter).


%% @private
do_search_objs(SrvId, Query, Opts) ->
    Opts2 = Opts#{
        fields => [<<"type">>, <<"path">>]
    },
    case do_search(SrvId, Query, Opts2) of
        {ok, N, Data, _Aggs, _Meta} ->
            Data2 = lists:map(
                fun(#{<<"obj_id">>:=ObjId, <<"type">>:=Type, <<"path">>:=Path}) ->
                    {Type, ObjId, Path}
                end,
                Data),
            {ok, N, Data2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_search_types(SrvId, Query, Opts) ->
    Opts2 = Opts#{
        aggs => #{
            types => #{
                terms => #{
                    field => type
                }
            }
        },
        size => 0
    },
    case do_search(SrvId, Query, Opts2) of
        {ok, N, [], #{<<"types">>:=Types}, _Meta} ->
            Data = lists:map(
                fun(#{<<"key">>:=Key, <<"doc_count">>:=Count}) -> {Key, Count} end,
                maps:get(<<"buckets">>, Types)),
            {ok, N, Data};
        {ok, 0, [], _Agg, _Meta} ->
            {ok, 0, []};
        {error, Error} ->
            {error, Error}
    end.


%%%% @private
%%do_search(SrvId, Query, Opts) ->
%%    Opts2 = Opts#{
%%        fields => [<<"type">>, <<"path">>]
%%    },
%%    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
%%    case nkelastic_api:search(SrvId, Index, IdxType, Query, Opts2) of
%%        {ok, N, List, Aggs, Meta} ->
%%            Data = lists:map(
%%                fun(#{<<"_id">>:=ObjId, <<"_source">>:=Source}) ->
%%                    #{<<"type">>:=Type, <<"path">>:=Path} = Source,
%%                    {Type, ObjId, Path}
%%                end,
%%                List),
%%            {ok, N, Data, Aggs, Meta};
%%        {error, search_error} ->
%%            {ok, 0, [], #{}, #{error=>search_error}};
%%        {error, Error} ->
%%            ?LLOG(notice, "Error calling search (~p, ~p): ~p", [Query, Opts, Error]),
%%            {ok, 0, [], #{}, #{}}
%%    end.


%% @private
do_search(SrvId, Query, Opts) ->
    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    case nkelastic_api:search(SrvId, Index, IdxType, Query, Opts) of
        {ok, N, List, Aggs, Meta} ->
            Data = lists:map(
                fun(#{<<"_id">>:=ObjId}=D) ->
                    Source = maps:get(<<"_source">>, D, #{}),
                    Source#{<<"obj_id">>=>ObjId}
                end,
                List),
            {ok, N, Data, Aggs, Meta};
        {error, search_error} ->
            {ok, 0, [], #{}, #{error=>search_error}};
        {error, Error} ->
            ?LLOG(notice, "Error calling search (~p, ~p): ~p", [Query, Opts, Error]),
            {ok, 0, [], #{}, #{}}
    end.



%%%% @private
%%delete(SrvId, Query) ->
%%    #es_config{index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
%%    ?LLOG(info, "delete query: ~p", [Query]),
%%    case nkelastic_api:delete_by_query(SrvId, Index, IdxType, Query) of
%%        {ok, #{<<"total">> := Total}} ->
%%            {ok, Total};
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @private
do_search_archive(SrvId, Query, Opts) ->
    #es_config{archive_index=Index, type=IdxType} = SrvId:config_nkdomain_store_es(),
    case nkelastic_api:search(SrvId, <<Index/binary, $*>>, IdxType, Query, Opts) of
        {ok, N, List, Aggs, Meta} ->
            Data = lists:map(
                fun(#{<<"_index">>:=Idx, <<"_id">>:=ObjId}=D) ->
                    Source = maps:get(<<"_source">>, D, #{}),
                    Source#{<<"_store_index">>=>Idx, <<"obj_id">>=>ObjId}
                end,
                List),
            {ok, N, Data, Aggs, Meta};
        {error, search_error} ->
            {ok, 0, [], #{}, #{error=>search_error}};
        {error, Error} ->
            ?LLOG(notice, "Error calling search (~p, ~p): ~p", [Query, Opts, Error]),
            {ok, 0, [], #{}, #{}}
    end.

%% @private
iterate(SrvId, Query, Opts, Fun, Acc0) ->
    #es_config{index = Index, type = IdxType} = SrvId:config_nkdomain_store_es(),
    nkelastic_api:iterate_fun(SrvId, Index, IdxType, Query, Opts, Fun, Acc0).
