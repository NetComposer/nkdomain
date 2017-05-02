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

-export([read_obj/2, save_obj/2, delete_obj/2, find_obj/2]).
-export([find_types/3, find_all_types/3, find_childs/3, find_all_childs/3, find/2, find_obj_alias/2]).
-export([delete_all_childs/3]).
-export([archive_save_obj/2, archive_find/2]).
-export([clean/1, reload_types/1, remove_index/1]).
-export([get_indices/1, get_aliases/2]).

-define(ES_ITER_SIZE, 100).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Reads an object
read_obj(SrvId, ObjId) ->
    {Index, IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
    case nkelastic_api:get(SrvId, Index, IdxType, ObjId) of
        {ok, Data, Vsn} ->
            {ok, Data#{'_store_vsn'=>Vsn}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Saves an object
save_obj(SrvId, #{obj_id:=ObjId}=Store) ->
    {Index, IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
    nkelastic_api:put_and_wait(SrvId, Index, IdxType, ObjId, Store).


%% @doc Removes an object
delete_obj(SrvId, ObjId) ->
    {Index, IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
    case find_childs(SrvId, ObjId, #{size=>0}) of
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
    Filters = case nkdomain_util:is_path(Id) of
        {true, Path} ->
            #{path => Path};
        false ->
            #{obj_id => to_bin(Id)}
    end,
    case do_search_objs(SrvId, #{filters=>Filters}) of
        {ok, 0, []} ->
            {error, object_not_found};
        {ok, 1, [{Type, ObjId, ObjPath}]} ->
            {ok, Type, ObjId, ObjPath};
        {ok, _, [{Type, ObjId, ObjPath}|_]} ->
            ?LLOG(warning, "Multiple objects for path ~s", [ObjPath]),
            {ok, Type, ObjId, ObjPath}
    end.


%% @doc Finds types
-spec find_types(nkservice:id(), nkdomain:obj_id(), nkdomain_store:search_spec()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]}.

find_types(SrvId, ObjId, Spec) ->
    do_search_types(SrvId, filter_parent(ObjId, Spec)).


%% @doc Finds all types
-spec find_all_types(nkservice:id(), nkdomain:path(), nkdomain_store:search_spec()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]}.

find_all_types(SrvId, Path, Spec) ->
    case filter_path(Path, Spec) of
        {ok, Spec2} ->
            do_search_types(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec find_childs(nkservice:id(), nkdomain:obj_id(), nkdomain_store:search_spec()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]}.

find_childs(SrvId, ParentId, Spec) ->
    do_search_objs(SrvId, filter_parent(ParentId, Spec)).


%% @doc Finds all objects on a path
-spec find_all_childs(nkservice:id(), nkdomain:domain(), nkdomain_store:search_spec()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id()}]}.

find_all_childs(SrvId, Path, Spec) ->
    case filter_path(Path, Spec) of
        {ok, Spec2} ->
            do_search_objs(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects having an alias
-spec find_obj_alias(nkservice:id(), binary()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id()}]} |
    {error, object_not_found}.

find_obj_alias(SrvId, Alias) ->
    do_search_objs(SrvId, #{filters=>#{aliases=>Alias}}).


%% @doc Generic find
-spec find(nkservice:id(), nkdomain_store:search_spec()) ->
    {ok, integer(), [map()], map(), map()} |
    {error, term()}.

find(SrvId, Spec) ->
    case do_search(SrvId, Spec) of
        {ok, N, Data, _Aggs, Meta} ->
            {ok, N, Data, Meta};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Archive an object
-spec archive_find(nkservice:id(), nkdomain_store:search_spec()) ->
    {ok, integer(), list(), map()}.

archive_find(SrvId, Spec) ->
    case do_search_archive(SrvId, Spec) of
        {ok, N, Data, _Aggs, Meta} ->
            {ok, N, Data, Meta};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Archive an object
archive_save_obj(SrvId, #{obj_id:=ObjId}=Store) ->
    {Index, IdxType} = nkdomain_store_es_callbacks:get_archive_index(SrvId),
    {{Y,M,D}, {_H,_Mi,_S}} = calendar:universal_time(),
    Index2 = list_to_binary(io_lib:format("~s-~4..0B~2..0B~2..0B", [Index, Y,M,D])),
    case nkelastic_api:put(SrvId, Index2, IdxType, ObjId, Store) of
        {ok, _Vsn} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec delete_all_childs(nkservice:id(), nkdomain:domain(), nkdomain_store:search_spec()) ->
    {ok, integer()} | {error, term()}.

delete_all_childs(SrvId, Path, Spec) ->
    case filter_path(Path, Spec) of
        {ok, Spec2} ->
            Spec3 = Spec2#{fields=>[<<"path">>], sort=>[#{<<"path">> => #{order=>desc}}]},
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
            iterate(SrvId, Spec3, Fun, 0);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Reload new types
%% Types are loaded automatically when root service starts
%% However, they can also be loaded by hand, for example to check errors.
-spec reload_types(nkservice:id()) ->
    ok | {error, term()}.

reload_types(SrvId) ->
    {Index, IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
    Mappings = nkdomain_store_es_callbacks:get_mappings(SrvId, Index),
    lager:info("Mappings: ~s", [nklib_json:encode_pretty(Mappings)]),
    case nkelastic_api:add_mapping(SrvId, Index, IdxType, Mappings) of
        ok ->
            Templates = nkdomain_store_es_callbacks:get_templates(SrvId),
            do_reload_templates(SrvId, maps:to_list(Templates));
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
    {Index, _IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
    nkelastic_api:delete_index(SrvId, Index).


%% @private
clean(SrvId) ->
    case do_clean_active(SrvId) of
        {ok, Active} ->
            case do_clean_expired(SrvId) of
                {ok, Inactive} ->
                    {ok, maps:merge(Active, Inactive)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
        end.


%% @private
do_clean_active(SrvId) ->
    Spec = #{
        filters => #{active=>true},
        size => ?ES_ITER_SIZE,
        fields => [<<"type">>]
    },
    Fun = fun(#{<<"obj_id">>:=ObjId, <<"type">>:=Type}, Acc) ->
        case SrvId:object_check_active(SrvId, Type, ObjId) of
            false ->
                {ok, Acc+1};
            true ->
                {ok, Acc}
        end
    end,
    case iterate(SrvId, Spec, Fun, 0) of
        {ok, 0} -> {ok, #{}};
        {ok, N} -> {ok, #{inactive=>N}};
        {error, Error} -> {error, Error}
    end.


%% @private
do_clean_expired(SrvId) ->
    Time = nklib_util:m_timestamp() + 10000,
    Spec = #{
        filters => #{expires_time => <<"<", (to_bin(Time))/binary>>},
        size => ?ES_ITER_SIZE
    },
    Fun = fun(#{<<"obj_id">>:=ObjId}, Acc) ->
        SrvId:object_do_expired(SrvId, ObjId),
        {ok, Acc+1}
    end,
    case iterate(SrvId, Spec, Fun, 0) of
        {ok, 0} -> {ok, #{}};
        {ok, N} -> {ok, #{expired=>N}};
        {error, Error} -> {error, Error}
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
do_search_objs(SrvId, Spec) ->
    Spec2 = Spec#{
        fields => [<<"type">>, <<"path">>]
    },
    case do_search(SrvId, Spec2) of
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
do_search_types(SrvId, Spec) ->
    Spec2 = Spec#{
        aggs => #{
            types => #{
                terms => #{
                    field => type
                }
            }
        },
        size => 0
    },
    case do_search(SrvId, Spec2) of
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


%% @private
do_search(SrvId, Spec) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            {Index, IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
            case nkelastic_api:search(SrvId, Index, IdxType, Query) of
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
                    ?LLOG(notice, "Error calling search (~p): ~p", [Query, Error]),
                    {ok, 0, [], #{}, #{}}
            end;
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, internal_error}
    end.


%% @private
do_search_archive(SrvId, Spec) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            {Index, IdxType} = nkdomain_store_es_callbacks:get_archive_index(SrvId),
            case nkelastic_api:search(SrvId, <<Index/binary, $*>>, IdxType, Query) of
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
                    ?LLOG(notice, "Error calling search (~p, ~p): ~p", [Query, Spec, Error]),
                    {ok, 0, [], #{}, #{}}
            end;
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, internal_error}
    end.



%% @private
iterate(SrvId, Spec, Fun, Acc0) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            {Index, IdxType} = nkdomain_store_es_callbacks:get_index(SrvId),
            nkelastic_api:iterate_fun(SrvId, Index, IdxType, Query, Fun, Acc0);
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, internal_error}
    end.


%% @private
filter_parent(ParentId, Spec) ->
    Filters1 = maps:get(filters, Spec, #{}),
    Filters2 = Filters1#{parent_id=>ParentId},
    Spec#{filters=>Filters2}.


%% @private
filter_path(Path, Spec) ->
    case nkdomain_util:is_path(Path) of
        {true, Path2} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{path=><<"childs_of:", Path2/binary>>},
            {ok, Spec#{filters=>Filters2}};
        false ->
            {error, invalid_path}
    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
