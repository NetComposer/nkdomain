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

%% Low level operations
%% {ok, E} = nkdomain_store_es_util:get_opts(SrvId).
%% nkelastic:delete(ObjId, E).



-module(nkdomain_store_es).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([read_obj/2, save_obj/3, delete_obj/2, find_obj/2]).
-export([search_agg_field/5]).
-export([search_types/3, search_all_types/3, search_childs/3, search_all_childs/3, search/2, search_obj_alias/2]).
-export([delete_all_childs/3]).
-export([clean/1, import_objects/3, print/2]).

-define(ES_ITER_SIZE, 100).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").


-type search_spec() :: nkelastic_search:search_spec().
-type meta() :: nkelastic:resp_meta().


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Reads an object
-spec read_obj(nkdomain:obj_id(), nkelastic:opts()) ->
    {ok, map(), meta()} | {error, object_not_found|term()}.

read_obj(ObjId, EsOpts) ->
    case nkelastic:get(ObjId, EsOpts) of
        {ok, Map, Meta} ->
            case Map of
                #{<<"srv_id">>:=<<"nkroot">>} ->
                    %% TODO Temporary hack
                    lager:warning("NKLOG Restoring empty nkroot on disk: ~s", [ObjId]),
                    {ok, _} = nkelastic:put(ObjId, Map#{<<"srv_id">>:=<<>>}, EsOpts);
                _ ->
                    ok
            end,
            case ?CALL_NKROOT(object_es_parse, [Map]) of
                {ok, Obj, []} ->
                    {ok, Obj, Meta};
                {ok, Obj, UnknownFields} ->
                    {ok, Obj, Meta#{unknown_fields=>UnknownFields}};
                {error, Error} ->
                    ?LLOG(warning, "error parsing loaded object ~s: ~p\n~p", [ObjId, Error, Map]),
                    {error, parse_error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Saves an object
-spec save_obj(nkdomain:obj_id(), map(), nkelastic:opts()) ->
    {ok, meta()} | {error, term()}.

save_obj(ObjId, Obj, EsOpts) ->
    Map = ?CALL_NKROOT(object_es_unparse, [Obj]),
    nkelastic:put(ObjId, Map, EsOpts).


%% @doc Removes an object
-spec delete_obj(nkdomain:obj_id(), nkelastic:opts()) ->
    {ok, meta()} | {error, term()}.

delete_obj(ObjId, EsOpts) ->
    case search_childs(ObjId, #{size=>0}, EsOpts) of
        {ok, 0, []} ->
            case nkdomain_lib:find_loaded(ObjId) of
                #obj_id_ext{pid=Pid} ->
                    nkdomain_obj:object_deleted(Pid);
                not_found ->
                    ok
            end,
            nkelastic:delete(ObjId, EsOpts);
        {ok, _, _} ->
            {error, object_has_childs};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds an object from its ID or Path
-spec find_obj(nkdomain:id(), nkelastic:opts()) ->
    {ok, Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

find_obj(Id, EsOpts) ->
    Filters = case nkdomain_util:is_path(Id) of
        {true, Path} ->
            #{path => Path};
        {false, Id2} ->
            #{obj_id => Id2}
    end,
    case do_search_objs(#{filters=>Filters}, EsOpts) of
        {ok, 0, []} ->
            {error, object_not_found};
        {ok, 1, [{Srv, Type, ObjId, ObjPath}]} ->
            {ok, nkdomain_store_es_util:stored_srv(Srv), Type, ObjId, ObjPath};
        {ok, _, [{Srv, Type, ObjId, ObjPath}|_]} ->
            ?LLOG(warning, "Multiple objects for path ~s", [ObjPath]),
            {ok, nkdomain_store_es_util:stored_srv(Srv), Type, ObjId, ObjPath}
    end.


%% @doc Generic search
-spec search(search_spec(), nkelastic:opts()) ->
    {ok, integer(), Data::[map()], Aggs::map(), meta()} | {error, term()}.

search(Spec, EsOpts) ->
    case do_search(Spec, EsOpts) of
        {ok, N, Data, Aggs, Meta} ->
            {ok, N, Data, Aggs, Meta};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds types
-spec search_types(nkdomain:obj_id(), nkdomain:search_spec(), nkelastic:opts()) ->
    {ok, integer(), [{nkdomain:type(), integer()}], meta()} | {error, term()}.

search_types(Id, Spec, EsOpts) ->
    case filter_childs(Id, Spec, EsOpts) of
        {ok, Spec2} ->
            do_search_types(Spec2, EsOpts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all types
-spec search_all_types(nkdomain:id(), nkdomain:search_spec(), nkelastic:opts()) ->
    {ok, integer(), [{nkdomain:type(), integer()}], meta()} | {error, term()}.

search_all_types(Id, Spec, EsOpts) ->
    case filter_all_childs(Id, Spec, EsOpts) of
        {ok, Spec2} ->
            do_search_types(Spec2, EsOpts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec search_childs(nkdomain:id(), search_spec(), nkelastic:opts()) ->
    {ok, integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], meta()} | {error, term()}.

search_childs(Id, Spec, EsOpts) ->
    case filter_childs(Id, Spec, EsOpts) of
        {ok, Spec2} ->
            do_search_objs(Spec2, EsOpts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects on a path
-spec search_all_childs(nkdomain:id(), nkdomain:search_spec(), nkelastic:opts()) ->
    {ok, integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], meta()} | {error, term()}.

search_all_childs(Id, Spec, EsOpts) ->
    case filter_all_childs(Id, Spec, EsOpts) of
        {ok, Spec2} ->
            do_search_objs(Spec2, EsOpts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds types
-spec search_agg_field(nkdomain:obj_id(), binary(), nkdomain:search_spec(), boolean(), nkelastic:opts()) ->
    {ok, integer(), [{nkdomain:type(), integer()}], meta()} | {error, term()}.

search_agg_field(Id, Field, Spec, SubChilds, EsOpts) ->
    case filter_childs(Id, Spec, SubChilds, EsOpts) of
        {ok, Spec2} ->
            Spec3 = Spec2#{
                aggs => #{
                    my_fields => #{
                        terms => #{
                            field => Field,
                            size => maps:get(size, Spec, 1)
                        }
                    }
                },
                size => 0
            },
            case do_search(Spec3, EsOpts) of
                {ok, N, [], #{<<"my_fields">>:=MyFields}, Meta} ->
                    #{
                        <<"buckets">> := Buckets,
                        <<"doc_count_error_upper_bound">> := Error,
                        <<"sum_other_doc_count">> := SumOther
                    } = MyFields,
                    Meta2 = Meta#{
                        agg_error => Error,
                        agg_sum_other => SumOther
                    },
                    Data = lists:map(
                        fun(#{<<"key">>:=Key, <<"doc_count">>:=Count}) -> {Key, Count} end,
                        Buckets),
                    {ok, N, Data, Meta2};
                {ok, 0, [], _Agg, Meta} ->
                    {ok, 0, [], Meta};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds all objects having an alias
-spec search_obj_alias(binary(), nkelastic:opts()) ->
    {ok, integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id()}], meta()} | {error, term()}.

search_obj_alias(Alias, EsOpts) ->
    do_search_objs(#{filters=>#{aliases=>Alias}}, EsOpts).


%%%% @doc Archive an object
%%-spec archive_search(nkdomain:search_spec()) ->
%%    {ok, integer(), list(), map()}.
%%
%%archive_search(Spec) ->
%%    case do_search_archive(Spec) of
%%        {ok, N, Data, _Aggs, Meta} ->
%%            {ok, N, Data, Meta};
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%%% @doc Archive an object
%%archive_save_obj(#{obj_id:=ObjId}=Store) ->
%%    {Index, IdxType} = nkdomain_store_es_callbacks:get_archive_index(SrvId),
%%    {{Y,M,D}, {_H,_Mi,_S}} = calendar:universal_time(),
%%    Index2 = list_to_binary(io_lib:format("~s-~4..0B~2..0B~2..0B", [Index, Y,M,D])),
%%    case nkelastic_api:put(Index2, IdxType, ObjId, Store) of
%%        {ok, _Vsn} ->
%%            ok;
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc Deletes objects
-spec delete_all_childs(nkdomain:id(), nkdomain:search_spec(), nkelastic:opts()) ->
    {ok, integer()} | {error, term()}.

delete_all_childs(Id, Spec, EsOpts) ->
    case filter_all_childs(Id, Spec, EsOpts) of
        {ok, Spec2} ->
            Spec3 = Spec2#{fields=>[<<"path">>], sort=>[#{<<"path">> => #{order=>desc}}]},
            Fun = fun(#{<<"obj_id">>:=ObjId}, Acc) ->
                case nkelastic:delete(ObjId, EsOpts) of
                    {ok, _} ->
                        case nkdomain_lib:find_loaded(ObjId) of
                            #obj_id_ext{pid=Pid} ->
                                nkdomain_obj:object_deleted(Pid);
                            _ ->
                                ok
                        end,
                        {ok, Acc+1};
                    {error, object_not_found} ->
                        {ok, Acc};
                    {error, Error} ->
                        {error, Error}
                end
            end,
            iterate(Spec3, Fun, 0, EsOpts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec clean(nkelastic:opts()) ->
    {ok, map()} | {error, term()}.


clean(EsOpts) ->
    case do_clean_active(EsOpts) of
        {ok, Active} ->
            case do_clean_expired(EsOpts) of
                {ok, Inactive} ->
                    {ok, maps:merge(Active, Inactive)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
        end.


%% @private
do_clean_active(EsOpts) ->
    Spec = #{
        filters => #{active=>true},
        size => ?ES_ITER_SIZE,
        fields => [<<"type">>]
    },
    Fun = fun(#{<<"obj_id">>:=ObjId, <<"type">>:=Type}, Acc) ->
        case ?CALL_NKROOT(object_check_active, [Type, ObjId]) of
            false ->
                {ok, Acc+1};
            true ->
                {ok, Acc}
        end
    end,
    case iterate(Spec, Fun, 0, EsOpts) of
        {ok, 0} -> {ok, #{}};
        {ok, N} -> {ok, #{inactive=>N}};
        {error, Error} -> {error, Error}
    end.


%% @private
do_clean_expired(EsOpts) ->
    Time = nkdomain_util:timestamp() + 10000,
    Spec = #{
        filters => #{expires_time => <<"<", (to_bin(Time))/binary>>},
        size => ?ES_ITER_SIZE
    },
    Fun = fun(#{<<"obj_id">>:=ObjId}, Acc) ->
        ?CALL_NKROOT(object_do_expired, [ObjId]),
        {ok, Acc+1}
    end,
    case iterate(Spec, Fun, 0, EsOpts) of
        {ok, 0} -> {ok, #{}};
        {ok, N} -> {ok, #{expired=>N}};
        {error, Error} -> {error, Error}
    end.


%% @doc
import_objects(FromIndex, StartPath, UserFun) ->
    {ok, To} = nkdomain_store_es_util:get_opts(),
    From = To#{index:=FromIndex},
    Spec = #{
        size => ?ES_ITER_SIZE,
        sort => <<"path">>,
        filters => #{path => <<"prefix:", (to_bin(StartPath))/binary>>}
    },
    Fun = fun(#{<<"obj_id">>:=ObjId, <<"path">>:=Path}=Data, Acc) ->
        case UserFun(Data) of
            continue ->
                lager:info("Import skipping ~s (~s)", [Path, ObjId]),
                {ok, Acc};
            {upgrade, Data2} ->
                case nkelastic:get(ObjId, To) of
                    {ok, Data2, _} ->
                        lager:info("Import already imported ~s (~s)", [Path, ObjId]),
                        {ok, Acc};
                    _ ->
                        case do_search_objs(#{filters=>#{path=>Path}}, To) of
                            {ok, 1, _List} ->
                              lager:warning("Import COULD NOT upgrade ~s (~s): path is present", [Path, ObjId]),
                                {ok, Acc};
                            {ok, 2, _List} ->
                                lager:warning("Import COULD NOT upgrade ~s (~s): path is DUPLICATED", [Path, ObjId]),
                                {ok, Acc};
                            {ok, 0, []} ->
                                case nkelastic:put(ObjId, Data2, To) of
                                    {ok, _} ->
                                        lager:notice("Import upgraded ~s (~s)", [Path, ObjId]),
                                        timer:sleep(10),
                                        %print("Old", Data),
                                        %print("New", Data2),
                                        {ok, Acc+1};
                                    {error, Error} ->
                                        lager:warning("Import COULD NOT upgrade ~s (~s): ~p", [Path, ObjId, Error]),
                                        {ok, Acc}
                                end
                        end
                end
        end
    end,
    iterate(Spec, Fun, 0, From).


%% @private
print(Txt, Obj) ->
    io:format("~s:\n~s\n\n", [Txt, nklib_json:encode_pretty(Obj)]).



%% ===================================================================
%% Util
%% ===================================================================


%% @private
-spec do_search_objs(search_spec(), nkelastic:opts()) ->
    {ok, N::integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]} | {error, term()}.

do_search_objs(Spec, EsOpts) ->
    Spec2 = Spec#{
        fields => [<<"srv_id">>, <<"type">>, <<"path">>]
    },
    case do_search(Spec2, EsOpts) of
        {ok, N, Data, _Aggs, _Meta} ->
            Data2 = lists:map(
                fun(#{<<"obj_id">>:=ObjId, <<"srv_id">>:=SrvId, <<"type">>:=Type, <<"path">>:=Path}) ->
                    {nkdomain_store_es_util:stored_srv(SrvId), Type, ObjId, Path}
                end,
                Data),
            {ok, N, Data2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_search_types(Spec, EsOpts) ->
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
    case do_search(Spec2, EsOpts) of
        {ok, N, [], #{<<"types">>:=Types}, Meta} ->
            Data = lists:map(
                fun(#{<<"key">>:=Key, <<"doc_count">>:=Count}) -> {Key, Count} end,
                maps:get(<<"buckets">>, Types)),
            {ok, N, Data, Meta};
        {ok, 0, [], _Agg, Meta} ->
            {ok, 0, [], Meta};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_search(Spec, EsOpts) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            case nkelastic:search(Query, EsOpts) of
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


%%%% @private
%%do_search_archive(Spec) ->
%%    case nkelastic_search:query(Spec) of
%%        {ok, Query} ->
%%            {Index, IdxType} = nkdomain_store_es_callbacks:get_archive_index(SrvId),
%%            case nkelastic_api:search(<<Index/binary, $*>>, IdxType, Query) of
%%                {ok, N, List, Aggs, Meta} ->
%%                    Data = lists:map(
%%                        fun(#{<<"_index">>:=Idx, <<"_id">>:=ObjId}=D) ->
%%                            Source = maps:get(<<"_source">>, D, #{}),
%%                            Source#{<<"_store_index">>=>Idx, <<"obj_id">>=>ObjId}
%%                        end,
%%                        List),
%%                    {ok, N, Data, Aggs, Meta};
%%                {error, search_error} ->
%%                    {ok, 0, [], #{}, #{error=>search_error}};
%%                {error, Error} ->
%%                    ?LLOG(notice, "Error calling search (~p, ~p): ~p", [Query, Spec, Error]),
%%                    {ok, 0, [], #{}, #{}}
%%            end;
%%        {error, Error} ->
%%            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
%%            {error, internal_error}
%%    end.



%% @private
iterate(Spec, Fun, Acc0, EsOpts) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            nkelastic:iterate_fun(Query, Fun, Acc0, EsOpts);
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, internal_error}
    end.



%% @private
filter_childs(Id, Spec, true, EsOpts) ->
    filter_all_childs(Id, Spec, EsOpts);

filter_childs(Id, Spec, false, EsOpts) ->
    filter_childs(Id, Spec, EsOpts).


%% @private
filter_childs(Id, Spec, EsOpts) ->
    case nkdomain_util:is_path(Id) of
        {false, Id2} ->
            {ok, parent_filter(Id2, Spec)};
        {true, Path} ->
            case find_obj(Path, EsOpts) of
                {ok, _Srv, _Type, ObjId, _Path} ->
                    {ok, parent_filter(ObjId, Spec)};
                {error, _} ->
                    {error, object_not_found}
            end
    end.


%% @private
parent_filter(Id, Spec) ->
    Filters1 = maps:get(filters, Spec, #{}),
    Filters2 = Filters1#{domain_id=>Id},
    Spec#{filters=>Filters2}.


%% @private
filter_all_childs(Id, Spec, EsOpts) ->
    case nkdomain_util:is_path(Id) of
        {true, Path} ->
            {ok, path_filter(Path, Spec)};
        {false, Id2} ->
            case find_obj(Id2, EsOpts) of
                {ok, _Srv, _Type, _ObjId, Path} ->
                    {ok, path_filter(Path, Spec)};
                {error, _} ->
                    {error, object_not_found}
            end
    end.


%% @private
path_filter(Path, Spec) ->
    Filters1 = maps:get(filters, Spec, #{}),
    Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
    Spec#{filters=>Filters2}.




%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
