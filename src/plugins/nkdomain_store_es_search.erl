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
-module(nkdomain_store_es_search).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([find_obj/3, search_objs/3, search_agg_objs/4, iterate_objs/5]).
-export([search/2, delete_objs/3]).
-export([clean/1, import_objects/3, print/2]).

-define(ES_ITER_SIZE, 100).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-type search_spec() :: nkelastic_search:search_spec().
-type search_objs_opts() :: nkdomain_db:search_objs_opts().
-type iter_fun() :: nkdomain_db:iter_fun().
-type meta() :: nkelastic:resp_meta().


%% ===================================================================
%% Public
%% ===================================================================



%% @doc Finds an object from its ID or Path
-spec find_obj(nkdomain:id(), FindDeleted::boolean(), nkelastic:opts()) ->
    {ok, Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

find_obj(Id, FindDeleted, EsOpts) ->
    Base = case FindDeleted of
        true ->
            [];
        false ->
            [{'not', {is_deleted, eq, true}}]
    end,
    Filters = case nkdomain_util:is_path(Id) of
        {true, Path} ->
            [{path, eq, Path}|Base];
        {false, Id2} ->
            [{obj_id, eq, Id2}|Base]
    end,
    case do_search_objs(#{filter_list=>Filters, fields=>[<<"obj_id">>, <<"type">>, <<"path">>]}, EsOpts) of
        {ok, 0, [], _Meta} ->
            {error, object_not_found};
        {ok, 1, [#{<<"type">>:=Type, <<"obj_id">>:=ObjId, <<"path">>:=ObjPath}], _Meta} ->
            {ok, Type, ObjId, ObjPath};
        {ok, _, [#{<<"type">>:=Type, <<"obj_id">>:=ObjId, <<"path">>:=ObjPath}|_], _Meta} ->
            ?LLOG(warning, "Multiple objects for path ~s", [ObjPath]),
            {ok, Type, ObjId, ObjPath}
    end.


%% @doc Generic search Called from domain/find and domain/find_all
-spec search(search_spec(), nkelastic:opts()) ->
    {ok, integer(), Data::[map()], Aggs::map(), meta()} | {error, term()}.

search(Spec, EsOpts) ->
    case do_search(Spec, EsOpts) of
        {ok, N, Data, Aggs, Meta} ->
            {ok, N, Data, Aggs, Meta};
        {error, Error} ->
            {error, Error}
    end.



%% @doc Generic search
-spec search_objs(nkelastic_search:filter_list(), search_objs_opts(), nkelastic:opts()) ->
    {ok, integer(), [RawObj::map()], meta()} | {error, term()}.

search_objs(Filters, Opts, EsOpts) ->
    Filters2 = get_filters(Filters, Opts),
    Fields1 = maps:get(fields, Opts, []),
    Fields2 = nklib_util:store_values([<<"obj_id">>, <<"type">>, <<"path">>], Fields1),
    Spec1 = maps:with([from, size, sort], Opts),
    Spec2 = Spec1#{filter_list=>Filters2, fields=>Fields2},
    do_search_objs(Spec2, EsOpts).



%% @doc Generic delete
-spec delete_objs(nkelastic_search:filter_list(), search_objs_opts(), nkelastic:opts()) ->
    {ok, integer(), Aggs::map(), meta()} | {error, term()}.

delete_objs(Filters, Opts, EsOpts) ->
    Filters2 = get_filters(Filters, Opts),
    Spec = #{filter_list=>Filters2},
    do_delete(Spec, EsOpts).



%% @doc Finds types
-spec search_agg_objs(nkelastic_search:filter_list(), binary(), search_objs_opts(), nkelastic:opts()) ->
    {ok, integer(), [{binary(), integer()}], meta()} | {error, term()}.

search_agg_objs(Filters, Field, Opts, EsOpts) ->
    Filters2 = get_filters(Filters, Opts),
    Size = maps:get(size, Opts, 100),
    TermsBase = maps:with([include, min_doc_count], Opts),
    Fields = case is_list(Field) of
        true ->
            case io_lib:printable_unicode_list(Field) of
                true ->
                    [to_bin(Field)];
                false ->
                    Field
            end;
        false ->
            [to_bin(Field)]
    end,
    AggsList = lists:foldr(fun(F, AggAcc) ->
        Include = maps:get(include, TermsBase, undefined),
        TermsBase2 = case Include of
            undefined ->
                TermsBase;
            _ ->
                case is_map(Include) andalso maps:is_key(F, Include) of
                    true ->
                        TermsBase#{
                            include => maps:get(F, Include, [])
                        };
                    false ->
                        maps:without([include], TermsBase)
                end
        end,
        [{F, #{
            terms => TermsBase2#{
                field => F,
                size => Size
            }
        }}|AggAcc]
    end,
    [],
    Fields),
    Spec = #{
        filter_list => Filters2,
        aggs => maps:from_list(AggsList),
        size => 0
    },
    case do_search(Spec, EsOpts) of
        {ok, N, [], Results, Meta} ->
            {Data, AggError, AggSumOther} = lists:foldr(fun(F, {Acc, AccError, AccSumOther}) ->
                MyFields = maps:get(F, Results),
                #{
                    <<"buckets">> := Buckets,
                    <<"doc_count_error_upper_bound">> := Error,
                    <<"sum_other_doc_count">> := SumOther
                } = MyFields,
                {Acc#{F => lists:map(
                    fun(#{<<"key">>:=Key, <<"doc_count">>:=Count}) -> {Key, Count} end,
                Buckets)},
                [Error|AccError],
                [SumOther|AccSumOther]}
            end,
            {#{}, [], []},
            Fields),
            {Data2, Meta2} = case Fields of
                [F|[]] ->
                    {maps:get(F, Data),
                    Meta#{
                        agg_error => lists:nth(1, AggError),
                        agg_sum_other => lists:nth(1, AggSumOther)
                    }};
                _ ->
                    {Data, Meta#{
                        agg_error => AggError,
                        agg_sum_other => AggSumOther
                    }}
            end,
            {ok, N, Data2, Meta2};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Generic search
-spec iterate_objs(nkelastic_search:filter_list(), search_objs_opts(), iter_fun(), term(), nkelastic:opts()) ->
    {ok, term()}| {error, term()}.

iterate_objs(Filters, Opts, Fun, Acc0, EsOpts) ->
    Filters2 = get_filters(Filters, Opts),
    Fields1 = maps:get(fields, Opts, []),
    Fields2 = nklib_util:store_values([<<"obj_id">>, <<"type">>, <<"path">>], Fields1),
    Spec1 = maps:with([from, size, sort], Opts),
    Spec2 = Spec1#{filter_list=>Filters2, fields=>Fields2},
    do_iterate(Spec2, Fun, Acc0, EsOpts).


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
        filter_list => [{active, eq, true}, {'not', {is_deleted, eq, true}}],
        size => ?ES_ITER_SIZE,
        fields => [<<"type">>]
    },
    Fun = fun(#{<<"obj_id">>:=ObjId, <<"type">>:=Type}, Acc) ->
        case ?CALL_NKROOT(object_do_active, [Type, ObjId]) of
            ok ->
                {ok, Acc};
            _ ->
                {ok, Acc+1}
        end
    end,
    case do_iterate(Spec, Fun, 0, EsOpts) of
        {ok, 0} -> {ok, #{}};
        {ok, N} -> {ok, #{inactive=>N}};
        {error, Error} -> {error, Error}
    end.


%% @private
do_clean_expired(EsOpts) ->
    Time = nkdomain_util:timestamp() + 10000,
    Spec = #{
        filter_list => [{expires_time, lt, Time}, {'not', {is_deleted, eq, true}}],
        size => ?ES_ITER_SIZE
    },
    Fun = fun(#{<<"obj_id">>:=ObjId}, Acc) ->
        ?CALL_NKROOT(object_do_expired, [ObjId]),
        {ok, Acc+1}
    end,
    case do_iterate(Spec, Fun, 0, EsOpts) of
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
        filter_list => [{path, prefix, StartPath}]
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
                        case do_search_objs(#{filter_list=>[{path, eq, Path}], size=>0}, To) of
                            {ok, 1, _List, _Meta} ->
                              lager:warning("Import COULD NOT upgrade ~s (~s): path is present", [Path, ObjId]),
                                {ok, Acc};
                            {ok, 2, _List, _Meta} ->
                                lager:warning("Import COULD NOT upgrade ~s (~s): path is DUPLICATED", [Path, ObjId]),
                                {ok, Acc};
                            {ok, 0, [], _Meta} ->
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
    do_iterate(Spec, Fun, 0, From).


%% @private
print(Txt, Obj) ->
    io:format("~s:\n~s\n\n", [Txt, nklib_json:encode_pretty(Obj)]).



%% ===================================================================
%% Util
%% ===================================================================


%% @private
-spec do_search_objs(search_spec(), nkelastic:opts()) ->
    {ok, N::integer(), [#{binary() => term()}], Meta::map()} | {error, term()}.

do_search_objs(Spec, EsOpts) ->
    case do_search(Spec, EsOpts) of
        {ok, N, Data, _Aggs, Meta} ->
            {ok, N, Data, Meta};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_iterate(Spec, Fun, Acc0, EsOpts) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            nkelastic:iterate_fun(Query, Fun, Acc0, EsOpts);
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, internal_error}
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
                {error, {search_error, _}} ->
                    {ok, 0, [], #{}, #{error=>search_error}};
                {error, Error} ->
                    ?LLOG(notice, "Error calling search (~p): ~p", [Query, Error]),
                    {ok, 0, [], #{}, #{}}
            end;
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, Error}
    end.


%% @private
do_delete(Spec, EsOpts) ->
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            case nkelastic:delete_by_query(Query, EsOpts) of
                {ok, Aggs, Meta} ->
                    {ok, maps:get(<<"total">>, Aggs, 0), Aggs, Meta};
                {error, {search_error, _}} ->
                    {ok, 0, #{}, #{error=>search_error}};
                {error, Error} ->
                    ?LLOG(notice, "Error calling search (~p): ~p", [Query, Error]),
                    {ok, 0, #{}, #{}}
            end;
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, Error}
    end.


%% @private
get_filters(Filters, Opts) ->
    Filters2 = case Opts of
        #{get_deleted:=true} ->
            Filters;
        _ ->
            [{'not', {is_deleted, eq, true}}|Filters]
    end,
    Filters3 = case Opts of
        #{type:=Type} ->
            [{type, eq, to_bin(Type)}|Filters2];
        _ ->
            Filters2
    end,
    Filters4 = case Opts of
        #{all_tags:=Tags1} ->
            [{tags, eq, Tag}||Tag <- Tags1]++Filters3;
        _ ->
            Filters3
    end,
    Filters5 = case Opts of
        #{some_tags:=Tags2} ->
            [{tags, values, Tags2}|Filters4];
        _ ->
            Filters4
    end,
    Filters5.



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
