%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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
%%ee
%% -------------------------------------------------------------------

%% @doc NkDomain GraphQL search utilities
-module(nkdomain_graphql_search).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_uid/2, search/3]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_uid(SrvId, UID) ->
    Req = #{group=>?GROUP_CORE, vsn=>?GROUP_CORE_V1A1, resource=><<"_uids">>, name=>UID},
    case nkdomain_api:request(SrvId, Req) of
        {ok, Actor, _} ->
            nkdomain_graphql_schema:make_object(SrvId, {actor, Actor});
        {error, #{<<"reason">>:=Reason, <<"message">>:=Msg}, _} ->
            {error, #{reason=>Reason, message=>Msg}}
    end.

%% @doc Search options
%% - apiGroup and kind are used to filter
%%   If both are present, specific filtering and sorting can be used
%% - domain, if present, takes priority
%% - pagination options in search_spec take priority
%%   and filtering and sorting are added to query
-type search_opts() ::
    #{
        apiGroup => nkdomain_api:group(),
        kind => nkdomain_api:kind(),
        domain => nkdomain_api:domain(),
        search_spec => nkservice_actor_search:search_spec(),
        filter_fun =>
            fun((nkservice:id(), Field::binary(), Op::atom(), Val::term(), list())->list()),
        sort_fun =>
            fun((nkservice:id(), Field::binary(), Order::atom(), list())->list())
    }.


%% @doc
-spec search(nkservice:id(), map(), search_opts()) ->
    {ok, map()} | {error, term()}.

search(SrvId, QueryParams, SearchOpts) ->
    %lager:notice("NKLOG Search Params ~p ~p   ", [QueryParams, SearchOpts]),
    try
        % Make filters
        BaseSearchSpec = maps:get(search_spec, SearchOpts, #{}),
        BaseFilter = maps:get(filter, BaseSearchSpec, #{}),
        BaseAnd = maps:get('and', BaseFilter, []),
        BaseOr = maps:get('or', BaseFilter, []),
        BaseNot = maps:get('not', BaseFilter, []),
        QueryFilters = case maps:get(<<"filter">>, QueryParams, null) of
            null -> #{};
            QueryFiltersMap -> QueryFiltersMap
        end,
        QueryAnd = maps:get(<<"and">>, QueryFilters, null),
        QueryOr = maps:get(<<"or">>, QueryFilters, null),
        QueryNot = maps:get(<<"not">>, QueryFilters, null),
        FilterFun = maps:get(filter_fun, SearchOpts, none),
        Filter = #{
            'and' => gen_filters(SrvId, QueryAnd, FilterFun, BaseAnd),
            'or' => gen_filters(SrvId, QueryOr, FilterFun, BaseOr),
            'not' => gen_filters(SrvId, QueryNot, FilterFun, BaseNot)
        },
        % Make sort and pagination
        % If any of Keys is present in BaseSearchSpec, it is added
        % to Search1. If not, but it is present in QueryParams (as binary),
        % it is added from there.
        Keys = [deep, from, size, last, first],
        Search1 = apply_params(Keys, BaseSearchSpec, QueryParams, #{}),
        {Search2, PagSort} = apply_pagination(Search1),
        QuerySort = maps:get(<<"sort">>, QueryParams, null),
        SortFun = maps:get(sort_fun, SearchOpts, none),
        BaseSort = maps:get(sort, BaseSearchSpec, []),
        Sort = gen_sort(SrvId, QuerySort, SortFun, BaseSort) ++ PagSort,
        Search3 = Search2#{
            filter => Filter,
            sort => Sort
        },
        Search4 = maps:merge(Search3, maps:with([apiGroup, kind], SearchOpts)),
        Domain = case maps:find(domain, SearchOpts) of
            {ok, BaseDomain} ->
                BaseDomain;
            error ->
                maps:get(<<"domain">>, QueryParams, ?ROOT_DOMAIN)
        end,
        search_api(SrvId, Domain, Search4)
    catch
        throw:Throw ->
            Throw
    end.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
search_api(SrvId, Domain, SearchSpec) ->
    ApiReq2 = #{
        group => ?GROUP_SEARCH,
        vsn => ?GROUP_CORE_V1A1,
        domain => Domain,
        verb => list,
        body => SearchSpec
    },
    %lager:notice("NKLOG SEARCH ~p", [ApiReq2]),
    case nkdomain_api:request(SrvId, ApiReq2) of
        {ok, #{<<"items">>:=Items, <<"metadata">>:=#{<<"total">>:=Total}}, _} ->
            Actors = lists:map(
                fun(Actor) ->
                    nkdomain_graphql_schema:make_object(SrvId, {actor, Actor})
                end,
                Items),
            Result = #{
                <<"actors">> => Actors,
                <<"totalCount">> => Total
            },
            %lager:notice("NKLOG SEARCH RESULT ~p", [Actors]),
            {ok, Result};
        {error, #{<<"reason">>:=Reason, <<"message">>:=Msg}, _} ->
            {error, #{reason=>Reason, message=>Msg}}
    end.


%% @doc
apply_params([], _BaseSearch, _QueryParams, Acc) ->
    Acc;

apply_params([Key|Rest], BaseSearch, QueryParams, Acc) ->
    Acc2 = case maps:find(Key, BaseSearch) of
        {ok, BaseParam} ->
            Acc#{Key => BaseParam};
        error ->
            case maps:get(to_bin(Key), QueryParams, null) of
                null ->
                    Acc;
                QueryParam ->
                    Acc#{Key=>QueryParam}
            end
    end,
    apply_params(Rest, BaseSearch, QueryParams, Acc2).


%% @private
apply_pagination(#{first:=First}=Search) ->
    case
        maps:is_key(last, Search) orelse
        maps:is_key(from, Search) orelse
        maps:is_key(size, Search)
    of
        true ->
            throw({error, {field_invalid, <<"first">>}});
        false ->
            {
                maps:remove(first, Search#{from=>0, size=>First}),
                [#{field=><<"metadata.updateTime">>, order=>asc}]
            }
    end;

apply_pagination(#{last:=Last}=Search) ->
    case
        maps:is_key(first, Search) orelse
        maps:is_key(from, Search) orelse
        maps:is_key(size, Search)
    of
        true ->
            throw({error, {field_invalid, <<"last">>}});
        false ->
            {
                maps:remove(last, Search#{from=>0, size=>Last}),
                [#{field=><<"metadata.updateTime">>, order=>desc}]
            }
    end;

apply_pagination(Search) ->
    case
        maps:is_key(from, Search) orelse
        maps:is_key(size, Search) orelse
        maps:is_key(sort, Search)
    of
        true ->
            {Search, []};
        false ->
            apply_pagination(Search#{last=>10})
    end.


%% @private
gen_filters(_SrvId, null, _FilterFun, Acc) ->
    Acc;

gen_filters(_SrvId, [], _FilterFun, Acc) ->
    Acc;

gen_filters(SrvId, [Filter|Rest], FilterFun, Acc) ->
    Acc2 = make_filter(SrvId, maps:to_list(Filter), FilterFun, Acc),
    gen_filters(SrvId, Rest, FilterFun, Acc2).


%% @private
make_filter(_SrvId, [], _FilterFun, Acc) ->
    Acc;

make_filter(SrvId, [{_Field, null}|Rest], FilterFun, Acc) ->
    make_filter(SrvId, Rest, FilterFun, Acc);

make_filter(SrvId, [{<<"metadata.fts">>, #{<<"word">>:=Word}=Map}|Rest], FilterFun, Acc) ->
    Field1 = case maps:get(<<"field">>, Map, null) of
        null -> <<"*">>;
        MapField when is_binary(MapField) -> MapField;
        _ -> throw({error, {field_invalid, <<"metadata.fts">>}}) % Can be enum
    end,
    Field2 = <<"metadata.fts.", Field1/binary>>,
    Filter = case binary:split(Word, <<"*">>) of
        [Word2, <<>>] ->
            #{field=>Field2, op=>prefix, value=>Word2};
        _ ->
            #{field=>Field2, op=>eq, value=>Word}
    end,
    make_filter(SrvId, Rest, FilterFun, [Filter|Acc]);

make_filter(SrvId, [{Field, Map}|Rest], FilterFun, Acc) when is_map(Map) ->
    FilterOpts = [
        <<"eq">>, <<"values">>, <<"gt">>, <<"gte">>, <<"lt">>, <<"lte">>,
        <<"prefix">>, <<"exists">>
    ],
    case maps:with(FilterOpts, Map) of
        FilterMap when map_size(FilterMap) > 0 ->
            % It is an operation
            Field2 = case maps:get(<<"key">>, Map, null) of
                null ->
                    Field;
                Key when is_binary(Key) ->
                    % It is a labels or links field, with a key
                    <<Field/binary, $., Key/binary>>;
                _ ->
                    throw({error, {field_invald, Field}})  % Can be enum

            end,
            Acc2 = make_filter(SrvId, Field2, maps:to_list(FilterMap), FilterFun, []),
            make_filter(SrvId, Rest, FilterFun, Acc++Acc2);
        _ ->
            % It is an nested field
            List = [
                {<<Field/binary, $., Field2/binary>>, Value}
                || {Field2, Value} <- maps:to_list(Map), Value /= null
            ],
            make_filter(SrvId, List++Rest, FilterFun, Acc)
    end.


%% @private
make_filter(SrvId, Field, [], FilterFun, []) ->
    make_filter(SrvId, Field, [{<<"exists">>, true}], FilterFun, []);

make_filter(_SrvId, _Field, [], _FilterFun, Acc) ->
    Acc;

make_filter(SrvId, Field, [{_Op, null}|Rest], FilterFun, Acc) ->
    make_filter(SrvId, Field, Rest, FilterFun, Acc);

make_filter(SrvId, <<"id">>, List, FilterFun, Acc) ->
    make_filter(SrvId, <<"uid">>, List, FilterFun, Acc);

make_filter(SrvId, Field, [{Op, Val}|Rest], FilterFun, Acc) ->
    Op2 = binary_to_existing_atom(Op, latin1),
    Value2 = remove_enum(Val),
    Acc2 =  case is_function(FilterFun, 5) of
        true ->
            FilterFun(SrvId, Field, Op2, Value2, Acc);
        false ->
            Filter = #{field => Field, op => Op2, value => Value2},
            [Filter|Acc]
    end,
    make_filter(SrvId, Field, Rest, FilterFun, Acc2).


%% @private
remove_enum(Val) ->
    case Val of
        {enum, Enum} ->
            Enum;
        _ when is_list(Val) ->
            [case V of {enum, Enum} -> Enum; _ -> V end || V <- Val];
        _ ->
            Val
    end.


%% @private
gen_sort(_SrvId, null, _Meta, Acc) ->
    Acc;

gen_sort(_SrvId, [], _Meta, Acc) ->
    Acc;

gen_sort(SrvId, [List|Rest], Meta, Acc) ->
    Acc2 = Acc ++ make_sort(SrvId, maps:to_list(List), Meta, []),
    gen_sort(SrvId, Rest, Meta, Acc2).


%% @private
make_sort(_SrvId, [], _SortFun, Acc) ->
    Acc;

make_sort(SrvId, [{_Field, null}|Rest], SortFun, Acc) ->
    make_sort(SrvId, Rest, SortFun, Acc);

make_sort(SrvId, [{Field, #{<<"order">>:={enum, Order}}}|Rest], SortFun, Acc) ->
    Order2 = case Order of
        <<"ASC">> -> asc;
        <<"DESC">> -> desc
    end,
    % We are not annotating fields with the type, it will be done later in the
    % API processor (sort on integer fields will not work)
    Acc2 = case is_function(SortFun, 4) of
        true ->
            SortFun(SrvId, Field, Order2, Acc);
        false when Field==<<"type">> ->
            [
                #{field=><<"group">>, order=>Order2},
                #{field=><<"kind">>, order=>Order2}
                | Acc
            ];
        false ->
            [#{field=>Field, order=>Order2}|Acc]
    end,
    make_sort(SrvId, Rest, SortFun, Acc2);

make_sort(SrvId, [{Field, Map}|Rest], SortFun, Acc) ->
    List = [
        {<<Field/binary, $., Field2/binary>>, Value}
        || {Field2, Value} <- maps:to_list(Map), Value /= null
    ],
    make_sort(SrvId, List++Rest, SortFun, Acc).


%%%% @private
%%read_objs(SrvId, From, Size, Filters, Sort) ->
%%    do_read_objs(SrvId, From, Size, Filters, Sort, []).
%%
%%
%%%% @private
%%do_read_objs(SrvId, Start, Size, Filters, Sort, Acc) ->
%%    Params = #{from=>Start, size=>Size, filter=>Filters, sort=>Sort},
%%    case nkservice_actor_db:search(SrvId, {service_search_actors, SrvId, Params}) of
%%        {ok, [], #{total:=Total}} ->
%%            {ok, Total, lists:reverse(Acc)};
%%        {ok, Data, #{total:=Total}} ->
%%            Data2 = [{ok, {actor, D}} || D <- Data] ++ Acc,
%%            case length(Data2) of
%%                Size ->
%%                    {ok, Total, lists:reverse(Data2)};
%%                Records when Records > Size ->
%%                    {ok, Total, lists:sublist(lists:reverse(Data2), Size)};
%%                Length when Total > Length ->
%%                    do_read_objs(SrvId, Start+Size, Size, Filters, Sort, Data2);
%%                _Length ->
%%                    {ok, Total, lists:reverse(Data2)}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).


