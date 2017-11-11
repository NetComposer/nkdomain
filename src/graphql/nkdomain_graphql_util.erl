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

%% @doc
-module(nkdomain_graphql_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([search/2, get_obj/1, get_type/1]).

-include("nkdomain.hrl").
-include("nkdomain_graphql.hrl").

%% ===================================================================
%% Public
%% ===================================================================



% Search operations
% - All 'and' filters go together, all must be true
% - All 'not' filters go together, all must be false
% - All 'or' filters go together (even if several are used for a single field)
%   One or more of them must be true

%% @doc 
search(Params, _Ctx) ->
    #{
        <<"from">> := From,
        <<"size">> := Size,
        <<"filter">> := Filter
    } = Params,
    Spec1 = case add_filters(Filter, []) of
        [] ->
            #{};
        FilterList ->
            #{
                filter_list => FilterList
            }
    end,
    Spec2 = case Params of
        #{<<"sort">>:=Sort} ->
            Spec1#{sort=> [
                <<Order/binary, $:, (to_bin(camel_to_erl(Field)))/binary>>
                || #{<<"field">>:={enum, Field}, <<"sortOrder">>:={enum, Order}} <- Sort
            ]};
        _ ->
            Spec1
    end,
    lager:error("Spec2: ~p", [Spec2]),
    case read_objs(From, Size, Spec2) of
        {ok, Total, Data2} ->
            Result = #search_results{
                objects = Data2,
                total_count = Total,
                page_info = #page_info{
                    has_next_page = false,
                    has_previous_page = false
                }
            },
            {ok, Result};
        {error, Error} ->
            {error, Error}
    end.



%% @private
get_obj(null) ->
    {ok, null};

get_obj(<<>>) ->
    get_obj(<<"root">>);

get_obj(ObjId) ->
    case nkdomain_lib:read(ObjId) of
        {ok, #obj_id_ext{}=ObjIdExt, Obj} ->
            {ok, {ObjIdExt, Obj}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_type(#{type:=Type}) ->
    Module = nkdomain_reg:get_type_module(Type),
    case Module:object_info() of
        #{schema_type:=SchemaType} ->
            {ok, nklib_util:to_binary(SchemaType)};
        _ ->
            lager:error("NKLOG Unknown type ~p", [Type]),
            {error, unknown_type}
    end;

get_type(Obj) ->
    lager:error("NKLOG Unknown type ~p", [Obj]),
    {error, unknown_type}.



%% ===================================================================
%% Internal
%% ===================================================================



%% @private
add_filters([], Acc) ->
    Acc;

add_filters([Filter|Rest], Acc) ->
    {Op, Spec} = do_add_filter(maps:to_list(Filter), 'and', []),
    add_filters(Rest, [{Op, Spec}|Acc]).


%% @private
do_add_filter([], Op, Acc) ->
    {Op, Acc};

do_add_filter([{_Field, null}|Rest], Op, Acc) ->
    do_add_filter(Rest, Op, Acc);

do_add_filter([{<<"op">>, {enum, Type}}|Rest], _Op, Acc) ->
    Op2 = case Type of
        <<"AND">> -> 'and';
        <<"OR">> -> 'or';
        <<"NOT">> -> 'not'
    end,
    do_add_filter(Rest, Op2, Acc);

do_add_filter([{Field, Filter}|Rest], Op, Acc) ->
    Acc2 = do_add_filter2(Field, maps:to_list(Filter), Acc),
    do_add_filter(Rest, Op, Acc2).


%% @private
do_add_filter2(_Field, [], Acc) ->
    Acc;

do_add_filter2(Field, [{_Op, null}|Rest], Acc) ->
    do_add_filter2(Field, Rest, Acc);

do_add_filter2(<<"type">>, [{<<"eq">>, {enum, Type}}|Rest], Acc) ->
    Mod = nkdomain_reg:get_schema_type_module(Type),
    true = Mod /= undefined,
    Type2 = nkdomain_reg:get_module_type(Mod),
    Acc2 = [{type, eq, Type2}|Acc],
    do_add_filter2(<<"type">>, Rest, Acc2);

do_add_filter2(<<"type">>, [{<<"values">>, Values}|Rest], Acc) ->
    Types = lists:map(
        fun({enum, Type}) ->
            Mod = nkdomain_reg:get_schema_type_module(Type),
            true = Mod /= undefined,
            nkdomain_reg:get_module_type(Mod)
        end,
        Values),
    Acc2 = [{type, values, Types}|Acc],
    do_add_filter2(<<"type">>, Rest, Acc2);

do_add_filter2(<<"name">>, [{Op, Val}|Rest], Acc) ->
    Acc2 = add_filter_text(name_norm, Op, Val, Acc),
    do_add_filter2(<<"name">>, Rest, Acc2);

do_add_filter2(<<"description">>, [{Op, Val}|Rest], Acc) ->
    Acc2 = add_filter_text(description_norm, Op, Val, Acc),
    do_add_filter2(<<"description">>, Rest, Acc2);

do_add_filter2(Field, [{Op, Value}|Rest], Acc)
        when Op == <<"eq">>; Op == <<"values">>; Op == <<"gt">>; Op == <<"gte">>;
             Op == <<"lt">>; Op == <<"lte">>; Op == <<"prefix">>; Op == <<"exists">> ->
    Acc2 =  [{camel_to_erl(Field), binary_to_existing_atom(Op, latin1), Value}|Acc],
    do_add_filter2(Field, Rest, Acc2);

do_add_filter2(<<"path">>, [{<<"childsOf">>, Value}|Rest], Acc) ->
    Acc2 =  [{path, subdir, Value}|Acc],
    do_add_filter2(<<"path">>, Rest, Acc2).


%% @private
add_filter_text(Field, Op, Value, Acc) when Op == <<"eq">>; Op == <<"prefix">> ->
    Value2 = nkdomain_store_es_util:normalize(Value),
    [{Field, binary_to_existing_atom(Op, latin1), Value2}|Acc];

add_filter_text(Field, <<"wordsAndPrefix">>, Value, Acc) ->
    case nkdomain_store_es_util:normalize_multi(Value) of
        [] ->
            Acc;
        [Word] ->
            [{Field, prefix, Word}|Acc];
        Words ->
            [Last|Full] = lists:reverse(Words),
            [{Field, prefix, Last}, {Field, values, Full} | Acc]
    end.



%% @private
read_objs(From, Size, Spec) ->
    do_read_objs(From, Size, Spec, []).


%% @private
do_read_objs(Start, Size, Spec, Acc) ->
    case nkdomain:search(Spec#{from=>Start, size=>Size, fields=>[]}) of
        {ok, Total, [], _Meta} ->
            {ok, Total, lists:reverse(Acc)};
        {ok, Total, Data, _Meta} ->
            Acc2 = lists:foldl(
                fun(#{<<"obj_id">>:=ObjId}, FunAcc) ->
                    case nkdomain_lib:read(ObjId) of
                        {ok, ObjIdExt, Obj} ->
                            [{ObjIdExt, Obj}|FunAcc];
                        {error, Error} ->
                            lager:warning("could not read object ~s: ~p", [ObjId, Error]),
                            FunAcc
                    end
                end,
                Acc,
                Data),
            case length(Acc2) of
                Size ->
                    {ok, Total, lists:reverse(Acc2)};
                Records when Records > Size ->
                    {ok, Total, lists:sublist(lists:reverse(Acc2), Size)};
                _ ->
                    do_read_objs(Start+Size, Size, Spec, Acc2)
            end;
        {error, Error} ->
            {error, Error}
    end.




%% @private
camel_to_erl(<<"createdById">>) -> created_by;
camel_to_erl(<<"createdTime">>) -> created_time;
camel_to_erl(<<"destroyedCode">>) -> destroyed_code;
camel_to_erl(<<"destroyedReason">>) -> destroyed_reason;
camel_to_erl(<<"destroyedTime">>) -> destroyed_time;
camel_to_erl(<<"domainId">>) -> domain_id;
camel_to_erl(<<"expiresTime">>) -> expires_time;
camel_to_erl(<<"iconId">>) -> icon_id;
camel_to_erl(<<"objId">>) -> obj_id;
camel_to_erl(<<"objName">>) -> obj_name;
camel_to_erl(<<"srvId">>) -> srv_id;
camel_to_erl(<<"subTypes">>) -> subtypes;
camel_to_erl(<<"updatedById">>) -> updated_by;
camel_to_erl(<<"updatedTime">>) -> updated_time;
camel_to_erl(Atom) when is_atom(Atom) -> Atom;
camel_to_erl(Erl) -> binary_to_existing_atom(Erl, utf8).



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).


