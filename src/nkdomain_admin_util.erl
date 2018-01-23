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

%% @doc NkDomain service callback module
-module(nkdomain_admin_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([get_data/3, get_agg_srv_id/3, get_agg_name/4, get_agg_term/4, table_filter/4]).
-export([db_search/3, db_aggs/3, db_aggs/4]).
-export([obj_id_url/1, obj_id_url/2, obj_path_url/2, table_entry/3]).
-export([get_type_info/2, get_type_view_mod/2, get_obj_view_mod/2]).
-export([add_filter/3, add_exists_filter/3, add_search_filter/3, add_time_filter/4, add_multiword_filter/3, get_file_url/2]).
-export([make_type_view_id/2, make_type_view_filter/2, make_obj_view_id/2, make_view_subview_id/3]).
-export([make_confirm/2, make_msg/2, make_msg_ext/4, get_domains/1]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_data([?ID_ADMIN_DETAIL_TYPE_VIEW, ?ID_ADMIN_TREE_ALL_OBJS, _Path], Spec, Session) ->
    do_get_data(?ID_ADMIN_TREE_ALL_OBJS, #{}, Spec, Session);

get_data([?ID_ADMIN_DETAIL_TYPE_VIEW, Type, _Path], Spec, Session) ->
    do_get_data(Type, #{}, Spec, Session);

get_data([?ID_ADMIN_DETAIL_OBJ_SUBVIEW, Type, ObjId, SubType, <<"table">>], Spec, Session) ->
    do_get_data(SubType, #{orig_type=>Type, obj_id=>ObjId}, Spec, Session);

get_data(_Parts, _Spec, Session) ->
    {error, unrecognized_element, Session}.


%% @private
do_get_data(Type, Opts, Spec, Session) ->
    case get_type_view_mod(Type, Session) of
        {ok, Mod} ->
            case nkdomain_admin_table:table_data(Type, Mod, Spec, Opts, Session) of
                {ok, Total, Data} ->
                    Reply = #{
                        total_count => Total,
                        pos => maps:get(start, Spec, 0),
                        data => Data
                    },
                    {ok, Reply, Session};
                {error, Error} ->
                    ?LLOG(warning, "error getting query: ~p", [Error]),
                    {ok, #{total_count=>0, pos=>0, data=>[]}, Session}
            end;
        _ ->
            {error, unrecognized_element, Session}
    end.


%% @doc
db_search(ObjType, {QueryType, Domain, Opts}, #admin_session{extra_filters=ExtraFilters}) ->
    nkdomain_db:search(ObjType, {QueryType, Domain, Opts#{extra_filters => ExtraFilters}});

%% @doc
db_search(ObjType, {QueryType, Domain, Filters, Opts}, #admin_session{extra_filters=ExtraFilters}) ->
    nkdomain_db:search(ObjType, {QueryType, Domain, Filters, Opts#{extra_filters => ExtraFilters}}).


%% @doc
db_aggs(Type, AggType, AdminSession) ->
    db_aggs(Type, AggType, #{}, AdminSession).

db_aggs(Type, AggType, Opts, #admin_session{extra_filters=ExtraFilters}) ->
    nkdomain_db:aggs(Type, AggType, Opts#{extra_filters=>ExtraFilters}).


%% @private
get_type_info(Type, _Session) ->
    case ?CALL_NKROOT(object_admin_info, [Type]) of
        Info when is_map(Info) ->
            {true, Info};
        _ ->
            false
    end.


%% @private
get_type_view_mod(?ID_ADMIN_TREE_ALL_OBJS, _Session) ->
    {ok, nkdomain_admin_all_objs_view};

get_type_view_mod(Type, _Session) ->
    case ?CALL_NKROOT(object_admin_info, [Type]) of
        #{type_view_mod:=Mod} ->
            {ok, Mod};
        _ ->
            not_found
    end.

%% @private
get_obj_view_mod(Type, _Session) ->
    case ?CALL_NKROOT(object_admin_info, [Type]) of
        #{obj_view_mod:=Mod} ->
            {ok, Mod};
        _ ->
            not_found
    end.


%% @private
get_agg_srv_id(Type, Path, Session) ->
    case do_get_agg(<<"srv_id">>, Type, Path, Session) of
        {ok, Data, OverFlow} ->
            SrvIds1 = lists:map(
                fun({S, _Num}) ->
                    case S of
                        <<>> -> {?NKROOT, <<"(root)">>};
                        _ -> {S, S}
                    end
                end,
                Data),
            SrvIds2 = [{?ADMIN_ALL_OBJS, <<>>} | lists:sort(SrvIds1)],
            SrvIds3 =  case OverFlow of
                false ->
                    SrvIds2;
                true ->
                    SrvIds2 ++ [{?ADMIN_ALL_OBJS, <<"...">>}]
            end,
            [#{id => I, value => V} || {I, V} <- SrvIds3];
        {error, _Error} ->
            []
    end.

%% @doc
get_agg_name(Field, Type, Path, Session) ->
    case do_get_agg(Field, Type, Path, Session) of
        {ok, Data, OverFlow} ->
            List1 = lists:foldl(
                fun({ObjId, _Num}, Acc) ->
                    case nkdomain:get_name(ObjId) of
                        {ok, #{name:=Name, path:=ObjPath, obj_name:=ObjName}} ->
                            Name2 = case Name of
                                <<>> ->
                                    case ObjName of
                                        <<>> when ObjId == <<"root">> -> <<"/">>;
                                        _ -> ObjName
                                    end;
                                _ ->
                                    Name
                            end,
                            [{ObjPath, ObjId, Name2}|Acc];
                        _ ->
                            Acc
                    end
                end,
                [],
                Data),
            List2 = [{I, N}||{_P, I, N} <- lists:keysort(1, List1)],
            List3 = [{?ADMIN_ALL_OBJS, <<>>}|List2],
            List4 = case OverFlow of
                false ->
                    List3;
                true ->
                    List3++[{?ADMIN_ALL_OBJS, <<"...">>}]
            end,
            [#{id => I, value => V}||{I, V} <- List4];
        {error, _Error} ->
            #{}
    end.


%% @doc
get_agg_term(Field, Type, Path, Session) ->
    case do_get_agg(Field, Type, Path, Session) of
        {ok, Data, OverFlow} ->
            List1 = lists:sort([{Name, Name} || {Name, _Num} <- Data]),
            List2 = [{?ADMIN_ALL_OBJS, <<>>}|List1],
            List3 = case OverFlow of
                false ->
                    List2;
                true ->
                    List2++[{?ADMIN_ALL_OBJS, <<"...">>}]
            end,
            [#{id => I, value => V}||{I, V} <- List3];
        {error, _Error} ->
            #{}
    end.

%% @private
do_get_agg(Field, Type, Path, Session) ->
    %% lager:error("NKLOG Field, Type, Path ~p", [{Field, Type, Path}]),
    Spec1 = #{deep=>true, size=>50},
    Spec2 = case Type of
        <<>> -> Spec1;
        _ -> Spec1#{type => Type}
    end,
    %case nkdomain_db:aggs(core, {query_values, Path, Field, Spec2}) of
    case db_aggs(core, {query_values, Path, Field, Spec2}, Session) of
        {ok, 0, _, _} ->
            {ok, [], false};
        {ok, _N, Data, #{agg_sum_other:=SumOther}} ->
            {ok, Data, SumOther>0};
        {error, Error} ->
            {error, Error}
    end.



%% @private
table_filter(_Field, <<>>, _Info, Acc) ->
    {ok, Acc};

table_filter(_Field, ?ADMIN_ALL_OBJS, _Info, Acc) ->
    {ok, Acc};

table_filter(<<"nkBaseDomain">>, Path, _Info, Acc) ->
    {ok, [{<<"path">>, subdir, Path}|Acc]};

table_filter(<<"domain">>, Data, _Info, Acc) ->
    {ok, [{<<"domain_id">>, eq, Data}|Acc]};

table_filter(<<"type">>, Data, _Info, Acc) ->
    {ok, [{<<"type">>, eq, Data}|Acc]};

%%table_filter({<<"service">>, Data}, _Info, Acc) ->
%%    Root = to_bin(?NKROOT),
%%    Data2 = case Data of
%%        Root -> <<>>;
%%        _ -> Data
%%    end,
%%    {ok, [{<<"srv_id">>, eq, Data2}|Acc]};

table_filter(<<"name">>, Data, _Info, Acc) ->
    {ok, add_multiword_filter(<<"name_norm">>, Data, Acc)};

table_filter(<<"obj_name">>, Data, _Info, Acc) ->
    {ok, add_search_filter(<<"obj_name">>, Data, Acc)};

table_filter(<<"created_by">>, Data, _Info, Acc) ->
    {ok, [{<<"created_by">>, eq, Data}|Acc]};

table_filter(<<"created_time">>, <<"custom">>, _Info, _Acc) ->
    {error, date_needs_more_data};

table_filter(<<"created_time">>, Data, #{timezone_offset:=Offset}, Acc) ->
    Secs = Offset * 60,
    {ok, add_time_filter(<<"created_time">>, Data, Secs, Acc)};

table_filter(_Field, _Data, _Info, _Acc) ->
    unknown.





%%%% @private
%%table_filter_time(<<"custom">>, _Filter, _Acc) ->
%%    {error, date_needs_more_data};
%%
%%table_filter_time(Data, Filter, Acc) ->
%%    Secs = 60 * maps:get(<<"timezone_offset">>, Filter, 0),
%%    TimeFilter = case Data of
%%        <<"today">> ->
%%            nkdomain_admin_util:time(today, Secs);
%%        <<"yesterday">> ->
%%            nkdomain_admin_util:time(yesterday, Secs);
%%        <<"last_7">> ->
%%            nkdomain_admin_util:time(last7, Secs);
%%        <<"last_30">> ->
%%            nkdomain_admin_util:time(last30, Secs);
%%        <<"custom">> ->
%%            <<"">>;
%%        _ ->
%%            <<"">>
%%    end,
%%    {ok, Acc#{<<"created_time">> => TimeFilter}}.


%% @private
table_entry(Type, Entry, Pos) ->
    #{
        <<"obj_id">> := ObjId,
        <<"path">> := Path,
        <<"created_by">> := CreatedBy,
        <<"created_time">> := CreatedTime
    } = Entry,
    {Domain, ShortName} = case Type of
        ?ID_ADMIN_TREE_ALL_OBJS ->
            {ok, D, _Type, S} = nkdomain_util:get_parts(Path),
            {D, S};
        _ ->
            {ok, D, S} = nkdomain_util:get_parts(Type, Path),
            {D, S}
    end,
    Enabled = maps:get(<<"enabled">>, Entry, true),
    %Root = nklib_util:to_binary(?NKROOT),
    %SrvId2 = case SrvId of
    %    Root -> <<"(nkroot)">>;
    %    _ -> SrvId
    %end,
    #{
        checkbox => <<"0">>,
        pos => Pos,
        id => ObjId,
        obj_name => obj_id_url(ObjId, ShortName),
        name => maps:get(<<"name">>, Entry, <<>>),
        domain => obj_path_url(Domain, Domain),
        created_by => obj_id_url(CreatedBy),
        created_time => CreatedTime,
        enabled => Enabled
    }.



%% @doc
obj_id_url(Id) ->
    case nkdomain:get_name(Id) of
        {ok, #{obj_id:=ObjId, name:=Name}} ->
            obj_id_url(ObjId, Name);
        _ ->
            obj_id_url(Id, <<"(deleted)">>)
    end.


%% @doc
obj_id_url(ObjId, Name) ->
    <<"<a href=\"#_id/", ObjId/binary, "\">", Name/binary, "</a>">>.


%% @doc
obj_path_url(Path, Name) ->
    <<"<a href=\"#", Path/binary, "\">", Name/binary, "</a>">>.


%% @private
add_filter(Field, Data, Acc) ->
    [{Field, eq, Data}|Acc].


%% @private
add_exists_filter(Field, Bool, Acc) ->
    [{Field, exists, Bool}|Acc].


%% @private
add_search_filter(Field, <<">", Data/binary>>, Acc) ->
    [{Field, gt, Data}|Acc];
add_search_filter(Field, <<"<", Data/binary>>, Acc) ->
    case binary:split(Data, <<"-">>) of
        [A, B] ->
            [{Field, gt, A}, {Field, lt, B}];
        _ ->
            [{Field, lt, Data}|Acc]
    end;
add_search_filter(Field, <<"!", Data/binary>>, Acc) ->
    [{'not', {Field, eq, Data}}|Acc];
add_search_filter(Field, Data, Acc) ->
    [{Field, prefix, Data}|Acc].


%% @doc
add_time_filter(Field, Spec, SecsOffset, Acc) ->
    Now = nklib_util:timestamp(),
    {{Y, M, D}, _} = nklib_util:timestamp_to_gmt(Now),
    TodayGMT = nklib_util:gmt_to_timestamp({{Y, M, D}, {0, 0, 0}}) * 1000,
    TodayS = TodayGMT + (SecsOffset * 1000),
    TodayE = TodayS + 24*60*60*1000 - 1,
    {S, E} = case Spec of
        <<"today">> ->
            {TodayS, TodayE};
        <<"yesterday">> ->
            Sub = 24*60*60*1000,
            {TodayS-Sub, TodayE-Sub};
        <<"last_7">> ->
            Sub = 7*24*60*60*1000,
            {TodayS-Sub, TodayE};
        <<"last_30">> ->
            Sub = 30*24*60*60*1000,
            {TodayS-Sub, TodayE}
    end,
    [{Field, gte, S}, {Field, lte, E}|Acc].


%% @doc
add_multiword_filter(Field, Data, Acc) ->
    [
        {Field, prefix, Word}
        || Word <- nkdomain_store_es_util:normalize_multi(Data)
    ] ++ Acc.





%%%% @private Useful for testing
%%time2(Spec, SecsOffset) ->
%%    <<"<", R1/binary>> = time(Spec, SecsOffset),
%%    [T1, R2] = binary:split(R1, <<"-">>),
%%    [T2, _] = binary:split(R2, <<">">>),
%%    T1B = nklib_util:timestamp_to_local(nklib_util:to_integer(T1) div 1000),
%%    T2B = nklib_util:timestamp_to_local(nklib_util:to_integer(T2) div 1000),
%%    {T1B, T2B}.


%% @doc
get_file_url(FileId, #admin_session{http_auth_id=AuthId}) ->
    <<"../_file/", FileId/binary, "?auth=", AuthId/binary>>.


%% @doc
make_type_view_id(Type, Path) ->
    nkadmin_util:make_id([?ID_ADMIN_DETAIL_TYPE_VIEW, Type, Path]).


%% @doc
make_type_view_filter(Type, Filter) ->
    nkadmin_util:make_id([?ID_ADMIN_DETAIL_TYPE_FILTER, Type, to_bin(Filter)]).


%% @doc
make_obj_view_id(Type, ObjId) ->
    nkadmin_util:make_id([?ID_ADMIN_DETAIL_OBJ_VIEW, Type, ObjId]).

%% @doc
make_view_subview_id(Type, ObjId, SubView) ->
    nkadmin_util:make_id([?ID_ADMIN_DETAIL_OBJ_SUBVIEW, Type, ObjId, SubView]).


%% @doc
make_confirm(Title, Text) ->
    #{
        class => <<"popup">>,
        value => #{
            class => <<"webix_confirm">>,
            % class => <<"webix_alert">>,
            value => #{
                title => to_bin(Title),
                text => to_bin(Text),
                ok => <<"OK">>,
                cancel => <<"Cancel">>,
                % type: "confirm-warning",
                type => <<"confirm-error">>,
                callback => #{
                    nkParseFunction =>
                    <<"'function(response) {console.log(\"RESPONSE (true/false): \", response);}'">>
                }
            }
        }
    }.


%% @doc
make_msg(Type, Msg) ->
    #{
        class => <<"popup">>,
        value => #{
            class =>  <<"webix_message">>,
            value => #{
                text => to_bin(Msg),
                type => case Type of ok -> <<"ok">>; error -> <<"error">> end,
                expire => 3000          % -1 for static
            }
        }
    }.


%% @doc
make_msg_ext(Type, Msg, Error, #admin_session{srv_id=SrvId}) ->
    {Code, Txt} = nkservice_util:error(SrvId, Error),
    Msg2 = <<Msg/binary, ": ", Txt/binary, " (", Code/binary, ")">>,
    make_msg(Type, Msg2).


%% @doc
get_domains(Base) ->
    case nkdomain:get_paths_type(Base, ?DOMAIN_DOMAIN) of
        {ok, _, List} ->
            {ok, [{ObjId, Path} || #{<<"obj_id">>:=ObjId, <<"path">>:=Path} <-List]};
        {error, Error} ->
            {error, Error}
    end.




%% @private
to_bin(R) -> nklib_util:to_binary(R).