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
-export([get_data/3, get_agg_srv_id/2, get_agg_name/3, get_agg_term/3, table_filter/3, table_filter_time/3]).
-export([obj_id_url/1, obj_id_url/2, obj_path_url/2, table_entry/3]).
-export([get_type_info/2, get_type_view_mod/2, get_obj_view_mod/2]).
-export([search_spec/1, time/2, time2/2, get_file_url/2]).
-export([make_type_view_id/1, make_type_view_subfilter_id/1, make_obj_view_id/2, make_view_subview_id/3]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_data([?ADMIN_DETAIL_TYPE_VIEW, Type], Spec, Session) ->
    do_get_data(Type, #{}, Spec, Session);

get_data([?ADMIN_DETAIL_OBJ_SUBVIEW, Type, ObjId, SubType, <<"table">>], Spec, Session) ->
    do_get_data(SubType, #{orig_type=>Type, obj_id=>ObjId}, Spec, Session);

get_data(_Parts, _Spec, Session) ->
    {error, unrecognized_element, Session}.


%% @private
do_get_data(Type, Opts, Spec, Session) ->
    case get_type_view_mod(Type, Session) of
        {ok, Mod} ->
            Start = maps:get(start, Spec, 0),
            Size = case maps:find('end', Spec) of
                {ok, End} when End>Start ->
                    End-Start;
                _ ->
                    100
            end,
            Filter = maps:get(filter, Spec, #{}),
            Sort = case maps:get(sort, Spec, undefined) of
                #{
                    id := SortId,
                    dir := SortDir
                } ->
                    {SortId, to_bin(SortDir)};
                undefined ->
                    undefined
            end,
            FunSpec = #{
                start => Start,
                size => Size,
                filter => Filter,
                sort => Sort
            },
            case Mod:table_data(FunSpec, Opts, Session) of
                {ok, Total, Data} ->
                    Reply = #{
                        total_count => Total,
                        pos => Start,
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


%% @private
get_type_info(Type, _Session) ->
    case ?CALL_NKROOT(object_admin_info, [Type]) of
        Info when is_map(Info) ->
            {true, Info};
        _ ->
            false
    end.


%% @private
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
get_agg_srv_id(Type, Path) ->
    case do_get_agg(<<"srv_id">>, Type, Path) of
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
get_agg_name(Field, Type, Path) ->
    case do_get_agg(Field, Type, Path) of
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
get_agg_term(Field, Type, Path) ->
    case do_get_agg(Field, Type, Path) of
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
do_get_agg(Field, Type, Path) ->
    Filter = case Type of
        <<>> -> #{};
        _ -> #{type => Type}
    end,
    Spec = #{
        filters => Filter,
        size => 50
    },
    case nkdomain:search_agg_field(Path, Field, Spec, true) of
        {ok, 0, _, _} ->
            {ok, [], false};
        {ok, _N, Data, #{agg_sum_other:=SumOther}} ->
            {ok, Data, SumOther>0};
        {error, Error} ->
            {error, Error}
    end.





%% @private
table_filter({_Field, <<>>}, _Info, Acc) ->
    {ok, Acc};

table_filter({_Field, ?ADMIN_ALL_OBJS}, _Info, Acc) ->
    {ok, Acc};

table_filter({<<"nkBaseDomain">>, Path}, _Info, Acc) ->
    {ok, Acc#{<<"path">> => <<"childs_of:", Path/binary>>}};

table_filter({<<"domain">>, Data}, _Info, Acc) ->
    {ok, Acc#{<<"domain_id">> => Data}};

table_filter({<<"service">>, Data}, _Info, Acc) ->
    Root = to_bin(?NKROOT),
    Data2 = case Data of
        Root -> <<>>;
        _ -> Data
    end,
    {ok, Acc#{<<"srv_id">> => Data2}};

table_filter({<<"obj_name">>, Data}, _Info, Acc) ->
    {ok, Acc#{<<"obj_name">> => search_spec(Data)}};

table_filter({<<"name">>, Data}, _Info, Acc) ->
    {ok, Acc#{<<"user.fullname_norm">> => search_spec(Data)}};

table_filter({<<"created_by">>, Data}, _Info, Acc) ->
    {ok, Acc#{<<"created_by">> => Data}};

table_filter({<<"created_time">>, <<"custom">>}, _Info, _Acc) ->
    {error, date_needs_more_data};

table_filter({<<"created_time">>, Data}, #{timezone_offset:=Offset}, Acc) ->
    Secs = Offset * 60,
    Filter = case Data of
        <<"today">> ->
            time(today, Secs);
        <<"yesterday">> ->
            time(yesterday, Secs);
        <<"last_7">> ->
            time(last7, Secs);
        <<"last_30">> ->
            time(last30, Secs);
        <<"custom">> ->
            <<"">>;
        _ ->
            <<"">>
    end,
    {ok, Acc#{<<"created_time">> => Filter}};

table_filter(_, _Info, _Acc) ->
    unknown.

%% @private
table_filter_time(<<"custom">>, _Filter, _Acc) ->
    {error, date_needs_more_data};

table_filter_time(Data, Filter, Acc) ->
    Secs = 60 * maps:get(<<"timezone_offset">>, Filter, 0),
    TimeFilter = case Data of
        <<"today">> ->
            nkdomain_admin_util:time(today, Secs);
        <<"yesterday">> ->
            nkdomain_admin_util:time(yesterday, Secs);
        <<"last_7">> ->
            nkdomain_admin_util:time(last7, Secs);
        <<"last_30">> ->
            nkdomain_admin_util:time(last30, Secs);
        <<"custom">> ->
            <<"">>;
        _ ->
            <<"">>
    end,
    {ok, Acc#{<<"created_time">> => TimeFilter}}.


%% @private
table_entry(Type, Entry, Pos) ->
    #{
        <<"obj_id">> := ObjId,
        <<"path">> := Path,
        <<"srv_id">> := SrvId,
        <<"created_by">> := CreatedBy,
        <<"created_time">> := CreatedTime
    } = Entry,
    {ok, Domain, ShortName} = nkdomain_util:get_parts(Type, Path),
    Enabled = maps:get(<<"enabled">>, Entry, true),
    Root = nklib_util:to_binary(?NKROOT),
    SrvId2 = case SrvId of
        Root -> <<"(nkroot)">>;
        _ -> SrvId
    end,
    #{
        checkbox => <<"0">>,
        pos => Pos,
        id => ObjId,
        service => SrvId2,
        obj_name => obj_id_url(ObjId, ShortName),
        name => maps:get(name, Entry, <<>>),
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
search_spec(<<">", _/binary>>=Data) -> Data;
search_spec(<<"<", _/binary>>=Data) -> Data;
search_spec(<<"!", _/binary>>=Data) -> Data;
search_spec(Data) -> <<"prefix:", Data/binary>>.


%% @doc
time(Spec, SecsOffset) ->
    Now = nklib_util:timestamp(),
    {{Y, M, D}, _} = nklib_util:timestamp_to_gmt(Now),
    TodayGMT = nklib_util:gmt_to_timestamp({{Y, M, D}, {0, 0, 0}}) * 1000,
    TodayS = TodayGMT + (SecsOffset * 1000),
    TodayE = TodayS + 24*60*60*1000 - 1,
    {S, E} = case Spec of
        today ->
            {TodayS, TodayE};
        yesterday ->
            Sub = 24*60*60*1000,
            {TodayS-Sub, TodayE-Sub};
        last7 ->
            Sub = 7*24*60*60*1000,
            {TodayS-Sub, TodayE};
        last30 ->
            Sub = 30*24*60*60*1000,
            {TodayS-Sub, TodayE}
    end,
    list_to_binary(["<", nklib_util:to_binary(S), "-", nklib_util:to_binary(E),">"]).


%% @private Useful for testing
time2(Spec, SecsOffset) ->
    <<"<", R1/binary>> = time(Spec, SecsOffset),
    [T1, R2] = binary:split(R1, <<"-">>),
    [T2, _] = binary:split(R2, <<">">>),
    T1B = nklib_util:timestamp_to_local(nklib_util:to_integer(T1) div 1000),
    T2B = nklib_util:timestamp_to_local(nklib_util:to_integer(T2) div 1000),
    {T1B, T2B}.


%% @doc
get_file_url(FileId, #admin_session{http_auth_id=AuthId}) ->
    <<"../_file/", FileId/binary, "?auth=", AuthId/binary>>.


%% @doc
make_type_view_id(Type) ->
    nkadmin_util:make_id([?ADMIN_DETAIL_TYPE_VIEW, Type]).


%% @doc
%%make_type_view_subfilter_id(Id) ->
%%    nkadmin_util:make_id([Id, <<"subdomains">>]).

make_type_view_subfilter_id(Type) ->
    nkadmin_util:make_id([make_type_view_id(Type), <<"subdomains">>]).

%% @doc
make_obj_view_id(Type, ObjId) ->
    nkadmin_util:make_id([?ADMIN_DETAIL_OBJ_VIEW, Type, ObjId]).

%% @doc
make_view_subview_id(Type, ObjId, SubView) ->
    nkadmin_util:make_id([?ADMIN_DETAIL_OBJ_SUBVIEW, Type, ObjId, SubView]).


%% @private
to_bin(K) -> nklib_util:to_binary(K).