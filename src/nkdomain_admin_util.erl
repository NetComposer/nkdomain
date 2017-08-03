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
-export([get_data/3, search_spec/1, time/2, time2/2, get_file_url/2]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_data(Key, Spec, Session) ->
    % lager:warning("NKLOG Spec ~p", [Spec]),
    case nkadmin_util:get_key_data(Key, Session) of
        #{data_fun:=Fun} ->
            Start = maps:get(start, Spec, 0),
            Size = case maps:find('end', Spec) of
                {ok, End} when End > Start -> End-Start;
                _ -> 100
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
            case Fun(FunSpec, Session) of
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
            {error, object_not_found, Session}
    end.


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


%% @private
to_bin(K) -> nklib_util:to_binary(K).