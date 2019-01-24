%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain
-module(
nkdomain_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all, nowarn_export_all]).

-include("nkdomain.hrl").


%% ===================================================================
%% Sample
%% ===================================================================


all_tests() ->
    lager:warning("\n\nStarting ALL_TESTS_1\n\n"),
    timer:sleep(2000),
    nkdomain_test_1:all_tests(),
    lager:warning("\n\nStarting ALL_TESTS_2\n\n"),
    timer:sleep(2000),
    nkdomain_test_2:all_tests(),
    lager:warning("\n\nStarting GRAPHQL_TESTS_1\n\n"),
    timer:sleep(2000),
    nkdomain_graphql_test_1:all_tests(),
    lager:warning("\n\nStarting GRAPHQL_TESTS_1\n\n"),
    timer:sleep(2000),
    nkdomain_graphql_test_2:all_tests(),
    lager:warning("\n\nALL TEST OK!\n\n").




drop() ->
    nkservice_pgsql_actors_util:drop(?ROOT_SRV).


t0() ->
    api(#{verb=>list, resource=>users}).


t1() ->
    {ok, #{<<"metadata">>:=#{<<"uid">>:=UID}}} =
        api(#{resource=>users, name=>admin}),
    api(#{resource=>uids, name=>UID}).



make_users(Num) when Num > 0 ->
    Num2 = integer_to_binary(Num),
    Body = <<"
        apiVersion: core/v1a1
        kind: User
        spec:
            password: ", Num2/binary, "
        metadata:
            name: ", Num2/binary, "
            labels:
                pos: ", Num2/binary, "
            domain: root
    "/utf8>>,
    case api(#{verb=>update, resource=>users, body=>yaml(Body)}) of
        {ok, #{<<"metadata">>:=#{<<"uid">>:=UID}}} ->
            Body2 = <<"
                spec:
                    name: name-", Num2/binary, "
                    surname: 'my surname-", Num2/binary, "'
                    birthTime: '1970-01-02'
                    gender: M
                    email:
                        - type: main
                          email: email-", Num2/binary, "
                        - type: home
                          email: email2-", Num2/binary, "
                metadata:
                    fts:
                        name: name-", Num2/binary, "
                        surname: 'my surname-", Num2/binary, "'
                    links:
                        users: ", UID/binary, "
            "/utf8>>,
            {ok, _} = api(#{verb=>update, resource=>contacts, body=>yaml(Body2)}),
            make_users(Num-1);
        {error, Error} ->
            {error, Error}
    end;

make_users(_) ->
    ok.



read_users_iter(Min, Max, Iters) when Iters > 0 ->
    read_users(Min, Max),
    read_users_iter(Min, Max, Iters-1);

read_users_iter(_Min, _Max, _Iters) ->
    ok.


read_users(Num, Max) when Num =< Max ->
    Num2 = integer_to_binary(Num),
    io:format("READING ~p", [Num2]),
    {ok, _} = api(#{verb=>get, resource=>users, name=>Num2}),
    read_users(Num+1, Max);

read_users(_, _) ->
    ok.




api(Api) ->
    nkdomain_api:request(?ROOT_SRV, Api#{group=>?GROUP_CORE, vsn=>?GROUP_CORE_V1A1}).



yaml(Str) ->
    [Obj] = nklib_yaml:decode(Str),
    Obj.