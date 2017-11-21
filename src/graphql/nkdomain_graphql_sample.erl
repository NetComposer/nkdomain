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

%% @doc GraphQL samples
-module(nkdomain_graphql_sample).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-compile(export_all).

-include("nkdomain.hrl").



get1() ->
    Query = <<"
        query {
            node(id: \"root\") {
                id
            }
        }
    ">>,
    {ok, #{data := #{<<"node">> := #{<<"id">> := <<"root">>}}}} = request(Query),
    ok.

get2() ->
    Query = <<"
        query {
            node(id: \"carlos@mail\") {
                id
                ... on User {
                    userName
                    domain {
                        path
                    }
                }
            }
        }
    ">>,
    request(Query).


introduce_user(Num) ->
    Num2 = nklib_util:to_binary(Num),
    Mutation = [
        "mutation {
            introduceUser(input: {
                userName: \"Name", Num2, "\"
                userSurname: \"SurName", Num2, "1\"
                email: \"g", Num2, "@test\"
            }) {
                objId
            }
        }"],
    {ok, #{<<"introduceUser">>:=#{<<"objId">>:=ObjId}}} = request(Mutation),
    ObjId.


all_objs() ->
    Query = <<"
        query {
            allObjects(
                from: 0
                size: 3
                filter: [
                    {
                        type: {values: [User, Domain]},
                        name: {fuzzy: \"carlos last\"}
                    }
                    {
                        op: AND
                        path: {childsOf: \"/sipstorm\"}
                    }
                    {
                        op: OR
                        createdTime: {gt: 0, lte: 9999999}
                    }
                ]
                sort: [
                    {
                        domainId: {
                            order: \"DESC\"
                        }
                    },
                    {
                        path: {}
                    }

                ]
            ) {
                totalCount
                objects {
                    type
                    objId
                    path
                    name
                    domain {
                        name
                    }
                }
            }
        }
    ">>,
    request(Query).



request(Query) ->
    nkdomain_graphql:request(?NKROOT, Query, #{}).