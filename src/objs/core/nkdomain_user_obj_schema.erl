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

%% @doc User Object Schemas
-module(nkdomain_user_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/2, object_schema_types/0, object_schema_queries/0, object_schema_mutations/0,
         object_mutation/3]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================



object_execute(Field, User) ->
    case Field of
        <<"userName">> -> {ok, maps:get(name, User, null)};
        <<"userSurname">> -> {ok, maps:get(surname, User, null)};
        <<"email">> -> {ok, maps:get(email, User, null)};
        <<"phone">> -> {ok, maps:get(phone_t, User, null)};
        <<"address">> -> {ok, maps:get(address_t, User, null)}
    end.


%% @doc
object_schema_types() ->
    #{
        'User' => #{
            fields => #{
                userName => {string, #{comment=>"User family name"}},
                userSurname => {string, #{comment=>"User surname"}},
                email => string,
                phoneTwo => string,
                address => string,
                status => {connection, 'UserStatus', #{comment => "User current statuses"}}
            },
            is_object => true,
            comment => "An User"
        },
        'UserStatus' => #{
            fields => #{
                domainPath => {no_null, string, #{comment=>"Domain this status belongs to"}},
                userStatus => string,
                updatedTime => time
            },
            is_connection => true
        }
    }.


object_schema_queries() ->
    #{
        allUsers => {'SearchResult', #{
            params => #{
                filter => {list, objectFilter, #{default => "[]"}},
                sort => {list, objectSortBy},
                from => {int, #{default => 0}},
                size => {int, #{default => 10}}
            }}}
    }.


object_schema_mutations() ->
    #{
        introduceUser => #{
            input => #{
                userName => {no_null, string},
                userSurname => {no_null, string},
                domain => string,
                objName => string,
                password => string,
                email => {no_null, string},
                phone => string,
                address => string
            },
            output => #{
                objId => {no_null, string},
                objName => {no_null, string},
                path => {no_null, string},
                email => {no_null, string},
                phone => string,
                address => string
            },
            comment => "Creates a new user"
        }
    }.


%% Sample:
%% mutation M {
%%     introduceUser(input: {
%%         userName: "Name1"
%%         userSurname: "SurName1"
%%         email: "g1@test"
%%     }) {
%%         objId
%%     }
%% }

object_mutation(<<"introduceUser">>, Params, _Ctx) ->
    {Base, User} = lists:foldl(
        fun({Key, Val}, {BaseAcc, UserAcc}) ->
            case Key of
                <<"userName">> ->
                    {BaseAcc, UserAcc#{name=>Val}};
                <<"userSurname">> ->
                    {BaseAcc, UserAcc#{surname=>Val}};
                <<"domain">> ->
                    {BaseAcc#{domain_id=>Val}, UserAcc};
                <<"objName">> ->
                    {BaseAcc#{obj_name=>Val}, UserAcc};
                <<"password">> ->
                    {BaseAcc, UserAcc#{password=>Val}};
                <<"email">> ->
                    {BaseAcc, UserAcc#{email=>Val}};
                <<"phone">> ->
                    {BaseAcc, UserAcc#{phone_t=>Val}};
                <<"address">> ->
                    {BaseAcc, UserAcc#{address_t=>Val}}
            end
        end,
        {#{}, #{}},
        maps:to_list(Params)),
    Obj1 = Base#{?DOMAIN_USER=>User},
    Obj2 = maps:merge(#{domain_id=>root}, Obj1),
    case nkdomain_user:create(Obj2) of
        {ok, #obj_id_ext{pid=Pid}=ObjIdExt, _} ->
            {ok, Obj} = nkdomain:get_obj(Pid),
            {ok, {ObjIdExt, Obj}};
        {error, Error} ->
            {error, Error}
    end.

