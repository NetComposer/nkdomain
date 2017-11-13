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

-export([object_execute/4, object_schema/1, object_query/3, object_mutation/3]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, _ObjIdExt, User, Args) ->
    case Field of
        <<"userName">> -> {ok, maps:get(name, User, null)};
        <<"userSurname">> -> {ok, maps:get(surname, User, null)};
        <<"email">> -> {ok, maps:get(email, User, null)};
        <<"phone">> -> {ok, maps:get(phone_t, User, null)};
        <<"address">> -> {ok, maps:get(address_t, User, null)};
        <<"statusConnection">> -> {ok, get_status(User, Args)};
        <<"pushConnection">> -> {ok, get_push(User, Args)}
    end.



get_status(User, Args) ->
    StatusList1 = maps:get(status, User, []),
    StatusList2 = [
        #{<<"domainPath">>=>Path, <<"userStatus">>=>nklib_json:encode(US), <<"updatedTime">>=>Time} ||
        #{domain_path:=Path, user_status:=US, updated_time:=Time} <- StatusList1
    ],
    StatusList3 = [{ok, #{<<"cursor">>=><<>>, <<"node">>=>Node}} || Node <-StatusList2],
    StatusList4 = case Args of
        #{<<"last">>:=N} when N > 0, N < 99 ->
            lists:sublist(StatusList3, N);
        _ ->
            StatusList3
    end,
    #{
        <<"edges">> => StatusList4,
        <<"totalCount">> => length(StatusList2)
    }.


get_push(User, Args) ->
    PushList1 = maps:get(push, User, []),
    PushList2 = [
        #{<<"domainPath">>=>Path, <<"pushData">>=>nklib_json:encode(Data),
          <<"deviceId">>=>DeviceId, <<"updatedTime">>=>Time}
        ||
        #{domain_path:=Path, device_id:=DeviceId, push_data:=Data, updated_time:=Time} <- PushList1
    ],
    PushList3 = [{ok, #{<<"cursor">>=><<>>, <<"node">>=>Node}} || Node <-PushList2],
    PushList4 = case Args of
        #{<<"last">>:=N} when N > 0, N < 99 ->
            lists:sublist(PushList3, N);
        _ ->
            PushList3
    end,
    #{
        <<"edges">> => PushList4,
        <<"totalCount">> => length(PushList2)
    }.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'User' => #{
            fields => #{
                userName => {string, #{comment=>"User family name"}},
                userSurname => {string, #{comment=>"User surname"}},
                email => string,
                phone => string,
                address => string,
                status => {connection_last, 'UserStatus', #{comment => "User current statuses"}},
                push => {connection_last, 'UserPush', #{comment => "User current statuses"}}
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
        },
        'UserPush' => #{
            fields => #{
                domainPath => {no_null, string, #{comment=>"Domain this push data belongs to"}},
                deviceId => {no_null, string},
                pushData => string,
                updatedTime => time
            },
            is_connection => true
        },
        'UserSearchResult' => #{
            fields => #{
                objects => {list_no_null, 'User', #{comment => "My Objects"}},
                pageInfo => {no_null, 'PageInfo'},
                totalCount => int
            }
        }
    };



object_schema(inputs) ->
    #{
        objectUserFilter => #{
            fields => nkdomain_graphql_obj:object_fields_filter(#{
                userName => {objectFilterNorm, #{comment => "User name"}},
                userSurname => {objectFilterNorm, #{comment => "User surname"}},
                email => {objectFilterKeyword, #{comment => "User email"}},
                phone => {objectFilterKeyword, #{comment => "User phone"}},
                address => {objectFilterKeyword, #{comment => "User address"}}
            }),
            comment => "Filter values to sort on"
        },
        objectUserSort => #{
            fields => nkdomain_graphql_obj:object_fields_sort([userName, userSurname, email, phone]),
            comment => "Fields to sort on"
        }
    };

object_schema(queries) ->
    #{
        allUsers => nkdomain_graphql_obj:query_all_objs('User')
    };


object_schema(mutations) ->
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
    };

object_schema(_) ->
    #{}.


%% @doc
object_query(<<"allUsers">>, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"userName">> => {norm, [?DOMAIN_USER, name]},
            <<"userSurname">> => {norm, [?DOMAIN_USER, surname]},
            <<"email">> => [?DOMAIN_USER, email],
            <<"phone">> => [?DOMAIN_USER, phone],
            <<"address">> => [?DOMAIN_USER, address_t]
        },
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"User">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).



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

