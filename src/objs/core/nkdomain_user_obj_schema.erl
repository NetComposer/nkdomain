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

-export([object_execute/5, object_schema/1, object_query/3, object_mutation/3]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, ObjIdExt, User, Args, Ctx) ->
    case Field of
        <<"userName">> -> {ok, maps:get(name, User, null)};
        <<"userSurname">> -> {ok, maps:get(surname, User, null)};
        <<"email">> -> {ok, maps:get(email, User, null)};
        <<"phone">> -> {ok, maps:get(phone_t, User, null)};
        <<"address">> -> {ok, maps:get(address_t, User, null)};
        <<"userStatusConnection">> -> {ok, get_status(User, Args)};
        <<"userPushConnection">> -> {ok, get_push(User, Args)};
        _ ->
            case binary:split(Field, <<"Connection">>) of
                [BaseType1, _] ->
                    BaseType2 = nklib_util:to_capital(BaseType1),
                    case nkdomain_reg:get_schema_type_module(BaseType2) of
                        undefined ->
                            null;
                        Module ->
                            Module:object_query({connection, ObjIdExt}, Args, Ctx)
                    end
            end
    end.


%% @doc 
get_status(User, #{<<"last">>:=Last}) when Last > 0, Last < 99 ->
    Status = maps:get(status, User, []),
    Objs1 = [
        #{<<"domainPath">>=>Path, <<"userStatus">>=>nklib_json:encode(US), <<"updatedTime">>=>Time} ||
        #{domain_path:=Path, user_status:=US, updated_time:=Time} <- Status
    ],
    Objs2 = lists:sublist(Objs1, Last),
    #{
        <<"objects">> => [{ok, E} || E <- Objs2],
        <<"totalCount">> => length(Objs1)
    }.


%% @doc 
get_push(User, #{<<"last">>:=Last}) when Last > 0, Last < 99 ->
    Push = maps:get(push, User, []),
    Objs1 = [
        #{<<"domainPath">>=>Path, <<"pushData">>=>nklib_json:encode(Data),
          <<"deviceId">>=>DeviceId, <<"updatedTime">>=>Time}
        ||
        #{domain_path:=Path, device_id:=DeviceId, push_data:=Data, updated_time:=Time} <- Push
    ],
    Objs2 = lists:sublist(Objs1, Last),
    #{
        <<"objects">> => [{ok, E} || E <- Objs2],
        <<"totalCount">> => length(Objs1)
    }.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'User' => #{
            type_class => nkobject,
            fields => #{
                userName => {string, #{comment=>"User family name"}},
                userSurname => {string, #{comment=>"User surname"}},
                email => string,
                phone => string,
                address => string,
                userStatus => {connection, 'UserStatus', #{
                                last => {int, #{default=>10}},
                                comment => "User current statuses"}},
                userPush => {connection, 'UserPush', #{
                                last => {int, #{default=>10}},
                                comment => "User current statuses"}}
            },
            comment => "An User"
        },
        'UserStatus' => #{
            fields => #{
                domainPath => {no_null, string, #{comment=>"Domain this status belongs to"}},
                userStatus => string,
                updatedTime => time
            }
        },
        'UserStatusConnection' => #{
            type_class => connection
        },
        'UserPush' => #{
            fields => #{
                domainPath => {no_null, string, #{
                                    comment=>"Domain this push data belongs to"}},
                deviceId => {no_null, string},
                pushData => string,
                updatedTime => time
            }
        },
        'UserPushConnection' => #{
            type_class => connection
        }
    };

object_schema(inputs) ->
    #{
        'UserFilter' => #{
            fields => nkdomain_graphql_obj:object_fields_filter(#{
                userName => {'FilterNormalizedString', #{comment => "User name"}},
                userSurname => {'FilterNormalizedString', #{comment => "User surname"}},
                email => {'FilterKeyword', #{comment => "User email"}},
                phone => {'FilterKeyword', #{comment => "User phone"}},
                address => {'FilterKeyword', #{comment => "User address"}}
            }),
            comment => "Filter values to sort on"
        },
        'UserSort' => #{
            fields => nkdomain_graphql_obj:schema_object_fields_sort([userName, userSurname, email, phone]),
            comment => "Fields to sort on"
        }
    };

object_schema(queries) ->
    #{
        allUsers => nkdomain_graphql_obj:schema_query_all_objs('User', 'Device', 'Device')
    };


object_schema(mutations) ->
    #{
        introduceUser => #{
            input => #{
                domain => string,
                objName => string,
                userName => {no_null, string},
                userSurname => {no_null, string},
                password => string,
                email => {no_null, string},
                phone => string,
                address => string
            },
            output => #{
                objId => {no_null, string},
                domain => {no_null, string},
                objName => {no_null, string},
                path => {no_null, string},
                userName => {no_null, string},
                userSurname => {no_null, string},
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


%% @doc
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

