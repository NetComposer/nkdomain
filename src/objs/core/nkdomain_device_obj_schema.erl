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

%% @doc Device Object Schemas
-module(nkdomain_device_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/4, object_schema/1, object_query/3, object_mutation/3]).
-export([sample_all/0]).

-include("nkdomain.hrl").

%%-define(LLOG(Type, Txt, Args),
%%    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, ObjIdExt, Device, _Args) ->
    case Field of
        <<"deviceUUID">> -> {ok, maps:get(device_uuid, Device, null)};
        <<"deviceSSODeviceIds">> -> {ok, get_sso_device_ids(Device)};
        <<"deviceSSODevices">> -> {ok, get_sso_devices(Device)};
        <<"deviceCurrentUser">> -> get_user(ObjIdExt);
        _ -> unknown_field
    end.


%% @private
get_sso_device_ids(Device) ->
    [{ok, Id} || Id <- maps:get(sso_device_ids, Device, [])].


%% @private
get_sso_devices(Device) ->
    [nkdomain_graphql_util:get_obj(Id) || {ok, Id} <- get_sso_device_ids(Device)].


%% @private
get_user(#obj_id_ext{pid=Pid}) when is_pid(Pid) ->
    case nkdomain_device:get_registered_user(Pid) of
        {ok, UserId} ->
            nkdomain_graphql_util:get_obj(UserId);
        _ ->
            null
    end;

get_user(_) ->
    null.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'Device' => #{
            type_class => nkobject,
            fields => #{
                deviceUUID => string,
                deviceSSODeviceIds => {list, string},
                deviceSSODevices => {list, 'Device'},
                deviceCurrentUser => {'User', #{comment => "User currently associated to this device"}}
            },
            comment => "A Device"
        }
    };

object_schema(inputs) ->
    #{
        'DeviceFilter' => #{
            fields => nkdomain_graphql_obj:object_fields_filter(#{
                deviceUUID => 'FilterKeyword',
                deviceSSODeviceId => 'FilterKeyword'
            }),
            comment => "Filter values to sort on"
        },
        'DeviceSort' => #{
            fields => nkdomain_graphql_obj:schema_object_fields_sort([]),
            comment => "Fields to sort on"
        }
    };

object_schema(queries) ->
    #{
        allDevices=> nkdomain_graphql_obj:schema_query_all_objs('Device', 'Device', 'Device')
    };


object_schema(mutations) ->
    #{
        introduceDevice => #{
            input => #{
                domain => string,
                objName => string,
                deviceUUID => {no_null, string},
                deviceSSODeviceIds => {list, string}
            },
            output => #{
                objId => {no_null, string},
                domain => {no_null, string},
                objName => {no_null, string},
                path => {no_null, string},
                deviceUUID => {no_null, string},
                deviceSSODeviceIds => {list, string}
            },
            comment => "Creates a new user"
        }
    };

object_schema(_) ->
    #{}.


%% @doc
object_query(<<"allDevices">>, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"deviceUUID">> => [?DOMAIN_DEVICE, device_uuid],
            <<"deviceSSODeviceId">> => [?DOMAIN_DEVICE, sso_device_ids]
        },
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"Device">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).


%% @doc
object_mutation(<<"introduceDevice">>, Params, _Ctx) ->
    {Base, Device} = lists:foldl(
        fun({Key, Val}, {BaseAcc, DeviceAcc}) ->
            case Key of
                <<"userName">> ->
                    {BaseAcc, DeviceAcc#{name=>Val}};
                <<"userSurname">> ->
                    {BaseAcc, DeviceAcc#{surname=>Val}};
                <<"domain">> ->
                    {BaseAcc#{domain_id=>Val}, DeviceAcc};
                <<"objName">> ->
                    {BaseAcc#{obj_name=>Val}, DeviceAcc};
                <<"password">> ->
                    {BaseAcc, DeviceAcc#{password=>Val}};
                <<"email">> ->
                    {BaseAcc, DeviceAcc#{email=>Val}};
                <<"phone">> ->
                    {BaseAcc, DeviceAcc#{phone_t=>Val}};
                <<"address">> ->
                    {BaseAcc, DeviceAcc#{address_t=>Val}}
            end
        end,
        {#{}, #{}},
        maps:to_list(Params)),
    Obj1 = Base#{?DOMAIN_USER=>Device},
    Obj2 = maps:merge(#{domain_id=>root}, Obj1),
    case nkdomain_user:create(Obj2) of
        {ok, #obj_id_ext{pid=Pid}=ObjIdExt, _} ->
            {ok, Obj} = nkdomain:get_obj(Pid),
            {ok, {ObjIdExt, Obj}};
        {error, Error} ->
            {error, Error}
    end.





%% @private
sample_all() ->
    Query = <<"
        query {
            allDevices(
                sort: [
                    {
                        path: {order: ASC}
                    }
                ],
                filter: [
                    {
                        objName: {prefix: \"device2\"}
                    }
                ])
                {
                    totalCount
                    objects {
                        objName
                        deviceUUID
                        deviceSSODeviceIds
                        deviceSSODevices {
                           objName
                        }
                        deviceCurrentUser {
                            objName
                        }
                    }
                }
        }">>,
   request(Query).




%% @private
request(Query) ->
    nkdomain_graphql:request(?NKROOT, Query, #{}).
