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

%% @doc Location Object Schemas
-module(nkdomain_location_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/4, object_schema/1, object_query/3, object_mutation/3]).
-export([sample_all/0]).

-include("nkdomain.hrl").


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(_Field, _ObjIdExt, _Location, _Args) ->
    null.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'Location' => #{
            type_class => nkobject,
            fields => #{},
            comment => "A Location"
        }
    };

object_schema(queries) ->
    #{
        allLocations => nkdomain_graphql_obj:schema_query_all_objs('Location')
    };


object_schema(_) ->
    #{}.


%% @doc
object_query(<<"allLocations">>, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"deviceUUID">> => [?DOMAIN_DEVICE, device_uuid],
            <<"deviceSSOLocationId">> => [?DOMAIN_DEVICE, sso_device_ids]
        },
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"Location">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).


%% @doc
object_mutation(<<"introduceLocation">>, Params, _Ctx) ->
    {Base, Location} = lists:foldl(
        fun({Key, Val}, {BaseAcc, LocationAcc}) ->
            case Key of
                <<"userName">> ->
                    {BaseAcc, LocationAcc#{name=>Val}};
                <<"userSurname">> ->
                    {BaseAcc, LocationAcc#{surname=>Val}};
                <<"domain">> ->
                    {BaseAcc#{domain_id=>Val}, LocationAcc};
                <<"objName">> ->
                    {BaseAcc#{obj_name=>Val}, LocationAcc};
                <<"password">> ->
                    {BaseAcc, LocationAcc#{password=>Val}};
                <<"email">> ->
                    {BaseAcc, LocationAcc#{email=>Val}};
                <<"phone">> ->
                    {BaseAcc, LocationAcc#{phone_t=>Val}};
                <<"address">> ->
                    {BaseAcc, LocationAcc#{address_t=>Val}}
            end
        end,
        {#{}, #{}},
        maps:to_list(Params)),
    Obj1 = Base#{?DOMAIN_USER=>Location},
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
            allLocations(
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
                        deviceSSOLocationIds
                        deviceSSOLocations {
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
