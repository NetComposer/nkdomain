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

%% @doc Domain Object Schemas
-module(nkdomain_domain_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/4, object_schema/1, object_query/3]).
-export([sample_all/0]).

-include("nkdomain.hrl").


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, _ObjIdExt, Domain, _Args) ->
    case Field of
        <<"config">> -> {ok, maps:get(config, Domain, #{})}
    end.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'Domain' => #{
            type_class => nkobject,
            fields => #{
                config => string
            },
            comment => "A Domain"
        }
    };

object_schema(queries) ->
    #{
        allDomains => nkdomain_graphql_obj:schema_query_all_objs('Domain')
    };


object_schema(_) ->
    #{}.


%% @doc
object_query(<<"allDomains">>, Params, _Ctx) ->
    Opts = #{
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"Domain">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).




%% @private
sample_all() ->
    Query = <<"
        query {
            allDomains(
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
                        deviceSSODomainIds
                        deviceSSODomains {
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
