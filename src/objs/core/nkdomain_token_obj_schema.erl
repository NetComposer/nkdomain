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

%% @doc Token Object Schemas
-module(nkdomain_token_obj_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_execute/4, object_schema/1, object_query/3]).
-export([sample_all/0]).

-include("nkdomain.hrl").

%%-define(LLOG(Type, Txt, Args),
%%    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% API
%% ===================================================================


%% @doc 
object_execute(Field, _ObjIdExt, Token, _Args) ->
    case Field of
        <<"data">> -> {ok, maps:get(data, Token)};
        _ -> unknown_field
    end.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'Token' => #{
            type_class => nkobject,
            fields => #{
                data => string
            },
            comment => "A Token"
        }
    };

object_schema(queries) ->
    #{
        allTokens=> nkdomain_graphql_obj:schema_query_all_objs('Token')
    };


object_schema(_) ->
    #{}.


%% @doc
object_query(<<"allTokens">>, Params, _Ctx) ->
    Opts = #{
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"Token">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).



%% @private
sample_all() ->
    Query = <<"
        query {
            allTokens(
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
                        deviceSSOTokenIds
                        deviceSSOTokens {
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
