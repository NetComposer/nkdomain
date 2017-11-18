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

%% @doc Session Object Schemas
-module(nkdomain_session_obj_schema).
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
object_execute(Field, _ObjIdExt, #{?DOMAIN_SESSION:=SessObj}=Session, _Args) ->
    case Field of
        <<"sessionLocal">> -> {ok, maps:get(local, SessObj, null)};
        <<"sessionRemote">> -> {ok, maps:get(remote, SessObj, null)};
        <<"sessionUserId">> -> {ok, maps:get(parent_id, Session)};
        <<"sessionUser">> -> nkdomain_graphql_util:get_obj(maps:get(parent_id, Session))
    end.


%%  @doc Generates new schema entries
object_schema(types) ->
    #{
        'Session' => #{
            fields => #{
                sessionLocal => string,
                sessionRemote => string,
                sessionUserId => {no_null, string},
                sessionUser => {no_null, 'User'}
            },
            is_object => true,
            comment => "An User Session"
        },
        'SessionSearchResult' => #{
            fields => #{
                objects => {list_no_null, 'Session', #{comment => "My Objects"}},
                pageInfo => {no_null, 'PageInfo'},
                totalCount => int
            }
        }
    };

object_schema(inputs) ->
    #{
        objectSessionFilter => #{
            fields => nkdomain_graphql_obj:object_fields_filter(#{
                sessionLocal => objectFilterKeyword,
                sessionRemote => objectFilterKeyword,
                sessionUserId => objectFilterKeyword
            }),
            comment => "Filter values to sort on"
        },
        objectSessionSort => #{
            fields => nkdomain_graphql_obj:schema_object_fields_sort([]),
            comment => "Fields to sort on"
        }
    };

object_schema(queries) ->
    #{
        allSessions=> nkdomain_graphql_obj:schema_query_all_objs('Session')
    };


object_schema(_) ->
    #{}.


%% @doc
object_query(<<"allSessions">>, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"sessionLocal">> => [?DOMAIN_SESSION, local],
            <<"sessionRemote">> => [?DOMAIN_SESSION, remote],
            <<"sessionUserId">> => parent_id
        },
        filters => [
            #{<<"type">> => #{<<"eq">> => {enum, <<"Session">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts).



%% @private
sample_all() ->
    Query = <<"
        query {
            allSessions(
                sort: [
                    {
                        path: {order: ASC}
                    }
                ],
                filter: [
                    {
                        sessionUserId: {prefix: \"adm\"}
                    }
                ])
                {
                    totalCount
                    objects {
                        sessionLocal
                        sessionRemote
                        sessionUserId
                        sessionUser {
                            objName
                        }
                    }
                }
        }">>,
   request(Query).




%% @private
request(Query) ->
    nkdomain_graphql:request(?NKROOT, Query, #{}).
