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
-export([filter_fields/0]).

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
            type_class => nkobject,
            fields => #{
                sessionLocal => string,
                sessionRemote => string,
                sessionUserId => {no_null, string},
                sessionUser => {no_null, 'User'}
            },
            comment => "An User Session"
        },
        'SessionConnection' => #{
            type_class => connection
        }
    };

object_schema(inputs) ->
    #{
        'SessionFilter' => #{
            fields => filter_fields(),
            comment => "Filter values to sort on"
        }
    };

object_schema(queries) ->
    #{
        allSessions=> nkdomain_graphql_obj:schema_query_all_objs('Session', 'Session', 'Object')
    };


object_schema(_) ->
    #{}.


%% @doc
object_query({connection, #obj_id_ext{type=?DOMAIN_USER, obj_id=UserObjId}}, Params, _Ctx) ->
    Opts = #{
        fields => #{
            <<"sessionLocal">> => [?DOMAIN_SESSION, local],
            <<"sessionRemote">> => [?DOMAIN_SESSION, remote],
            <<"sessionUserId">> => parent_id
        },
        filters => [
            #{<<"parent_id">> => #{<<"eq">> => UserObjId}},
            #{<<"type">> => #{<<"eq">> => {enum, <<"Session">>}}}
        ]
    },
    nkdomain_graphql_util:search(Params, Opts);

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
filter_fields() ->
    nkdomain_graphql_obj:object_fields_filter(#{
          sessionLocal => 'FilterKeyword',
          sessionRemote => 'FilterKeyword',
          sessionUserId => 'FilterKeyword'
      }).


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
