%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain User Actor GraphQL
-module(nkdomain_user_actor_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([config/0, schema/1, connections/1, query/5, mutation/5, execute/5]).

-behavior(nkservice_graphql_schema).

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================

config() ->
    #{
        type => <<"User">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_USERS
    }.


%%  @doc Generates new schema entries
schema(types) ->
    #{
        'UserSpec' => #{
            fields => #{
                password => {string, #{comment=>"Hashed password"}}
            }
        },
        'User' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => 'UserSpec',
                status => 'ActorStatus'
            }),
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{}),
            comment => "An User"
        }
    };

schema(inputs) ->
    #{
    };

schema(queries) ->
    #{
        allUsers => nkdomain_graphql_schema:actor_query(<<"User">>, #{})
    };


schema(mutations) ->
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

schema(_) ->
    #{}.


%% @private
connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allUsers">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"User">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).


mutation(_SrvId, _Name, _Params, _Meta, _Ctx) ->
    continue.


execute(_SrvId, _Field, _Obj, _Meta, _Args) ->
    continue.

