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

%% @doc NkDomain Config Actor GraphQL
-module(nkdomain_configmap_actor_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([config/0, schema/1, connections/1, query/5]).

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
        type => <<"ConfigMap">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_CONFIGMAPS
    }.


%%  @doc Generates new schema entries
schema(types) ->
    #{
        'ConfigMap' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                data => {list, 'Map'},
                status => 'ActorStatus'
            }),
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{}),
            comment => "A ConfigMap"
        }
    };

schema(inputs) ->
    #{
    };

schema(queries) ->
    #{
        allConfigMaps => nkdomain_graphql_schema:actor_query(<<"ConfigMap">>, #{})
    };

schema(_) ->
    #{}.


connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allConfigMaps">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"ConfigMap">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).



