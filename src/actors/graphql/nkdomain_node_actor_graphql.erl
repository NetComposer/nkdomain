%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain ClusterNode Actor GraphQL
-module(nkdomain_node_actor_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([config/0, schema/1, connections/1, query/5, execute/5]).
-import(nkdomain_graphql_execute, [get_value/2, get_map/2, get_time/3, get_object/2]).

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
        type => <<"ClusterNode">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_NODES
    }.


%%  @doc Generates new schema entries
schema(types) ->
    #{
        'ClusterNodeSpec' => #{
            fields => #{
                ttlSecs => {integer, #{comment=>"ClusterNode TTL in secs"}}
            }
        },
        'ClusterNode' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => {no_null, 'ClusterNodeSpec'},
                status => 'ActorStatus'
            }),
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{}),
            comment => "A ClusterNode"
        }
    };

schema(inputs) ->
    #{};

schema(queries) ->
    #{
        allClusterNodes => nkdomain_graphql_schema:actor_query(<<"ClusterNode">>, #{})
    };

schema(_) ->
    #{}.


connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allClusterNodes">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"Node">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).


%% @private
execute(_SrvId, Field, {nkdomain, {spec, _Type, Spec, _Actor}}, _Meta, _Params) ->
    get_value(Field, Spec);

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.

