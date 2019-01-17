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

%% @doc NkDomain Access Id OpenAPI
-module(nkdomain_access_id_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1, parameters/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    Spec = #{
        create => #{
            description => <<"
                Creates an Access Id Actor.
                Netcomposer verb is 'create'.
            ">>
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_ACCESS_IDS, Spec).



schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to represent a unique Access Id.">>,
        fields => #{
            data => nkdomain_openapi_schemas:actor_schema_data()
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_TOKENS, Spec).



parameters(_SrvId) ->
    #{
    }.