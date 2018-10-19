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

%% @doc NkDomain Domain OpenAPI
-module(nkdomain_domain_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_DOMAINS, #{}).


schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor that creates a new subdomain.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.DomainSpec")
        },
        schemas => #{
            'core.v1a1.DomainSpec' => #{
                description => <<"Specification for this actor.">>,
                properties => #{
                    httpPool => nkdomain_openapi_util:ref_schema("core.v1a1.DomainHttpPool")
                }
            },
            'core.v1a1.DomainHttpPool' => #{
                description => <<"
                    Each domain starts an http pool to be used by connections and
                    callbacks of its actors
                ">>,
                properties => #{
                    maxConnections => #{
                        description => <<"Maximum number of connections">>,
                        type => integer,
                        minimum => 1,
                        default => 10       % ?POOL_MAX_CONNECTIONS
                    },
                    timeout => #{
                        description => <<"Timeout for connections (msecs)">>,
                        type => integer,
                        minimum => 1,
                        default => 180000       % ?POOL_TIMEOUT
                    }
                }
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_DOMAINS, Spec).



