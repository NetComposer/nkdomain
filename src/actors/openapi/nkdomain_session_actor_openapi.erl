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

%% @doc NkDomain Session OpenAPI
-module(nkdomain_session_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    Spec = #{
        subpaths => #{
            '_rpc/refresh' => #{
                get => #{
                    description => <<"
                        Refresh an user password for the initial number of seconds.
                    ">>
                }
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_SESSIONS, Spec).


schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to represent an User session.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.SessionSpec")
        },
        required => [spec],
        schemas => #{
            'core.v1a1.SessionSpec' => #{
                description => <<"Specification for this actor.">>,
                properties => #{
                    ttlSecs => #{
                        description => <<"Seconds the session can survive between refreshes">>,
                        type => integer,
                        minimum => 1
                    }
                }
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_SESSIONS, Spec).
