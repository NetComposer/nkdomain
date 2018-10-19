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

%% @doc NkDomain User OpenAPI
-module(nkdomain_user_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1, parameters/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    Spec = #{
        subpaths => #{
            '_checkpass' => #{
                get => #{
                    parameters => #{
                        password => <<"core.v1a1.UserParamPassword">>
                    },
                    description => <<"
                        Checks an user password.
                        For a OK reply, in `Status`, `reason` would be either 'password_valid' or 'password_invalid'
                    ">>
                }
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_USERS, Spec).



schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to create File Providers to store files in the system.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.UserSpec")
        },
        required => [spec],
        schemas => #{
            'core.v1a1.UserSpec' => #{
                description => <<"Specification for this actor.">>,
                properties => #{
                    password =>#{
                        description => <<"Password for the user. Only a hash of it will be stored.">>,
                        type => string
                    }
                }
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_USERS, Spec).



parameters(_SrvId) ->
    [
        #{
            'core.v1a1.UserParamPassword' => #{
                name => <<"password">>,
                description => <<"Password user in base64 form">>,
                in => query,
                schema => #{
                    type => string
                },
                required => true
            }
        }
    ].