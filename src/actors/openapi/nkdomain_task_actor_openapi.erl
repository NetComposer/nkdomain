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

%% @doc NkDomain Task OpenAPI
-module(nkdomain_task_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1, parameters/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    Spec = #{
        list => #{
            parameters => #{
                fieldSelector => <<"core.v1a1.TaskParamFieldSelector">>,
                sort => <<"core.v1a1.TaskParamSort">>
            }
        },
        create => #{
            description => <<"
                Creates a Task Actor.
                You will typically use the field `metadata.subtype` to represent a
                recognized type of task, and `spec.job` to set any additional data
                that subtype needs.

                Netcomposer verb is 'create'.
            ">>,
            callbacks => #{
                onTaskStatus => cb_on_task_status()
            }
        },
        subpaths => #{
            state => #{
                update => #{
                    description => <<"
                        Updates the task state.
                        Only the fields status and progress can be updated.
                    ">>,
                    request_schema => <<"core.v1a1.TaskStatus">>
                }
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_TASKS, Spec).



%% @private
cb_on_task_status() ->
    #{
        '{$request.body#/spec.callbackUrl}' => #{
            post => #{
                requestBody => #{
                    description => <<"subscription payload">>,
                    content => #{
                        'application/json' => #{
                            schema => nkdomain_openapi_util:ref_schema("core.v1a1.TaskCallback")
                        }
                    }
                },
                responses => #{
                    '202' => #{
                        description => <<>>
                    }
                }
            }
        }
    }.


schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to represent a specific time living token.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.TaskSpec"),
            status => nkdomain_openapi_util:ref_schema("core.v1a1.TaskStatus")
        },
        required => [spec],
        schemas => #{
            'core.v1a1.TaskSpec' => #{
                description => <<"Task Actor specification.">>,
                properties => #{
                    job => #{
                        description => <<"Specific job information">>,
                        type => object,
                        additionalProperties => true
                    },
                    maxTries => #{
                        description => <<"
                            Maximum number of task restart attempts.
                            After that number of retries, the task will be marked as
                            failed and deleted.">>,
                        type => integer,
                        minimum => 1,
                        default => 3
                    },
                    maxSecs => #{
                        description => <<"
                            Maximum duration for the task (including all attemmpts)
                            Adter that number of seconds, the task will be marked as
                            failed and deleted.">>,
                        type => integer,
                        minimum => 1,
                        default => 60
                    },
                    callbackUrl => #{
                        description => <<>>,
                        type => string,
                        format => uri,
                        example => <<"http:/a.b.c">>
                    }
                },
                required => [job]
            },
            'core.v1a1.TaskStatus' => #{
                description => <<"Task Actor current status.">>,
                properties => #{
                    lastTryStartTime => #{
                        description => <<"Timestamp of last Task start try">>,
                        type => string,
                        format => datetime
                    },
                    tries => #{
                        description => <<"Number of attempts so far">>,
                        type => integer
                    },
                    status => #{
                        description => <<"
                            If activated, current status of the task
                            (start, progress, error, success, faillure).">>,
                        type => string,
                        enum => [start, progress, error, success, faillure]
                    },
                    progress => #{
                        description => <<"Current % of completion">>,
                        type => integer
                    }
                }
            },
            'core.v1a1.TaskCallback' => #{
                description => <<"Task callback status.">>,
                properties => #{
                    statusInfo => nkdomain_openapi_util:ref_schema("core.v1a1.TaskStatus"),
                    uid => #{
                        description => <<"Current % of completion">>,
                        type => integer
                    }
                }
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_TASKS, Spec).



parameters(SrvId) ->
    {Filter, Sort} = nkdomain_openapi_util:filter_parameters(SrvId, "core", "v1a1", ?RES_CORE_TASKS),
    #{
        'core.v1a1.TaskParamFieldSelector' =>
        nkdomain_openapi_util:filter_parameter(Filter),

        'core.v1a1.TaskParamSort' =>
        nkdomain_openapi_util:sort_parameter(Sort)
    }.