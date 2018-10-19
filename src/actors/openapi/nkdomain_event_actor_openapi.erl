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

%% @doc NkDomain Event OpenAPI
-module(nkdomain_event_actor_openapi).
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
                fieldSelector => <<"core.v1a1.EventParamFieldSelector">>,
                sort => <<"core.v1a1.EventParamSort">>
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_EVENTS, Spec).


schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor representing an Event happening on the system.">>,
        fields => #{
            reason => #{
                description => <<"
                    Reason of the event.
                    Common reason are:

                    * 'ActorCreated'
                    * 'ActorUpdated'
                    * 'ActorDeleted'
                ">>,
                type => string
            },
            message => #{
                description => <<"Textual representation of the reason.">>,
                type => string
            },
            source => nkdomain_openapi_util:ref_schema("core.v1a1.EventSource"),
            firstTimestamp => #{
                description => <<"Timestamp of the first time this event was seen in a time slot.">>,
                type => string,
                format => 'date-time'
            },
            lastTimestamp => #{
                description => <<"Timestamp of the last time this event was seen in a time slot.">>,
                type => string,
                format => 'date-time'
            },
            count => #{
                description => <<"Number of times this events was fired in the time slot.">>,
                type => integer,
                minimum => 1
            },
            involvedObject => nkdomain_openapi_util:ref_schema("core.v1a1.EventInvolvedObject")
        },
        required => [reason, source, firstTimestamp, lastTimestamp, count, involvedObject],
        schemas => #{
            'core.v1a1.EventInvolvedObject' => #{
                description => <<"Data of the actor generating the event">>,
                properties => #{
                    apiVersion => #{
                        description => <<"The referred API Version.">>,
                        type => string
                    },
                    kind => #{
                        description => <<"A string representing the REST resource this object represents. In CamelCase.">>,
                        type => string
                    },
                    name => #{
                        description => <<"Name of the actor's name">>,
                        type => string
                    },
                    resourceVersion => #{
                        description => <<"Actor's resource version">>,
                        type => string
                    },
                    domain => #{
                        description => <<"Domain this actor belongs to. Cannot be updated.">>,
                        type => string
                    },
                    uid => #{
                        description => <<"Actor's UID">>,
                        type => string
                    },
                    subtype => #{
                        description => <<"Optional sub-classification of the Actor.">>,
                        type => string
                    }
                },
                required => [apiVersion, kind, name, domain, uid]
            },
            'core.v1a1.EventSource' => #{
                description => <<"Source of the Event.">>,
                properties => #{
                    component => #{
                        description => <<"Component of the system generating the event.">>,
                        type => string
                    },
                    host => #{
                        description => <<"Host generating the event.">>,
                        type => string
                    }
                },
                required => [component, host]
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_EVENTS, Spec).


parameters(_SrvId) ->
    Filter = [
        <<"reason">>,
        <<"count">>,
        <<"firstTimestamp">>,
        <<"lastTimestamp">>,
        <<"involvedObject.uid">>,
        <<"involvedObject.domain">>,
        <<"involvedObject.apiVersion">>,
        <<"involvedObject.kind">>,
        <<"involvedObject.name">>,
        <<"involvedObject.subtype">>,
        <<"involvedObject.resourceVersion">>
    ],
    Sort = [
        <<"reason">>,
        <<"count">>,
        <<"firstTimestamp">>,
        <<"lastTimestamp">>,
        <<"involvedObject.domain">>,
        <<"involvedObject.apiVersion">>,
        <<"involvedObject.kind">>,
        <<"involvedObject.name">>,
        <<"involvedObject.subtype">>
    ],
    #{
        'core.v1a1.EventParamFieldSelector' =>
            nkdomain_openapi_util:filter_parameter(Filter),

        'core.v1a1.EventParamSort' =>
            nkdomain_openapi_util:sort_parameter(Sort)
    }.