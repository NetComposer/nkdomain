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

%% @doc NkDomain OpenAPI support
%% https://swagger.io/docs/specification/about/

-module(nkdomain_openapi_schemas).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([actor_schema/5]).
-export([actor_schema_data/0]).
-export([search_post/0, search_delete/0]).
-import(nkdomain_openapi_util, [
            ref_schema/1, ref_parameter/1, make_ref/3, make_ref/4,
            ok_response/1, list_response/0]).

-include("nkdomain.hrl").


%% ===================================================================
%% Templates - Actor Schemas
%% ===================================================================

%% @doc 
actor_schema(SrvId, Group, Vsn, Type, Spec) ->
    {ok, Config} = nkdomain_actor_util:get_config(SrvId, Group, Type),
    #{camel:=Kind} = Config,
    DefDesc = <<"An Actor schema">>,
    Desc = maps:get(description, Spec, DefDesc),
    Properties1 = #{
        status => ref_schema("common.v1.ActorStatus")
    },
    Fields = maps:get(fields, Spec, #{}),
    Properties2 = maps:merge(Properties1, Fields),
    Schemas = maps:get(schemas, Spec, #{}),
    Required = maps:get(required, Spec, []),
    Schemas#{
        make_ref(Group, Vsn, Kind) => #{
            description => Desc,
            properties => Properties2#{
                apiVersion => #{
                    description => <<"The referred API Version">>,
                    type => string
                },
                kind => #{
                    description => <<"A string value representing the REST resource this object represents.">>,
                    type => string
                },
                metadata => ref_schema("common.v1.ActorMeta")
            },
            required => [apiVersion, kind, metadata, status | Required]
        },

        make_ref(Group, Vsn, Kind, "List") => #{
            description => <<"A list of ", Kind/binary>>,
            properties => #{
                apiVersion => #{
                    description => <<"The referred API Version">>,
                    type => string
                },
                kind => #{
                    description => <<"A string value representing the REST resource this object represents.">>,
                    type => string
                },
                items => #{
                    description => <<"List of actors">>,
                    type => array,
                    items => ref_schema([Group, $., Vsn, $., Kind])
                },
                metadata => ref_schema("common.v1.ListMeta")
            },
            required => [apiVersion, kind, items, metadata]
        }
    }.


%% @doc
actor_schema_data() ->
    #{
        description => <<"
            Configuration data. Data contains the configuration data.
            Each key must consist of alphanumeric characters, '-', '_' or '.'.">>,
        type => object,
        additionalProperties => #{
            type => string
        }
    }.


%% ===================================================================
%% Templates - Search
%% ===================================================================

search_post() ->
    #{
        description => <<"
            Launches an Actor query.

            It can optionally delete the full set of resulting actors, if
            parameter delete is used
            ">>,
        tags => [general],
        parameters => [
            ref_parameter("common.v1.ParamSearchDelete")
        ],
        requestBody => #{
            required => true,
            content => #{
                'application/json' => #{
                    schema => ref_schema(<<"common.v1.SearchBody">>)
                },
                'application/yaml' => #{
                    schema => ref_schema(<<"common.v1.SearchBody">>)
                }
            }
        },
        responses => list_response()
    }.


%% @private
search_delete() ->
    #{
        description => <<"Deletes Actors based on query.">>,
        tags => [general],
        requestBody => #{
            required => true,
            content => #{
                'application/json' => #{
                    schema => ref_schema(<<"common.v1.SearchBody">>)
                },
                'application/yaml' => #{
                    schema => ref_schema(<<"common.v1.SearchBody">>)
                }
            }
        },
        responses => ok_response("common.v1.Status")
    }.
