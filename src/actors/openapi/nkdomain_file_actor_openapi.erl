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

%% @doc NkDomain File OpenAPI
-module(nkdomain_file_actor_openapi).
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
                fieldSelector => <<"core.v1a1.FileParamFieldSelector">>,
                sort => <<"core.v1a1.FileParamSort">>
            }
        },
        get => #{
            parameters => #{
                getBodyInline => <<"core.v1a1.FileParamGetBodyInline">>
            }
        },
        subpaths => #{
            '_download' => #{
                get => #{
                    description => <<"
                        Download the binary of a file directly.
                        A 'content-type' header will be generated.
                    ">>,
                    responses =>
                        nkdomain_openapi_util:binary_response()
                }
            },
            '_rpc/downloadLink' => #{
                get => #{
                    description => <<"
                        Generates a temporary link for direct download of the file
                        from an S3 store.
                        TTL will be based on provider's config.
                    ">>,
                    responses =>
                    nkdomain_openapi_util:ok_response("core.v1a1.FileDownloadLinkS3")
                }
            }
        },
        subresources => #{
            '_upload' => #{
                create => #{
                    description => <<"
                        Creates a file by uploading the binary directly instead
                        of using the field 'bodyBase64'.

                        Must include a header with the 'Content-Type' and
                        use the 'provider' parameter
                    ">>,
                    parameters => #{
                        provider => <<"core.v1a1.FileParamProvider">>,
                        contentType => <<"core.v1a1.FileParamContentType">>
                    },
                    request =>  #{
                        required => true,
                        content => #{
                            '*/*' => #{
                                schema => #{
                                    type => string,
                                    format => binary
                                }
                            }
                        }
                    },
                    responses => default
                }
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_FILES, Spec).


schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to store files in the system.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.FileSpec")
        },
        required => [spec],
        schemas => #{
            'core.v1a1.FileSpec' => #{
                description => <<"
                    Specification for this actor.
                    Before creating a `File`, a `FileProvider` object must be created and
                    used in the `provider`field.

                    `Field` actors will always have a `link` of class `fileproviders`
                    pointing to its provider.">>,
                properties => #{
                    name => #{
                        description => <<"Name of the file. If not defined, it will the same as `metadata.name`">>,
                        type => string
                    },
                    contentType => #{
                        description => <<"Standarized content type">>,
                        type => string
                    },
                    size => #{
                        description => <<"Size of the file. Will be calculated at the server">>,
                        type => string
                    },
                    hash => #{
                        description => <<"
                            If the FileProvider is configured properly, a hash of the file
                            will be generated at the server. ">>,
                        type => string
                    },
                    password => #{
                        description => <<"
                            If the FileProvider is configured properly, a new password will
                            be generated and use to encrypt the file.">>,
                        type => string
                    },
                    provider => #{
                        description => <<"UID or `selfLink` of the FileProvider this file belongs to">>,
                        type => string
                    },
                    bodyBase64 => #{
                        description => <<"
                            Can be used when creating the file, however it will be decoded
                            and extracted into a real file, and this field will disappear.">>,
                        type => string
                    },
                    externalId => #{
                        description => <<"
                            Id used on the storage system.
                            Can also be used on creation if the file is already in storage
                            system (see documentation for S3 File Provider
                        ">>,
                        type => string
                    },
                    url => #{
                        description => <<"
                            If used, the server will try to download this resource and
                            copy it over the specified provider. Content-Type and size
                            will be taken from the resource Headers
                        ">>,
                    type => string
                }
                },
                required => [contentType, provider]
            },
            'core.v1a1.FileDownloadLinkS3' => #{
                description => <<"Link to download file.">>,
                properties => #{
                    url => #{
                        description => <<"HTTP url to use.">>,
                        type => string,
                        format => uri
                    },
                    ttlSecs => #{
                        description => <<"Time for which this token is valid.">>,
                        type => integer
                    }
                },
                required => [url, ttlSecs]
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_FILES, Spec).


parameters(SrvId) ->
    {Filter, Sort} = nkdomain_openapi_util:filter_parameters(SrvId, "core", "v1a1", ?RES_CORE_FILES),
    #{
        'core.v1a1.FileParamFieldSelector' =>
            nkdomain_openapi_util:filter_parameter(Filter),

        'core.v1a1.FileParamSort' =>
            nkdomain_openapi_util:sort_parameter(Sort),

        'core.v1a1.FileParamGetBodyInline' => #{
            name => getBodyInline,
            description => <<"If true, body will be included in field `bodyBase64`">>,
            in => query,
            schema => #{
                type => boolean,
                default => false
            }
        },
        'core.v1a1.FileParamProvider' => #{
            name => provider,
            description => <<"Provider ID">>,
            in => query,
            required => true,
            schema => #{
                type => string
            }
        },
        'core.v1a1.FileParamContentType' => #{
            name => 'content-type',
            description => <<"File's MIME content-type">>,
            in => header,
            required => true,
            schema => #{
                type => string
            }
        }
    }.
