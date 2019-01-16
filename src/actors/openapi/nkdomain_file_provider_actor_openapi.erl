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

%% @doc NkDomain File OpenAPI
-module(nkdomain_file_provider_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1, parameters/1]).

-include("nkdomain.hrl").
-include_lib("nkfile/include/nkfile.hrl").

%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    Spec = #{
        list => #{
            parameters => #{
                fieldSelector => <<"core.v1a1.FileProviderParamFieldSelector">>,
                sort => <<"core.v1a1.FileProviderParamSort">>
            }
        },
        subpaths => #{
            '_rpc/uploadLink' => #{
                get => #{
                    description => <<"
                        Generates a temporary upload link, directly to an S3 provider.
                        It will be active only for the configured time in the provider.
                        You will get an HTTP method, an URL and an Id. After uploading
                        the file, you must create the 'File' actor using the field
                        'ExternalId'.
                     ">>,
                    parameters => #{
                        contentType => <<"core.v1a1.FileProviderParamContentType">>
                    },
                    responses =>
                        nkdomain_openapi_util:ok_response("core.v1a1.FileProviderUploadLinkS3")
                }
            }
        },
        subresources => #{
            '{actorName}/files' => #{
                create => #{
                    description => <<"
                        Creates a file, belonging to this FileProvider, so it
                        doens't need to be included in body
                    ">>,
                    parameters => #{
                        actorName => <<"common.v1.ParamActorName">>
                    },
                    request_schema => <<"core.v1a1.File">>,
                    responses => nkdomain_openapi_util:created_response("core.v1a1.File")
                }
            },
            '{actorName}/files/_upload' => #{
                create => #{
                    description => <<"
                        Creates a file by uploading the binary directly instead
                        of using the field 'bodyBase64', belonging to this FileProvider,

                        Must include a header with the 'Content-Type'.
                    ">>,
                    parameters => #{
                        actorName => <<"common.v1.ParamActorName">>,
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
                    responses => nkdomain_openapi_util:created_response("core.v1a1.File")
                }
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_FILE_PROVIDERS, Spec).



schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to create File Providers to store files.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.FileProviderSpec")
        },
        required => [spec],
        schemas => #{
            'core.v1a1.FileProviderSpec' => #{
                description => <<"
                    Specification for this actor.

                    The field `storageClass` must be used to select the class of
                    file provider. Currently supported types are:

                     * `filesystem` (use _filesystemConfig_ field).
                     * `s3` (use _s3Config_ field)

                    Actors of type `File` will have a _link_ of type `fileproviders`
                    pointing to its provider.
                    ">>,
                properties => #{
                    storageClass => #{
                        description => <<"Storage class to use. See above.">>,
                        type => string,
                        enum => [filesystem, s3]
                    },
                    maxSize => #{
                        description => <<"If specified, no file is allowed with a greater size (in bytes)">>,
                        type => integer
                    },
                    encryptionAlgo => #{
                        description => <<"
                            If specified, uploaded files will be encrypted using a random
                            password different for each one, and uncrypted on downloads.">>,
                        type => string,
                        enum => ['aes_cfb128']
                    },
                    hashAlgo => #{
                        description => <<"
                            If specified, a hash of all uploaded files will be calculated,
                            and checked on downloads.">>,
                        type => string,
                        enum => ['sha256']
                    },
                    directDownload => #{
                        description => <<"Allow direct downloads from provider.">>,
                        type => boolean,
                        default => ?FILE_DIRECT_DOWNLOAD
                    },
                    directUpload => #{
                        description => <<"Allow direct uploads to provider.">>,
                        type => boolean,
                        default => ?FILE_DIRECT_UPLOAD
                    },
                    directDownloadSecs => #{
                        description => <<"Time-to-live for direct download links.">>,
                        type => integer,
                        default => ?FILE_DIRECT_DOWNLOAD_SECS
                    },
                    directUploadSecs => #{
                        description => <<"Time-to-live for direct upload links.">>,
                        type => integer,
                        default => ?FILE_DIRECT_UPLOAD_SECS
                    },
                    filesystemConfig => nkdomain_openapi_util:ref_schema("core.v1a1.FileProviderSpecFilesystem"),
                    s3Config => nkdomain_openapi_util:ref_schema("core.v1a1.FileProviderSpecS3")
                },
                required => [storageClass]
            },

            'core.v1a1.FileProviderSpecFilesystem' => #{
                description => <<"Local filesystem provider.">>,
                properties => #{
                    filePath => #{
                        description => <<"Path to store files on the local server.">>,
                        type => string
                    }
                },
                required => [filePath]
            },

            'core.v1a1.FileProviderSpecS3' => #{
                description => <<"S3-compatible remote provider.">>,
                properties => #{
                    region => #{
                        description => <<"AWS region to work with.">>,
                        type => string
                    },
                    key => #{
                        description => <<"S3 AWS key.">>,
                        type => string
                    },
                    secret => #{
                        description => <<"S3 AWS secret.">>,
                        type => string
                    },
                    bucket => #{
                        description => <<"S3 bucket to use.">>,
                        type => string
                    },
                    path => #{
                        description => <<"Optional path inside the bucket.">>,
                        type => string
                    },
                    scheme => #{
                        description => <<"Protocol to use.">>,
                        type => string,
                        enum => [http, https],
                        default => https
                    },
                    host => #{
                        description => <<"Remote host. If empty will use Amazon's.">>,
                        type => string
                    },
                    port => #{
                        description => <<"HTTP port (if not 80/443)">>,
                        type => integer
                    }
                },
                required => [bucket, key, secret]
            },

            'core.v1a1.FileProviderUploadLinkS3' => #{
                description => <<"Link to upload a file.">>,
                properties => #{
                    verb => #{
                        description => <<"HTTP verb to use.">>,
                        type => string,
                        enum => [<<"PUT">>, <<"POST">>]
                    },
                    url => #{
                        description => <<"HTTP url to use.">>,
                        type => string,
                        format => uri
                    },
                    id => #{
                        description => <<"File id to use to create the file">>,
                        type => string
                    },
                    ttlSecs => #{
                        description => <<"Time for which this token is valid.">>,
                        type => integer
                    }
                },
                required => [verb, url, id]
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_FILE_PROVIDERS, Spec).


parameters(SrvId) ->
    {Filter, Sort} = nkdomain_openapi_util:filter_parameters(SrvId, "core", "v1a1", ?RES_CORE_FILE_PROVIDERS),
    #{
        'core.v1a1.FileProviderParamFieldSelector' =>
            nkdomain_openapi_util:filter_parameter(Filter),

        'core.v1a1.FileProviderParamSort' =>
            nkdomain_openapi_util:sort_parameter(Sort),

        'core.v1a1.FileProviderParamContentType' => #{
            name => <<"contentType">>,
            description => <<"MIME Content-Type for the file.">>,
            in => query,
            schema => #{
                type => string
            },
            required => true
        }
    }.