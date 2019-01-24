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

%% @doc NkDomain OpenAPI support
%% https://swagger.io/docs/specification/about/

-module(nkdomain_openapi_core).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([base/1, paths/1, schemas/1, parameters/1, responses/1]).
-import(nkdomain_openapi_util, [ref_schema/1]).

-include("nkdomain.hrl").




%% ===================================================================
%% Core implementation
%% ===================================================================


base(SrvId) ->
    {_ApisA, _ApisB, Tags} = nkdomain_openapi_util:get_apis_tags(SrvId),
    Tags2 = [#{name => Name} || Name <- Tags],
    #{
        openapi => <<"3.0.0">>,
        info => #{
            version => <<"v10">>,
            title => <<"NetComposer API">>,
            description => <<"
                Standard NetComposer platform external API. The number of available API headers
                will depend of the type of installed plugins
            ">>,
            termsOfService => <<"http://swagger.io/terms/">>,
            contact => #{
                name => <<"NetComposerr API Team">>,
                email => <<"carlosj.gf@gmail.com">>,
                url => <<"http://netscale.io">>
            },
            license => #{
                name => <<"Apache 2.0">>,
                url => <<"https://www.apache.org/licenses/LICENSE-2.0.html">>
            }
        },
        servers => [
            #{url => <<"/">>},
            #{url => <<"https://netcomposer.org/api">>}
        ],
        tags => [
            #{name => <<"general">>},
            #{name => <<"apis">>}
            | Tags2
        ]
    }.



paths(SrvId) ->
    Map1 = #{
        '/apis' => #{
            get => #{
                description => <<"Get available API groups & versions.">>,
                tags => [general],
                responses => #{
                    '200' => #{
                        description => <<"OK">>,
                        content => #{
                            'application/json' => #{
                                schema => ref_schema("common.v1.APIGroupList")
                            }
                        }
                    }
                }
            }
        },
        '/apis/core/v1a1/_uids/{actorUID}' => #{
            get => #{
                description => <<"Gets a specific actor by its UID.">>,
                tags => [core, core_v1a1],
                parameters => [
                    #{
                        name => actorUID,
                        in => path,
                        required => true,
                        description => <<"The UID of actor to retrieve">>,
                        schema => #{
                            type => string
                        }
                    }
                ],
                responses =>
                    nkdomain_openapi_util:ok_response("common.v1.Actor")
            }
        },
        '/apis/_ws' => #{
            get => #{
                description => <<"
                    Starts a WS connection to launch API commands.
                    Once started the connection, you can start commands with the following format:


                    ```
                    {
                        verb: \"create\",
                        group: \"core/v1a1\",
                        domain: \"root\",
                        resource: \"users\",
                        params: {
                            activate: true
                        },
                        body: {
                         ...
                        }
                        tid: 1
                    }
                    ```

                    The server would then reply with:

                    ```
                    {
                        apiVersion: \"v1\",
                        kind: \"Status\",
                        status: \"Success\",
                        code: 200,
                        body: {
                        ...
                        }
                        tid: 1
                    }
                    ```
                ">>,
                tags => [general],
                responses => #{
                    '101' => #{
                        description => <<"Switching Protocols">>
                    }
                }
            }
        },
        '/graphql' => #{
            post => #{
                description => <<"
                    Launches a GraphQL query.

                    **Example1**:
                    ```
                    query {
                        node(id: \"user-Jo8pwRALAg7RIBi9Ku3Cvnm75W2\") {
                            id
                            ... on User {
                                type
                                metadata {
                                    uid
                                    name
                                    domain
                                    labels {
                                        key
                                        value
                                    }
                                }
                            }
                        }
                    }
                    ```

                    **Example2**:

                    ```
                    query {
                        allContacts(
                            deep: true
                            domain: \"c.b.a-nktest\"
                            sort: [
                                {spec.normalizedName: {}},
                                {spec.birthTime: {order: desc}},
                            ],
                            filter: {
                                'and': [
                                    {spec.birthTime: {gte: '2008'}}
                                ]
                            }
                        ) {
                            totalCount
                            actors {
                                id
                                type
                                spec {
                                    name
                                    surname
                                    normalizedName
                                    normalizedSurname
                                    birthTime
                                    gender
                                    timezone
                                    email {
                                        type
                                        email
                                        meta {
                                            key
                                            value
                                        }
                                    }
                                }
                            }
                        }
                    }
                    ```
                ">>,
                tags => [general],
                requestBody => #{
                    required => true,
                    content => #{
                        'text/plain' => #{
                            schema => #{
                                type => string
                            }
                        }
                    }
                },
                responses =>
                    nkdomain_openapi_util:ok_response("common.v1.ListMeta")
            }
        },
        '/search/v1a1' => #{
            post => nkdomain_openapi_schemas:search_post()
        },
        '/search/v1a1/domains/{domainName}' => #{
            parameters => [
                nkdomain_openapi_util:ref_parameter("common.v1.ParamDomainName")
            ],
            post => nkdomain_openapi_schemas:search_post()
        }
    },
    {ApisA, ApisB, _Tags} = nkdomain_openapi_util:get_apis_tags(SrvId),
    Map2 = lists:foldl(
        fun(Api, Acc) ->
            Acc#{
                Api => #{
                    get => #{
                    description => <<"get available versions for this API group.">>,
                    tags => [apis],
                    responses => #{
                        '200' => #{
                            description => <<"OK">>,
                            content => #{
                                'application/json' => #{
                                    schema => ref_schema("common.v1.APIGroup")
                                }
                            }
                        }
                    }
                }
            }
            }
        end,
        Map1,
        ApisA),
    Map3 = lists:foldl(
        fun(Api, Acc) ->
            Acc#{
                Api => #{
                    get => #{
                        description => <<"get available resources for this API group & version.">>,
                        tags => [apis],
                        responses => #{
                            '200' => #{
                                description => <<"OK">>,
                                content => #{
                                    'application/json' => #{
                                        schema => ref_schema("common.v1.APIResourceList")
                                    }
                                }
                            }
                        }
                    }
                }
            }
        end,
        Map2,
        ApisB),
    Map3.





schemas(_SrvId) -> #{

    'common.v1.APIGroup' => #{
        description => <<"Contains the name, the supported versions and the preferred version of a group.">>,
        properties => #{
            name => #{
                description => <<"The name of the Group.">>,
                type => string
            },
            preferredVersion => ref_schema("common.v1.GroupVersionForDiscovery"),
            versions => #{
                description => <<"Versions currently supported in this group.">>,
                type => array,
                items => ref_schema("common.v1.GroupVersionForDiscovery")
            }
        }
    },

    'common.v1.APIGroupList' => #{
        description => <<"A list of API groups.">>,
        properties => #{
            apiVersion => #{
                description => <<"The referred API Version.">>,
                type => string
            },
            kind => #{
                description => <<"A string representing the REST resource this object represents. In CamelCase.">>,
                type => string
            },
            groups => #{
                description => <<"The list of API groups.">>,
                type => array,
                items => ref_schema("common.v1.APIGroup")
            }
        }
    },

    'common.v1.GroupVersionForDiscovery' => #{
        description => <<"GroupVersion contains the 'group/version' and 'version' strings of an API group.">>,
        properties => #{
            groupVersion => #{
                description => <<"groupVersion specifies the API group and version in the form 'group/version'">>,
                type => string
            },
            version => #{
                description => <<"version specifies only the version of the group">>,
                type => string
            }
        }
    },

    'common.v1.APIResourceList' => #{
        description => <<"A list of APIResource, it is used to expose the name of the resources supported in a specific group and version.">>,
        properties => #{
            groupVersion => #{
                description => <<"The referred API Version.">>,
                type => string
            },
            kind => #{
                description => <<"Kind is a string value representing the REST resource this object represents.">>,
                type => string
            },
            resources => #{
                description => <<"resources contains the name of the resources">>,
                type => array,
                items => ref_schema("common.v1.APIResource")
            }
        }
    },

    'common.v1.APIResource' => #{
        description => <<"APIResource specifies the name of a resource.">>,
        properties => #{
            kind => #{
                description => <<"The kind for the resource (e.g. 'User' is the kind for a resource 'users')">>,
                type => string
            },
            name => #{
                description => <<"Name of the resource (plural).">>,
                type => string
            },
            shortNames => #{
                description => <<"A list of suggested short names of the resource.">>,
                type => array,
                items => #{
                    type => string
                }
            },
            singularName => #{
                description => <<"The singular name of the resource.">>,
                type => string
            },
            verbs => #{
                description => <<"
                    A list of supported netcomposer verbs (this includes get, list,
                    watch, create, update, patch, delete, deletecollection).">>,
                type => array,
                items => #{
                    type => string
                }
            }
        }
    },

    'common.v1.Actor' => #{
        description => <<"A generic actor">>,
        properties => #{
            apiVersion => #{
                description => <<"The referred API Version">>,
                type => string
            },
            kind => #{
                description => <<"A string value representing the REST resource this object represents.">>,
                type => string
            },
            data => #{
                description => <<"Dependant on the returned Type">>,
                type => object
            },
            spec => #{
                description => <<"Dependant on the returned Type">>,
                type => object
            },
            metadata => ref_schema("common.v1.ActorMeta"),
            status => #{
                description => <<"Dependant on the returned Type">>,
                type => object
            }
        }
    },

    'common.v1.ListMeta' => #{
        description => <<"
            ListMeta describes metadata that synthetic resources must have, including lists and various status objects.
            A resource may have only one of {ActorMeta, ListMeta}">>,
        properties => #{
            size => #{
                description => <<"Number of returned objects">>,
                type => integer
            },
            total => #{
                description => <<"Numer of total actors belonging to the query. Included only if the `totals` parameter is used">>
            }
        }
    },

    'common.v1.ActorMeta' => #{
        description => <<"
            ActorMeta holds the metadata that all persisted resources must have,
            which includes all objects users must create.">>,
        properties => #{
            alarms => #{
                description => <<"
                    If the actor is in 'alarm state' here a list of current alarms will be shown.
                    Keys would be the alarm class.">>,
                type => object,
                additionalProperties => ref_schema("common.v1.Alarm")
            },
            annotations => #{
                description => <<"
                    Annotations is an unstructured key value map stored with a resource
                    that may be set by external tools to store and retrieve arbitrary
                    metadata. They are not queryable and should be preserved when
                    modifying objects.">>,
                type => object,
                additionalProperties => #{
                    type => string
                }
            },
            creationTime => #{
                description => <<"A timestamp representing the server time when this object was created.">>,
                type => string,
                format => 'date-time'
            },
            description => #{
                description => <<"Any free text describing the instance of the actor">>,
                type => string
            },
            domain => #{
                description => <<"Domain this actor belongs to. Cannot be updated.">>,
                type => string
            },
            expiresTime => #{
                description => <<"A timestamp at which this resource will expire and will be deleted">>,
                type => string,
                format => 'date-time'
            },
            fts => #{
                description => <<"
                    Map of string keys and values that can be used to perform _full text search_ on
                    actors. Keys are the field value, and value can be any text word or phrase.
                    An example could be `{\"field1\": \"My Text\", \"field2\": \"Magnífico\"}`
                    "/utf8>>,
                type => object,
                additionalProperties => #{
                    type => string
                }
            },
            generation => #{
                description => <<"
                    A sequence number representing a specific generation of the actor.
                    It is incremented at each change. Read-only.">>,
                type => integer,
                format => int64
            },
            isEnabled => #{
                description => <<"
                    Describes if the object is 'enabled' or not. By default all actors are
                    enabled. Disabled actors cannot perform some operations.">>,
                type => boolean
            },
            isInAlarm => #{
                description => <<"Describes if the object is 'alarm state' or not. See the 'alarms' parameter.">>,
                type => boolean
            },
            labels => #{
                description => <<"
                    Map of string keys and values that can be used to organize and
                    categorize (scope and select) objects.">>,
                type => object,
                additionalProperties => #{
                    type => string
                }
            },
            links => #{
                description => <<"
                    List of other actors this actor is linked to. An actor cannot be deleted
                    if other actors are linked to them. See each actor's documentation for
                    special supported links.
                    Keys are the type of link, and values are the UID of the linked actor.
                    You can also use the `selfLink` version of actor when creating or updating objects,
                    and the server will find the UID and update this value.">>,
                type => object,
                additionalProperties => #{
                    type => string
                }
            },
            name => #{
                description => <<"
                    Name must be unique within a namespace. Is required when creating
                    resources, although some resources may allow a client to request the
                    generation of an appropriate name automatically. Name is primarily
                    intended for creation idempotence and configuration definition. Cannot
                    be updated.">>,
                type => string
            },
            resourceVersion => #{
                description => <<"
                    An opaque value that represents the internal version of this object
                    that can be used by clients to determine when objects have changed.">>,
                type => string
            },
            selfLink => #{
                description => <<"A URL representing this object. Read-only.">>,
                type => string
            },
            subtype => #{
                description => <<"Optional sub-classification of the Actor.">>,
                type => string
            },
            uid => #{
                description => <<"
                    UID is the unique in time and space value for this object. It is
                    typically generated by the server on successful creation of a resource
                    and is not allowed to change on PUT operations.">>,
                type => string
            },
            updateTime => #{
                description => <<"A timestamp representing the server time when this object was updated.">>,
                type => string,
                format => 'date-time'
            }
        }
    },

    'common.v1.Status' => #{
        description => <<"Status is a return value for calls that don't return other objects.">>,
        properties => #{
            apiVersion => #{
                description => <<"Defines the versioned schema of this representation of an object.">>,
                type => string
            },
            code => #{
                description => <<"'Suggested HTTP return code for this status, 0 if not set.'">>,
                type => integer,
                format => int32
            },
            details => #{
                description => <<"
                    Extended data associated with the reason.  Each reason may define its
                    own extended details. This field is optional and the data returned is
                    not guaranteed to conform to any schema except that defined by the
                    reason type.">>,
                type => object,
                additionalProperties => true
            },
            kind => #{
                description => <<"
                    String value representing the REST resource this object
                    represents. Servers may infer this from the endpoint the client
                    submits requests to. Cannot be updated. In CamelCase.">>,
                type => string
            },
            message => #{
                description => <<"A human-readable description of the status of this operation.">>,
                type => string
            },
            metadata => ref_schema("common.v1.ListMeta"),
            reason => #{
                description => <<"A machine-readable description of the represented status.">>,
                type => string
            },
            status => #{
                description => <<"Status of the operation. One of 'Success' or 'Failure'.">>,
                type => string
            }
        }
    },

    'common.v1.Alarm' => #{
        description => <<"Represents a alarm fired in relation to this actor and currently active">>,
        properties => #{
            code => #{
                description => <<"Single-word error code.">>,
                type => string
            },
            message => #{
                description => <<"Human-readable error reason.">>,
                type => string
            },
            lastTime => #{
                description => <<"Last time this alarm fired.">>,
                type => string,
                format => 'date-time'
            },
            meta => #{
                description => <<"Additional information">>,
                type => object,
                additionalProperties => true
            }
        }
    },

    'common.v1.ActorStatus' => #{
        description => <<"Represents the current, in-memory status of the Actor">>,
        properties => #{
            isActivated => #{
                description => <<"If the actor is currently activated">>,
                type => boolean
            }
        }
    },

    'common.v1.SearchBody' => #{
        description => <<"
            A query description body. The request will find actors that fulfills
            all the parameters and filters. Actors will be paginated and sorted
            depending on the corresponding parameters.

            If options `apiGroup` and `kind` are used, they are added as filters
            and also specific fields for filtering and sorting become available.
            See documentation for 'list' for each kind
            ">>,
        properties => #{
            apiGroup => #{
                description => <<"Filter actors of this apiGroup">>,
                type => string
            },
            kind => #{
                description => <<"Filter actors of this Kind">>,
                type => string
            },
            deep => #{
                description => <<"Find actors in domains bellow query domain.">>,
                type => boolean,
                default => false
            },
            from => #{
                description => <<"Record to start with (for pagination).">>,
                type => integer,
                minimum => 0
            },
            size => #{
                description => <<"Number of records to retrieve.">>,
                type => integer,
                minimum => 0,
                default => 10
            },
            totals => #{
                description => <<"
                    Calculate the grand total of actors that fulfill the requirements.
                    Set to 'false' if you don't need to know the total.">>,
                type => boolean,
                default => true
            },
            filter => #{
                description => <<"
                    Set of filters for the query. Actors will be retrieved only if
                    they fulfill **all** of the filters in the 'and' section, **one or more**
                    of the filters in the 'or' section and **none** of the filters in
                    the 'not' section
                ">>,
                type => object,
                properties => #{
                    'and' => #{
                        description => <<"All of this filters in this section are needed.">>,
                        type => array,
                        items => ref_schema("common.v1.SearchFilter")
                    },
                    'or' => #{
                        description => <<"Some of the filters in this section are needed.">>,
                        type => array,
                        items => ref_schema("common.v1.SearchFilter")
                    },
                    'not' => #{
                        description => <<"None of the filters in this section are allowed in result.">>,
                        type => array,
                        items => ref_schema("common.v1.SearchFilter")
                    }
                }
            },
            sort => #{
                description => <<"Fields to sort on">>,
                type => array,
                items => ref_schema("common.v1.SearchSort")
            }
        }
    },

    'common.v1.SearchFilter' => #{
        description => <<"Filter search specification">>,
        properties => #{
            field => #{
                description => <<"
                    Field to apply the filter.
                    See available fields to filter on for each resource type">>,
                type => string
            },
            op => #{
                description => <<"Filter operation.">>,
                type => string,
                enum => [eq, ne, gt, gte, lt, lte, exists, values, prefix]
            },
            value => #{
                description => <<"Value for the operation.">>,
                oneOf => [
                    #{type => string},
                    #{type => integer},
                    #{type => boolean},
                    #{
                        type => array,
                        items => #{
                            oneOf => [
                                #{type => string},
                                #{type => integer},
                                #{type => boolean}
                            ]
                        }
                    }
                ]
            }
        },
        required => [field, op, value]
    },

    'common.v1.SearchSort' => #{
        description => <<"Sort search specification">>,
        properties => #{
            field => #{
                description => <<"
                    Field to sort on.
                    See available fields to sort on for each resource type">>,
                type => string
            },
            order => #{
                description => <<"Sort order.">>,
                type => string,
                enum => [asc, desc]
            }
        },
        required => [field]
    },

    'common.v1.Callback' => #{
        description => <<"A callback specification">>,
        properties => #{
            url => #{
                description => <<"URL to call">>,
                type => string,
                format => uri
            },
            method => #{
                description => <<"Method to use">>,
                type => string,
                enum => [get, post, put, delete, head],
                default => post
            },
            redirects => #{
                description => <<"Number of redirects to allow">>,
                type => integer,
                minimum => 0,
                maximum => 10,
                default => 0
            },
            insecure => #{
                description => <<"For HTTPS, do not check certificate">>,
                type => boolean,
                default => false
            }
        },
        required => [url]
    }

}.


%% @private
parameters(_SrvId) -> #{

    'common.v1.ParamDeep' => #{
        name => deep,
        in => query,
        description => <<"Go to deeper subdomains. Finds actor in all domains bellow current domain.">>,
        schema => #{
            type => boolean,
            default => false
        }
    },

    'common.v1.ParamFrom' => #{
        name => from,
        in => query,
        description => <<"Position to start in the query for pagination (default is 0)">>,
        schema => #{
            type => integer,
            minimum => 0
        }
    },

    'common.v1.ParamSize' => #{
        name => size,
        in => query,
        description => <<"Number of records to return (default is 10)">>,
        schema => #{
            type => integer,
            minimum => 0
        }
    },

    'common.v1.ParamSort' => nkdomain_openapi_util:sort_parameter([]),

    'common.v1.ParamLabelSelector' => #{
        name => labelSelector,
        description => <<"
            Labels to filter on. It can be a field or a list fields. It can include a value or
            not (and it would find actors having this label with any value).

            **Example**: `labelSelector=label1,label2:true`
            ">>,
        in => query,
        schema => #{
            type => string
        }
    },

    'common.v1.ParamFieldSelector' => nkdomain_openapi_util:filter_parameter([]),

    'common.v1.ParamLinkedTo' => #{
        name => linkedTo,
        description => <<"
            Filter to actors linked to another actor, optionally with an specific _link type_.

            **Example**: `linkedTo=user-l4lc993kcdl3lc,user-39fk4kc93kc09:my_user_type`
            ">>,
        in => query,
        schema => #{
            type => string
        }
    },

    'common.v1.ParamFts' => #{
        name => fts,
        description => <<"

            Allows to filter based on the simple internal full text search system.
            When creating actors, you can specify one or several fts fields in the
            metadata.fts field. They are parsed as word, an it is possible to query on
            them.

            **Example**: if metadata.fts.field1: 'My náme', you could perform the following queries:
              * `fts=my (search 'my' in all fields)`
              * `fts=field1:my*`
              * `fts=nam*`
            "/utf8>>,
        in => query,
        schema => #{
            type => string
        }
    },

    'common.v1.ParamTotals' => #{
        name => totals,
        in => query,
        description => <<"
            Calculate the grand total of records for this query.
            If you don't need the total number of records, set to false to speed the query.
            Default is true.
            ">>,
        schema => #{
            type => boolean
        }
    },

    'common.v1.ParamDomainName' => #{
        name => domainName,
        in => path,
        required => true,
        description => <<"The name of the domain where the actor is">>,
        schema => #{
            type => string,
            default => root
        }
    },

    'common.v1.ParamActorName' => #{
        name => actorName,
        in => path,
        required => true,
        description => <<"The name of the related actor">>,
        schema => #{
            type => string
        }
    },

    'common.v1.ParamActivate' => #{
        name => activate,
        in => query,
        description => <<"Activate the actor (if not already activated)">>,
        schema => #{
            type => boolean,
            default => true
        }
    },

    'common.v1.ParamActorTTL' => #{
        name => ttl,
        in => query,
        description => <<"TTL to use for activation in seconds (if not already activated)">>,
        schema => #{
            type => integer
        }
    },

    'common.v1.ParamCascade' => #{
        name => cascade,
        in => query,
        description => <<"Deletes all linked objects in cascade">>,
        schema => #{
            type => boolean,
            default => false
        }
    },

    'common.v1.ParamWatch' => #{
        name => watch,
        in => query,
        description => <<"
            Starts a watching over events related to this resource.
            See the parameter 'resourceVersion' also">>,
        schema => #{
            type => boolean
        }
    },

    'common.v1.ParamResourceVersion' => #{
        name => resourceVersion,
        in => query,
        description => <<"For watch requests, start at the actor having this resourceVersion.">>,
        schema => #{
            type => string
        }
    },

    'common.v1.ParamSearchDelete' => #{
        name => delete,
        in => query,
        description => <<"Deletes all returned actors.">>,
        schema => #{
            type => boolean,
            default => false
        }
    }
}.


%% @private
responses(_SrvId) -> #{

    'common.v1.Response400' => #{
        description => <<"Bad Request">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response401' => #{
        description => <<"Unauthorized">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response404' => #{
        description => <<"Not Found">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response405' => #{
        description => <<"Method Not Allowed">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response409' => #{
        description => <<"Conflict">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response422' => #{
        description => <<"Unprocessable">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response429' => #{
        description => <<"Too Many Requests">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    },

    'common.v1.Response500' => #{
        description => <<"Internal Error">>,
        content => #{
            'application/json' => #{
                schema => ref_schema("common.v1.Status")
            }
        }
    }
}.



