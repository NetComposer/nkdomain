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

%% @doc GraphQL testing
-module(nkdomain_graphql_test_2).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkdomain_test_util, [api/1, yaml/1]).

-compile(export_all).
-compile(nowarn_export_all).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").


%% ===================================================================
%% Public
%% ===================================================================

all_tests() ->
    nkdomain_test_util:create_test_data(),
    token_test(),
    config_test(),
    task_test(),
    session_test(),
    file_provider_test(),
    file_test(),
    nkdomain_test_util:delete_test_data(),
    ok.


%% @doc
token_test() ->
    Now = nklib_date:epoch(secs),
    {ok, Expires} = nklib_date:to_3339(Now+2, secs),

    Body1 = <<"
        apiVersion: core/v1a1
        kind: Token
        data:
            key1: val1
        metadata:
            subtype: MyType
            domain: c.b.a-nktest
            expiresTime: ", Expires/binary, "
            annotations:
                ann1: value1
            links:
                /apis/core/v1a1/domains/b.a-nktest/users/ut1: user
    ">>,
    Body2 = yaml(Body1),
    {created, _} = api(#{verb=>create, body=>Body2}),

    Q1 = <<"
        query {
            allTokens(
                domain: \"c.b.a-nktest\"
                sort: [
                    {metadata: {subtype: {}}},
                    {metadata: {updateTime: {}}}
                ],
                filter: {
                    and: [
                        {metadata: {subtype: {eq: \"MyType\"}}}
                    ]
                }
            ) {
                totalCount
                actors {
                    id
                    kind
                    data {
                        key
                        value
                    }
                    metadata {
                        subtype
                        expiresTime
                        annotations {
                            key
                            value
                        }
                        links {
                            key
                            value
                        }

                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allTokens">>:=#{<<"actors">>:=[Token], <<"totalCount">>:=1}}} = request(Q1),
    {ok, #{<<"metadata">>:=#{<<"uid">>:=UT1_UID}}} = api(#{domain=>"b.a-nktest", resource=>"users", name=>"ut1"}),
    #{
        <<"id">> := <<"tokens-", _/binary>>,
        <<"kind">> := <<"Token">>,
        <<"data">> := [
            #{<<"key">> := <<"key1">>, <<"value">> := <<"val1">>}
        ],
        <<"metadata">> := #{
            <<"subtype">> := <<"MyType">>,
            <<"expiresTime">> := Expires,
            <<"annotations">> := [
                #{<<"key">> := <<"ann1">>, <<"value">> := <<"value1">>}
            ],
            <<"links">> := [
                #{<<"key">> := <<"domains-", _/binary>>, <<"value">>:=<<"io.netc.core.domain">>},
                #{<<"key">> := UT1_UID, <<"value">> := <<"user">>}
            ]
        }
    } = Token,
    timer:sleep(2100),
    {ok, #{<<"allTokens">>:=#{<<"actors">>:=[], <<"totalCount">>:=0}}} = request(Q1),
    ok.



%% @doc
config_test() ->
    api(#{verb=>deletecollection, domain=>"c.b.a-nktest", resource=>configmaps, params=>#{
        fieldSelector => "metadata.subtype:MyType"}}),

    Body1 = <<"
        apiVersion: core/v1a1
        kind: ConfigMap
        data:
            key1: val1
        metadata:
            subtype: MyType
            domain: c.b.a-nktest
    ">>,
    Body2 = yaml(Body1),
    {created, _} = api(#{verb=>create, body=>Body2}),

    Q1 = <<"
        query {
            allConfigMaps(
                domain: \"c.b.a-nktest\"
                sort: [
                    {metadata: {subtype: {}}},
                    {metadata: {updateTime: {}}}
                ],
                filter: {
                    and: [
                        {metadata: {subtype: {eq: \"MyType\"}}}
                    ]
                }
            ) {
                totalCount
                actors {
                    id
                    kind
                    data {
                        key
                        value
                    }
                    metadata {
                        subtype
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allConfigMaps">>:=#{<<"actors">>:=[CM], <<"totalCount">>:=1}}} = request(Q1),
    #{
        <<"id">> := <<"configmaps-", _/binary>>,
        <<"kind">> := <<"ConfigMap">>,
        <<"data">> := [
            #{<<"key">> := <<"key1">>, <<"value">> := <<"val1">>}
        ],
        <<"metadata">> := #{
            <<"subtype">> := <<"MyType">>
        }
    } = CM,
    ok.


%% @doc
task_test() ->
    api(#{verb=>deletecollection, domain=>"c.b.a-nktest", resource=>tasks, params=>#{
        fieldSelector=>"metadata.subtype:MyType"}}),

    Body1 = <<"
        apiVersion: core/v1a1
        kind: Task
        spec:
            job:
                key1: val1
        metadata:
            subtype: MyType
            domain: c.b.a-nktest
    ">>,
    Body2 = yaml(Body1),
    {created, _} = api(#{verb=>create, body=>Body2}),

    Q1 = <<"
        query {
            allTasks(
                domain: \"c.b.a-nktest\"
                sort: [
                    {metadata: {subtype: {}}},
                    {metadata: {updateTime: {}}}
                ],
                filter: {
                    and: [
                        {metadata: {subtype: {eq: \"MyType\"}}}
                    ]
                }
            ) {
                totalCount
                actors {
                    id
                    kind
                    spec {
                        maxSecs
                        maxTries
                        job {
                            key
                            value
                        }

                    }
                    metadata {
                        uid
                        subtype
                    }
                    status {
                        isActivated
                        lastTryStartTime
                        tries
                        status
                        progress
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allTasks">>:=#{<<"actors">>:=[T1], <<"totalCount">>:=1}}} = request(Q1),
    % In a search operation, we don't have isActivated, status or progress
    #{
        <<"id">> := <<"tasks-", _/binary>>,
        <<"kind">> := <<"Task">>,
        <<"spec">> := #{
            <<"maxSecs">> := 3600,
            <<"maxTries">> := 3,
            <<"job">> := [
                #{<<"key">> := <<"key1">>, <<"value">> := <<"val1">>}
            ]
        },
        <<"metadata">> := #{
            <<"uid">> := UID1,
            <<"subtype">> := <<"MyType">>
        },
        <<"status">> := #{
            <<"isActivated">> := null,
            <<"lastTryStartTime">> := Time1,
            <<"tries">> := 1,
            <<"status">> := null,
            <<"progress">> := null
        }
    } = T1,

    Q2 = <<"
        query {
            node(id: \"", UID1/binary, "\") {
                ... on Task {
                    kind
                    status {
                        isActivated
                        lastTryStartTime
                        tries
                        status
                        progress
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"node">>:=T2}} = request(Q2),
    #{
        <<"kind">> := <<"Task">>,
        <<"status">> := #{
            <<"isActivated">> := true,
            <<"lastTryStartTime">> := Time1,
            <<"progress">> := 0,
            <<"status">> := <<"start">>,
            <<"tries">> := 1
        }
    } = T2,
    ok.



session_test() ->
    api(#{verb=>deletecollection, domain=>"a-nktest", resource=>sessions, params=>#{
        fieldSelector => <<"metadata.subtype:TestType">>}}),

    Y1 = yaml(<<"
        spec:
            ttlSecs: 2
        data:
            a: 1
        metadata:
            name: s1
            subtype: TestType
            domain: a-nktest
    ">>),

    {created, _} = api(#{verb=>create, resource=>sessions, body=>Y1}),


    Q1 = <<"
        query {
            allSessions(
                domain: \"a-nktest\"
            ) {
                totalCount
                actors {
                    id
                    kind
                    spec {
                        ttlSecs
                    }
                    data {
                        key
                        value
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allSessions">>:=#{<<"actors">>:=[S1], <<"totalCount">>:=1}}} = request(Q1),
    #{
        <<"id">> := <<"sessions-", _/binary>>,
        <<"kind">> := <<"Session">>,
        <<"data">> := [
            #{<<"key">> := <<"a">>,<<"value">> := <<"1">>}
        ],
        <<"spec">> := #{<<"ttlSecs">> := 2}
    } = S1,
    timer:sleep(2100),
    {ok, #{<<"allSessions">>:=#{<<"actors">>:=[], <<"totalCount">>:=0}}} = request(Q1),
    ok.


file_provider_test() ->
    nkdomain_test_2:file_provider_test(),
    nkdomain_test_2:file_test(),

    Q1 = <<"
        query {
            allFileProviders(
                domain: \"a-nktest\"
                sort: [
                    {spec: {storageClass: {}}}
                ]
            ) {
                totalCount
                actors {
                    id
                    kind
                    spec {
                        storageClass
                        maxSize
                        encryptionAlgo
                        hashAlgo
                        filesystemConfig {
                            filePath
                        }
                        s3Config {
                            region
                            key
                            secret
                            bucket
                            path
                            scheme
                            host
                            port
                        }
                    }
                    filesConnection {
                        totalCount
                        actors {
                            spec {
                                size
                                hash
                            }
                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allFileProviders">>:=#{<<"actors">>:=[FP1, FP2], <<"totalCount">>:=2}}} = request(Q1),
    #{
        <<"id">> := <<"fileproviders-", _/binary>>,
        <<"kind">> := <<"FileProvider">>,
        <<"spec">> := #{
            <<"storageClass">> := <<"filesystem">>,
            <<"maxSize">> := 3,
            <<"encryptionAlgo">> := <<"aes_cfb128">>,
            <<"hashAlgo">> := <<"sha256">>,
            <<"filesystemConfig">>  := #{
                <<"filePath">> := <<"/tmp">>
            },
            <<"s3Config">> := null
        },
        <<"filesConnection">> := #{
            <<"totalCount">> := 3,
            <<"actors">> := [
                #{
                    <<"spec">> := #{
                        <<"size">> := 3,
                        <<"hash">> := _
                    }
                },
                _,
                _
            ]
        }
    } = FP1,

    #{
        <<"id">> := <<"fileproviders-", _/binary>>,
        <<"kind">> := <<"FileProvider">>,
        <<"spec">> := #{
            <<"storageClass">> := <<"s3">>,
            <<"maxSize">> := null,
            <<"encryptionAlgo">> := <<"aes_cfb128">>,
            <<"hashAlgo">> := null,
            <<"filesystemConfig">> := null,
            <<"s3Config">> := #{
                <<"region">> := null,
                <<"key">> := <<"5UBED0Q9FB7MFZ5EWIO">>,
                <<"secret">> := <<>>,
                <<"bucket">> := <<"bucket1">>,
                <<"path">> := null,
                <<"scheme">> := <<"http">>,
                <<"host">> := <<"127.0.0.1">>,
                <<"port">> := 9000
            }
        },
        <<"filesConnection">> := #{
            <<"totalCount">> := 0,
            <<"actors">> := []
        }
    } = FP2,
    ok.



file_test() ->
    Q1 = <<"
        query {
            allFiles(
                domain: \"a-nktest\"
                sort: [
                    {spec: {contentType: {order: DESC}}}
                    {spec: {size: {}}}
                ]
                filter: {
                    and: [
                        {spec: {size: {gte: 3}}}
                    ]
                }
            ) {
                totalCount
                actors {
                    id
                    kind
                    spec {
                        size
                        contentType
                        password
                        hash
                        downloadLink
                    }
                    metadata {
                        name
                    }
                    fileProviderConnection {
                        spec {
                            storageClass
                            filesystemConfig {
                                filePath
                            }
                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allFiles">>:=#{<<"actors">>:=[F1, _, _], <<"totalCount">>:=3}}} = request(Q1),
    Hash = base64:encode(crypto:hash(sha256, <<"123">>)),
    #{
        <<"id">> := <<"files-", _/binary>>,
        <<"kind">> := <<"File">>,
        <<"spec">> := #{
            <<"size">> := 3,
            <<"contentType">> := <<"type1">>,
            <<"password">> := _,
            <<"hash">> := Hash,
            <<"downloadLink">> := Download
        },
        <<"metadata">> := #{
            <<"name">> := Name
        },
        <<"fileProviderConnection">> := #{
            <<"spec">> := #{
                <<"storageClass">> := <<"filesystem">>,
                <<"filesystemConfig">> := #{
                    <<"filePath">> := <<"/tmp">>
                }
            }
        }
    } = F1,
    [Url|_] = nkdomain_plugin:get_external_urls('nkdomain-root'),
    Download = <<Url/binary, "/apis/core/v1a1/domains/a-nktest/files/", Name/binary, "/_download">>,
    ok.




%% ===================================================================
%% Util
%% ===================================================================


request(Query) ->
    nkservice_graphql:request(?ROOT_SRV, Query, #{}).


