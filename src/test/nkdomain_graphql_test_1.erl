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
-module(nkdomain_graphql_test_1).
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
    ok = core_test(),
    ok = alarm_test(),
    ok = actors_test_1(),
    ok = actors_test_2(),
    nkdomain_test_util:create_test_data(),
    users_test(),
    event_test(),
    contact_test(),
    nkdomain_test_util:delete_test_data(),
    ok.


core_test() ->
    {error, {<<"actor_not_found">>, _}} =
        request(<<"query { node(id: \"unknown\") { id } }">>),

    {error, {<<"unknown_field">>, <<"The query refers to a field which is not known (document.ROOT.node2 )">>}} =
        request(<<"query { node2(id: \"unknown\") { id } }">>),

    {error,{<<"parser_error">>,<<"1: syntax error before: 2">>}} =
        request(<<"query { node(id 2: \"unknown\") { id } }">>),


    {ok, #{<<"metadata">>:=#{<<"uid">>:=B_UID}}} = api(#{domain=>"a-nktest", resource=>"domains", name=>"b"}),
    {ok, #{<<"metadata">>:=#{<<"uid">>:=UT1_UID}}} = api(#{domain=>"b.a-nktest", resource=>"users", name=>"ut1"}),
    Q1 = <<"
        query {
            node(id: \"", UT1_UID/binary, "\") {
                id
                ... on Actor {
                    apiVersion
                    kind
                    metadata {
                        uid
                        name
                        domain
                        resourceVersion
                        generation
                        creationTime
                        updateTimeAlias: updateTime
                        updateTime(format:  UNIXTIME)
                        description
                        selfLink
                        isEnabled
                        expiresTime
                        labels {
                            key
                            value
                        }
                        annotations {
                            key
                            value
                        }
                        links {
                            key
                            value
                        }
                        fts {
                            key
                            value
                        }
                    }
                }
            }
        }">>,
    io:format("Q1: ~s\n", [Q1]),
    {ok, #{<<"node">>:=R1}} = request(Q1),
    #{
        <<"id">> := UT1_UID,
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"User">>,
        <<"metadata">> := #{
            <<"uid">> := UT1_UID,
            <<"domain">> := <<"b.a-nktest">>,
            <<"name">> := <<"ut1">>,
            <<"resourceVersion">> := _,
            <<"generation">> := Gen,
            <<"creationTime">> := _CT,
            <<"updateTime">> := UT_EPOCH,
            <<"updateTimeAlias">> := UT_TXT,
            <<"description">> := null,
            <<"selfLink">> := <<"/apis/core/v1a1/domains/b.a-nktest/users/ut1">>,
            <<"isEnabled">> := true,
            <<"expiresTime">> := null,
            <<"labels">> := [
                #{<<"key">> := <<"is_a-nktest_domain">>,<<"value">> := <<"true">>},
                #{<<"key">> := <<"is_b_domain">>,<<"value">> := <<"true">>}
            ],
            <<"annotations">> := [],
            <<"links">> := [
                #{<<"key">> := <<"domains">>,<<"value">> := B_UID}
            ],
            <<"fts">> := [
                #{
                    <<"key">> := <<"fts_name">>,
                    <<"value">> := <<"Úser MY name"/utf8>>
                }
            ]
        }
    } = R1,
    true = is_integer(Gen) andalso Gen >= 0,
    UT = binary_to_integer(UT_EPOCH),
    {ok, UT} = nklib_date:to_epoch(UT_TXT, msecs),
    ok.


alarm_test() ->
    Q1 = <<"
        query {
            allActors(
                deep: true,
                sort: [
                  {apiGroup: {order: DESC}}
                  {kind: {order: DESC}}
                  {metadata: {creationTime: {}}}
                ]
                filter: {
                    and: [{metadata: {isInAlarm: {eq: true}}}]
                }
            ) {
                totalCount
                actors {
                    ... on Actor {
                        kind
                        metadata {
                            name
                            isInAlarm
                            alarms {
                                class
                                code
                                message
                                lastTime
                                meta {
                                    key
                                    value
                                }
                            }
                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"actors">>:=[]}}} = request(Q1),

    {ok, #{<<"metadata">>:=#{<<"uid">>:=B_UID}}} = api(#{domain=>"a-nktest", resource=>"domains", name=>"b"}),

    ok = nkservice_actor_srv:sync_op(B_UID, {set_alarm, class1, #{
        <<"code">> => <<"code1">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 1}
    }}),
    ok = nkservice_actor_srv:sync_op(B_UID, {set_alarm, class2, #{
        <<"code">> => <<"code2">>,
        <<"message">> => <<"message2">>,
        <<"lastTime">> => <<"date1">>
    }}),
    ok = nkservice_actor_srv:sync_op(B_UID, {set_alarm, class1, #{
        <<"code">> => <<"code3">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 2}
    }}),

    ok = nkservice_actor_srv:async_op(B_UID, save),
    timer:sleep(50),

    {ok, #{<<"allActors">>:=#{<<"actors">>:=[B1]}}} = request(Q1),
    #{
        <<"kind">> := <<"Domain">>,
        <<"metadata">> := #{
            <<"name">> := <<"b">>,
            <<"isInAlarm">> := true,
            <<"alarms">> := [
                #{
                    <<"class">> := <<"class1">>,
                    <<"code">> := <<"code3">>,
                    <<"lastTime">> := <<"20", _/binary>>,
                    <<"message">> := <<"message1">>,
                    <<"meta">> := [
                        #{
                            <<"key">> := <<"a">>,
                            <<"value">> := <<"2">>
                        }
                    ]
                },
                #{
                    <<"class">> := <<"class2">>,
                    <<"code">> := <<"code2">>,
                    <<"lastTime">> := <<"date1">>,
                    <<"message">> := <<"message2">>,
                    <<"meta">> := []
                }
            ]
        }
    } = B1,

    ok = nkservice_actor_srv:async_op(B_UID, clear_all_alarms),
    ok = nkservice_actor_srv:async_op(B_UID, save),
    timer:sleep(50),
    {ok, #{<<"allActors">>:=#{<<"actors">>:=[]}}} = request(Q1),
    ok.


actors_test_0() ->
    Q1 = <<"
        query {
            allActors(
            deep: true
            sort: [
              {kind: {order: DESC}}
              {apiGroup: {}}
              {metadata: {name: {}}}
              {metadata: {domain: {}}}
            ],
            filter: {
              and: [
                {apiGroup: {eq: \"core\"}}
                {kind: {prefix: \"U\"}}
                {metadata: {domain: {eq: \"root\"}}}
              ]
            }

          ) {
            totalCount
            actors {
              id
              apiVersion
              kind
              metadata {
                name
                selfLink
              }
            }
          }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"actors">>:=[Admin], <<"totalCount">>:=1}}} = request(Q1),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"id">> := <<"user-", _/binary>>,
        <<"kind">> := <<"User">>,
        <<"metadata">> := #{
            <<"name">> := <<"admin">>,
            <<"selfLink">> := <<"/apis/core/v1a1/domains/root/users/admin">>}
    } = Admin,
    ok.



actors_test_1() ->
    Q1 = <<"
        query {
            allActors(
                domain: \"a-nktest\"
                deep: true,
                sort: [
                  {kind: {order: DESC}}
                  {metadata: {creationTime: {}}}
                ]

            ) {
                totalCount
                actors {
                    ... on Actor {
                        kind
                        metadata {
                            name
                            creationTime

                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"actors">>:=Actors1, <<"totalCount">>:=7}}} = request(Q1),
    [
        % First, the only user
        #{<<"kind">>:=<<"User">>, <<"metadata">>:=#{<<"name">>:=<<"ut1">>}},
        % Then the 4 creation event, on time order
        #{<<"kind">>:=<<"Event">>, <<"metadata">>:=#{<<"name">>:=<<"domains.a", _/binary>>, <<"creationTime">>:=T0}}=I1,
        #{<<"kind">>:=<<"Event">>, <<"metadata">>:=#{<<"name">>:=<<"domains.b", _/binary>>, <<"creationTime">>:=T1}}=I2,
        #{<<"kind">>:=<<"Event">>, <<"metadata">>:=#{<<"name">>:=<<"domains.c", _/binary>>, <<"creationTime">>:=T2}}=I3,
        #{<<"kind">>:=<<"Event">>, <<"metadata">>:=#{<<"name">>:=<<"users.ut1", _/binary>>, <<"creationTime">>:=T3}}=_I4,
        % Then the domains, time order
        #{<<"kind">>:=<<"Domain">>, <<"metadata">>:=#{<<"name">>:=<<"b">>, <<"creationTime">>:=T4}}=I5,
        #{<<"kind">>:=<<"Domain">>, <<"metadata">>:=#{<<"name">>:=<<"c">>, <<"creationTime">>:=T5}}
    ] = Actors1,
    true = T3 > T2 andalso T2 > T1 andalso T1 > T0,
    true = T5 > T4,


    Q2 = <<"
        query {
            allActors(
                domain: \"a-nktest\"
                deep: true,
                sort: [
                  {kind: {order: DESC}}
                  {metadata: {creationTime: {}}}
                ]
                from: 1
                size: 3
            ) {
                totalCount
                actors {
                    ... on Actor {
                        kind
                        metadata {
                            name
                            creationTime

                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"actors">>:=Actors2, <<"totalCount">>:=7}}} = request(Q2),
    [I1, I2, I3] = Actors2,

    Q3 = <<"
        query {
            allActors(
                domain: \"a-nktest\"
                #deep: true,
                sort: [
                  {kind: {order: DESC}}
                  {metadata: {creationTime: {}}}
                ]
            ) {
                totalCount
                actors {
                    ... on Actor {
                        kind
                        metadata {
                            name
                            creationTime
                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"actors">>:=Actors3, <<"totalCount">>:=2}}} = request(Q3),
    [I1, I5] = Actors3,

    {ok, #{<<"metadata">>:=#{<<"uid">>:=UT1_UID}}} = api(#{domain=>"b.a-nktest", resource=>"users", name=>"ut1"}),
    Q4A = <<"
        query {
            allActors(
                filter: {
                    and: [
                        {id: {eq: \"", UT1_UID/binary, "\"}}
                    ]
                }
            ) {
                totalCount
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"totalCount">>:=0}}} = request(Q4A),

    Q4B = <<"
        query {
            allActors(
                deep: true
                filter: {
                    and: [
                        {id: {eq: \"", UT1_UID/binary, "\"}}
                    ]
                }
            ) {
                totalCount
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"totalCount">>:=1}}} = request(Q4B),

    % Test sort by domain and uid in metadata (field conversions)
    % Sort can bee seen in SQL log
    Q4C = <<"
        query {
            allActors(
                domain: \"b.a-nktest\"
                sort: [{metadata: {domain: {}}}]
                filter: {
                    and: [
                        {metadata: {uid: {eq: \"", UT1_UID/binary, "\"}}}
                    ]
                }
            ) {
                totalCount
            }
        }
    ">>,
    {ok, #{<<"allActors">>:=#{<<"totalCount">>:=1}}} = request(Q4C),
    ok.


actors_test_2() ->
    Filters1 = <<",
        {metadata: {creationTime: {prefix: \"2018\"}}}
        {metadata: {generation: {gte: 0}}}
        {metadata: {isEnabled: {eq: true}}}
        {metadata: {uid: {exists: true}}}
        {metadata: {labels: {key: \"is_a-nktest_domain\"}}}
        {metadata: {links: {key: \"domains\"}}}
        {metadata: {fts: {word: \"náme\"}}}
    "/utf8>>,
    {1, [
        {<<"User">>, #{<<"uid">>:=UT1, <<"name">>:=<<"ut1">>, <<"domain">>:=<<"b.a-nktest">>}}
    ]} = all_actors_query(Filters1),
    {ok, _, _} = nkdomain_register:get_domain_data('nkdomain-root', "b.a-nktest"),
    ok = nkservice_actor:enable(UT1, false),
    {0, []} = all_actors_query(Filters1),
    ok = nkservice_actor:enable(UT1, true),
    timer:sleep(100),
    {1, [
        {<<"User">>, #{<<"uid">>:=UT1, <<"name">>:=<<"ut1">>, <<"links">>:=[#{<<"value">>:=B1}]}}
    ]} = all_actors_query(Filters1),

    Filters2 = <<"{metadata: {creationTime: {prefix: \"1980\"}}}">>,
    {0, []} = all_actors_query(Filters2),
    Filters3 = <<"{metadata: {generation: {gt: 1000}}}">>,
    {0, []} = all_actors_query(Filters3),

    % Labels
    Filters4 = <<"{metadata: {labels: {key: \"is_b_domain\"}}}">>,
    {3, [
        {<<"Domain">>, #{<<"name">>:=<<"b">>, <<"creationTime">>:=CT1a}},
        {<<"Domain">>, #{<<"name">>:=<<"c">>, <<"creationTime">>:=CT1b}},
        {<<"User">>, #{<<"uid">>:=UT1, <<"creationTime">>:=CT1c}}
    ]=List4} = all_actors_query(Filters4),
    true = (CT1c > CT1b) andalso (CT1b > CT1a),

    Filters5 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", exists:true}}}
    ">>,
    {3, List4} = all_actors_query(Filters5),

    Filters6 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", exists:false}}}
    ">>,
    {1, [{<<"Domain">>, #{<<"name">>:=<<"a-nktest">>}}]} = all_actors_query(Filters6),

    Filters7 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", eq:\"true\"}}}
    ">>,
    {3, List4} = all_actors_query(Filters7),

    Filters8 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", eq:\"tru\"}}}
    ">>,
    {0, []} = all_actors_query(Filters8),

    Filters9 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", prefix:\"tru\"}}}
    ">>,
    {3, List4} = all_actors_query(Filters9),

    Filters10 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", values:[\"true\", \"tru\"]}}}
    ">>,
    {3, List4} = all_actors_query(Filters10),

    Filters11 = <<"
        {kind: {values: [Domain, User]}}
        {metadata: {labels: {key: \"is_b_domain\", lt:\"true\"}}}
    ">>,
    {0, []} = all_actors_query(Filters11),

    % Links
    Filters20 = <<"{metadata: {links: {key: \"domains\", eq: \"", B1/binary, "\"}}}">>,
    {2, [
        {<<"Domain">>, #{<<"name">>:=<<"c">>, <<"creationTime">>:=CT20a}},
        {<<"User">>, #{<<"uid">>:=UT1, <<"creationTime">>:=CT20b}}
    ]} = all_actors_query(Filters20),
    true = CT20b > CT20a,

    Filters21 = <<"{metadata: {links: {key: \"domains\"}}}">>,
    {N21, _} = all_actors_query(Filters21),
    true = N21 > 2,

    Filters22 = <<"{metadata: {links: {key: \"domain2\"}}}">>,
    {0, []} = all_actors_query(Filters22),

    % FTS
    Filters30 = <<"{metadata: {fts: {word: \"náme\"}}}"/utf8>>,
    {1, [{<<"User">>, #{<<"uid">>:=UT1}}]} = all_actors_query(Filters30),

    Filters31 = <<"{metadata: {fts: {word: \"nám\"}}}"/utf8>>,
    {0, []} = all_actors_query(Filters31),

    Filters32 = <<"{metadata: {fts: {word: \"nám*\"}}}"/utf8>>,
    {1, [{<<"User">>, #{<<"uid">>:=UT1}}]} = all_actors_query(Filters32),

    Filters33 = <<"{metadata: {fts: {word: \"náme*\"}}}"/utf8>>,
    {1, [{<<"User">>, #{<<"uid">>:=UT1}}]} = all_actors_query(Filters33),

    Filters34 = <<"{metadata: {fts: {field: \"fts_name\", word: \"náme*\"}}}"/utf8>>,
    {1, [{<<"User">>, #{<<"uid">>:=UT1}}]} = all_actors_query(Filters34),

    Filters35 = <<"{metadata: {fts: {field: \"fts_name2\", word: \"náme*\"}}}"/utf8>>,
    {0, []} = all_actors_query(Filters35),
    ok.


users_test() ->
%%    create_test_data(),
    Upd1 = #{<<"metadata">> => #{<<"isEnabled">>=>false}},
    {ok, _} = api(#{verb=>update, domain=>"b.a-nktest", resource=>users, name=>"ut1", body=>Upd1}),
    Upd2 = #{<<"metadata">> => #{<<"isEnabled">>=>true}},
    {ok, _} = api(#{verb=>update, domain=>"b.a-nktest", resource=>users, name=>"ut1", body=>Upd2}),
    nkdomain_api_events:wait_for_save(),

    %
    Q1 = <<"
        query {
            allUsers(
                deep: true,
                sort: [
                  {metadata: {name: {order: DESC}}}
                ]
            ) {
                totalCount
                actors {
                    kind
                    metadata {
                        name
                    }
                    eventsConnection(last:2) {
                        totalCount
                        actors {
                            reason
                            metadata {
                                name
                                updateTime
                            }
                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allUsers">>:=#{<<"actors">>:=Users1, <<"totalCount">>:=2}}} = request(Q1),
    [
        #{<<"metadata">> := #{<<"name">> := <<"ut1">>}, <<"eventsConnection">> := U1_Evs1},
        #{<<"metadata">> := #{<<"name">> := <<"admin">>}, <<"eventsConnection">> := Admin_Evs1}
    ] = Users1,
    #{<<"totalCount">>:=1, <<"actors">>:=[#{<<"reason">>:=<<"ActorCreated">>}]} = Admin_Evs1,
    #{<<"totalCount">>:=3, <<"actors">>:=[
        #{<<"reason">>:=<<"ActorUpdated">>, <<"metadata">>:=#{<<"updateTime">>:=T3}},
        #{<<"reason">>:=<<"ActorUpdated">>, <<"metadata">>:=#{<<"updateTime">>:=T2}}
    ]} = U1_Evs1,

    Q2 = re:replace(Q1, <<"last">>, <<"first">>, [{return, binary}]),
    io:format("~s\n", [Q2]),
    {ok, #{<<"allUsers">>:=#{<<"actors">>:=Users2, <<"totalCount">>:=2}}} = request(Q2),
    [
        #{<<"metadata">> := #{<<"name">> := <<"ut1">>}, <<"eventsConnection">> := U1_Evs2},
        #{<<"metadata">> := #{<<"name">> := <<"admin">>}, <<"eventsConnection">> := Admin_Evs1}
    ] = Users2,
    #{<<"totalCount">>:=3, <<"actors">>:=[
        #{<<"reason">>:=<<"ActorCreated">>, <<"metadata">>:=#{<<"updateTime">>:=T1}},
        #{<<"reason">>:=<<"ActorUpdated">>, <<"metadata">>:=#{<<"updateTime">>:=T2}}
    ]} = U1_Evs2,

    true = (T3 > T2) andalso (T2 > T1),
    ok.


event_test() ->
    Q1 = <<"
        query {
            allEvents(
                domain: \"b.a-nktest\"
                deep: true,
                sort: [
                  {involvedObject: {kind: {order: DESC}}}
                  {metadata: {creationTime: {order: ASC}}}
                ]
            ) {
                totalCount
                actors {
                    id
                    kind
                    reason
                    message
                    firstTimestamp
                    lastTimestamp
                    count
                    involvedObject {
                        uid
                        apiVersion
                        kind
                        name
                        domain
                        resourceVersion
                    }
                    source {
                        component
                        host
                    }
                    metadata {
                        creationTime
                        name
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allEvents">>:=#{<<"actors">>:=Events1, <<"totalCount">>:=5}}} = request(Q1),
    [
        #{
            <<"id">> := <<"events-", _/binary>>=EvU1,
            <<"kind">> := <<"Event">>,
            <<"reason">> := <<"ActorCreated">>,
            <<"message">> := <<>>,
            <<"firstTimestamp">> := CT2,
            <<"lastTimestamp">> := CT2,
            <<"count">> := 1,
            <<"involvedObject">> := #{
                <<"uid">> := <<"users-", _/binary>>,
                <<"apiVersion">> := <<"core/v1a1">>,
                <<"kind">> := <<"User">>,
                <<"name">> := <<"ut1">>,
                <<"domain">> := <<"b.a-nktest">>,
                <<"resourceVersion">> := _
            },
            <<"source">> := #{
                <<"component">> := <<"nkdomain">>,
                <<"host">> := _
            },
            <<"metadata">> := #{
                <<"creationTime">> := T2,
                <<"name">> := <<"users.ut1.", _/binary>>
            }
        },
        #{
            <<"id">> := <<"events-", _/binary>>=EvU2,
            <<"kind">> := <<"Event">>,
            <<"reason">> := <<"ActorUpdated">>,
            <<"message">> := <<>>,
            <<"firstTimestamp">> := CT3,
            <<"lastTimestamp">> := CT3,
            <<"count">> := 1,
            <<"involvedObject">> := #{
                <<"uid">> := <<"users-", _/binary>>,
                <<"kind">> := <<"User">>,
                <<"name">> := <<"ut1">>,
                <<"domain">> := <<"b.a-nktest">>,
                <<"resourceVersion">> := _
            },
            <<"source">> := #{
                <<"component">> := <<"nkdomain">>,
                <<"host">> := _
            },
            <<"metadata">> := #{
                <<"creationTime">> := T3,
                <<"name">> := <<"users.ut1.", _/binary>>
            }
        },
        #{
            <<"id">> := <<"events-", _/binary>>=EvU3,
            <<"kind">> := <<"Event">>,
            <<"reason">> := <<"ActorUpdated">>,
            <<"message">> := <<>>,
            <<"firstTimestamp">> := CT4,
            <<"lastTimestamp">> := CT4,
            <<"count">> := 1,
            <<"involvedObject">> := #{
                <<"uid">> := <<"users-", _/binary>>,
                <<"kind">> := <<"User">>,
                <<"name">> := <<"ut1">>,
                <<"domain">> := <<"b.a-nktest">>,
                <<"resourceVersion">> := _
            },
            <<"source">> := #{
                <<"component">> := <<"nkdomain">>,
                <<"host">> := _
            },
            <<"metadata">> := #{
                <<"creationTime">> := T4,
                <<"name">> := <<"users.ut1.", _/binary>>
            }
        },
        #{
            <<"id">> := <<"events-", _/binary>>,
            <<"kind">> := <<"Event">>,
            <<"reason">> := <<"ActorCreated">>,
            <<"message">> := <<>>,
            <<"firstTimestamp">> := CT0,
            <<"lastTimestamp">> := CT0,
            <<"count">> := 1,
            <<"involvedObject">> := #{
                <<"uid">> := <<"domains-", _/binary>>,
                <<"kind">> := <<"Domain">>,
                <<"name">> := <<"b">>,
                <<"domain">> := <<"a-nktest">>,
                <<"resourceVersion">> := _
            },
            <<"source">> := #{
                <<"component">> := <<"nkdomain">>,
                <<"host">> := _
            },
            <<"metadata">> := #{
                <<"creationTime">> := T0,
                <<"name">> := <<"domains.b.", _/binary>>
            }
        },
        #{
            <<"id">> := <<"events-", _/binary>>,
            <<"kind">> := <<"Event">>,
            <<"reason">> := <<"ActorCreated">>,
            <<"message">> := <<>>,
            <<"firstTimestamp">> := CT1,
            <<"lastTimestamp">> := CT1,
            <<"count">> := 1,
            <<"involvedObject">> := #{
                <<"uid">> := <<"domains-", _/binary>>,
                <<"kind">> := <<"Domain">>,
                <<"name">> := <<"c">>,
                <<"domain">> := <<"b.a-nktest">>,
                <<"resourceVersion">> := _
            },
            <<"source">> := #{
                <<"component">> := <<"nkdomain">>,
                <<"host">> := _
            },
            <<"metadata">> := #{
                <<"creationTime">> := T1,
                <<"name">> := <<"domains.c.", _/binary>>
            }
        }
    ] = Events1,
    true = (T0 < T1) andalso (T1 < T2) andalso (T2 < T3) andalso (T3 < T4),
    true = (CT0 =< CT1) andalso (CT1 =< CT2) andalso (CT2 =< CT3) andalso (CT3 =< CT4),

    Q2 = <<"
        query {
            allEvents(
                domain: \"b.a-nktest\"
                deep: true,
                filter: {
                    and: [
                        {involvedObject: {kind: {eq: User}}}
                    ]
                }
                sort: [
                  {lastTimestamp: {}}
                  {metadata: {name: {order: DESC}}}
                ]
            ) {
                totalCount
                actors {
                    id
                    kind
                    reason
                    lastTimestamp
                }
            }
        }
    ">>,
    {ok, #{<<"allEvents">>:=#{<<"actors">>:=Events2, <<"totalCount">>:=3}}} = request(Q2),
    [#{<<"id">>:=EvU1}, #{<<"id">>:=EvU2b}, #{<<"id">>:=EvU3b}] = Events2,
    %% TODO Check why:
    true = lists:sort([EvU2b, EvU3b]) == lists:sort([EvU2, EvU3]),

    ok.


%% @doc
contact_test() ->
    api(#{verb=>deletecollection, domain=>"c.b.a-nktest", resource=>contacts, params=>#{
        fieldSelector =>"metadata.subtype:mytest"}}),

    Body1 = <<"
        apiVersion: core/v1a1
        kind: Contact
        spec:
            name: 'My Náme'
            surname: 'My Surname'
            birthTime: 2018-01-01
            gender: M
            timezone: -1
            url:
                - url: url1
                - url: url2
                  type: type2
                  meta:
                    a: 1
            phone:
                - type: mobile
                  phone: 123456
                - type: fixed
                  phone: 654321
            email:
                email: test@test.com
            im:
                - type: irc
                  im: abc
            address:
                - type: home
                  street: 'My street'
                  code: 1234
                  country: Spain
            pubkey:
                - type: github
                  key: abcde
                  meta:
                    key1: val1
            profile:
                - type: type1
                  startTime: 2017-01
                  stopTime: 2018-02
                  data:
                    data1: val1
                  meta:
                    meta1: val1
        metadata:
            subtype: mytest
            name: ct1
            domain: c.b.a-nktest
            links:
                users: /apis/core/v1a1/domains/b.a-nktest/users/ut1
            fts:
                fullName: 'My Náme My Surname'
    "/utf8>>,
    Body2 = yaml(Body1),
    {created, _} = api(#{verb=>create, body=>Body2}),

    Q1 = <<"
        query {
            allContacts(
                domain: \"c.b.a-nktest\"
                sort: [
                    #{spec.normalizedName: {}},
                    #{spec.normalizedSurname: {}},
                    #{spec.gender: {}},
                    #{spec.birthTime: {}},
                    #{spec.timezone: {}}
                ]
            ) {
                totalCount
                actors {
                    id
                    kind
                    spec {
                        name
                        surname
                        normalizedName
                        normalizedSurname
                        birthTime
                        gender
                        timezone
                    url {
                        type
                        url
                        meta {
                            key
                            value
                        }
                    }
                    phone {
                        type
                        phone
                        meta {
                            key
                            value
                        }
                    }
                    email {
                        type
                        email
                        meta {
                            key
                            value
                            }
                        }
                    im {
                        type
                        im
                        meta {
                            key
                         value
                        }
                    }
                    address {
                        type
                        street
                        code
                        city
                        province
                        state
                        country
                        meta {
                            key
                            value
                        }
                    }
                    pubkey {
                        type
                        key
                        meta {
                            key
                            value
                        }
                    }
                    profile {
                        type
                        data {
                            key
                            value
                        }
                        startTime
                        stopTime
                        meta {
                            key
                            value
                        }
                    }

                }
            }
        }
    }
    ">>,
    {ok, #{<<"allContacts">>:=#{<<"actors">>:=[Contact], <<"totalCount">>:=1}}} = request(Q1),
    #{
        <<"id">> := <<"contacts-", _/binary>>,
        <<"kind">> := <<"Contact">>,
        <<"spec">> := #{
            <<"name">> := <<"My Náme"/utf8>>,
            <<"surname">> := <<"My Surname">>,
            <<"normalizedName">> := <<"my name">>,
            <<"normalizedSurname">> := <<"my surname">>,
            <<"gender">> := <<"MALE">>,
            <<"birthTime">> := <<"2018-01-01T00:00:00Z">>,
            <<"timezone">> := -1,
            <<"url">> := [
                #{
                    <<"type">> := null,
                    <<"url">> := <<"url1">>,
                    <<"meta">> := []
                },
                #{
                    <<"type">> := <<"type2">>,
                    <<"url">> := <<"url2">>,
                    <<"meta">> := [
                        #{
                            <<"key">> := <<"a">>,
                            <<"value">> := <<"1">>
                        }
                    ]
                }
            ],
            <<"phone">> := [
                #{
                    <<"type">> := <<"mobile">>,
                    <<"phone">> := <<"123456">>,
                    <<"meta">> := []
                },
                #{
                    <<"type">> := <<"fixed">>,
                    <<"phone">> := <<"654321">>,
                    <<"meta">> := []
                }
            ],
            <<"email">> := [
                #{
                    <<"type">> := null,
                    <<"email">> := <<"test@test.com">>,
                    <<"meta">> := []
                }
            ],
            <<"im">> := [
                #{
                    <<"type">> := <<"irc">>,
                    <<"im">> := <<"abc">>,
                    <<"meta">> := []
                }
            ],
            <<"address">> := [
                #{
                    <<"type">> := <<"home">>,
                    <<"city">> := null,
                    <<"code">> := <<"1234">>,
                    <<"country">> := <<"Spain">>,
                    <<"meta">> := [],
                    <<"province">> := null,
                    <<"state">> := null,
                    <<"street">> := <<"My street">>
                }
            ],
            <<"pubkey">> := [
                #{
                    <<"type">> := <<"github">>,
                    <<"key">> := <<"abcde">>,
                    <<"meta">> := [
                        #{
                            <<"key">> := <<"key1">>,
                            <<"value">> := <<"val1">>
                        }
                    ]
                }
            ],
            <<"profile">> := [
                #{
                    <<"type">> := <<"type1">>,
                    <<"data">> := [
                        #{
                            <<"key">> := <<"data1">>,
                            <<"value">> := <<"val1">>
                        }
                    ],
                    <<"startTime">> := <<"2017-01-01T00:00:00Z">>,
                    <<"stopTime">> := <<"2018-02-01T00:00:00Z">>,
                    <<"meta">> := [
                        #{
                            <<"key">> := <<"meta1">>,
                            <<"value">> := <<"val1">>
                        }
                    ]
                }
            ]
        }
    } = Contact,

    Q2 = <<"
        query {
            allUsers(deep:true, sort: [{metadata: {name: {}}}]) {
                totalCount
                actors {
                    contactsConnection {
                        totalCount
                        actors {
                            kind
                            spec {
                                phone {
                                    phone
                                }
                            }
                        }
                    }
                }
            }
        }
    ">>,
    {ok, #{<<"allUsers">>:=#{<<"actors">>:=Users1, <<"totalCount">>:=2}}} = request(Q2),
    [
        #{
            <<"contactsConnection">> := #{<<"actors">> := [],<<"totalCount">> := 0}
        },
        #{
            <<"contactsConnection">> := #{
                <<"totalCount">> := 1,
                <<"actors">> := [
                    #{
                        <<"kind">> := <<"Contact">>,
                        <<"spec">> := #{
                            <<"phone">> := [
                                #{<<"phone">> := <<"123456">>},
                                #{<<"phone">> := <<"654321">>}
                            ]
                        }
                    }
                ]
            }
        }
    ] = Users1,


    Q3 = <<"
        query {
            allContacts(
                domain: \"c.b.a-nktest\"
            ) {
                totalCount
                actors {
                    kind
                    userConnection {
                        kind
                        metadata {
                            name
                        }
                    }
                }
            }
    }
    ">>,
    {ok, #{<<"allContacts">>:=#{<<"actors">>:=[Contact2], <<"totalCount">>:=1}}} = request(Q3),
    #{
        <<"kind">> := <<"Contact">>,
        <<"userConnection">> := #{
            <<"kind">> := <<"User">>,
            <<"metadata">> := #{
                <<"name">> := <<"ut1">>
            }
        }
    } = Contact2,
    ok.



%% ===================================================================
%% Util
%% ===================================================================

all_actors_query(Filters) ->
    Q = [
        "query {
            allActors(
                deep: true
                sort: [
                    {metadata: {creationTime: {order: ASC}}}
                ],
                filter: {
                    and: [",
        Filters,
        "]
    }
    ) {
        totalCount
        actors {
            ... on Actor {
                kind
                metadata {
                    uid
                    name
                    domain
                    creationTime
                    generation
                    links {
                        key
                        value
                    }
                }
            }
          }
        }
    }"],
    Q2 = list_to_binary(Q),
    io:format("Q: ~s\n", [Q2]),
    {ok, #{<<"allActors">>:=#{<<"actors">>:=Actors, <<"totalCount">>:=Count}}} = request(Q2),
    {Count, [{Type, Data}||#{<<"kind">>:=Type, <<"metadata">>:=Data}<-Actors]}.



request(Query) ->
    nkservice_graphql:request(?ROOT_SRV, Query, #{}).


