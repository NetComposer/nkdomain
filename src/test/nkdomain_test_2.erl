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

%% @doc
-module(nkdomain_test_2).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkdomain_test_util, [
        api/1, api_watch/1, wait_api_event/2, api_watch_stop/1,
        http_get/1, http_post/2, http_put/2,
        http_delete/1, http_list/1, http_watch/1, wait_http_event/2, http_watch_stop/1,
        clean_events/0, yaml/1]).

-compile(export_all).
-compile(nowarn_export_all).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").


%% ===================================================================
%% Public
%% ===================================================================

all_tests() ->
    ok = alarm_test(),
    nkdomain_test_util:create_test_data(),
    ok = token_test(),
    ok = config_test(),
    ok = task_test(),
    ok = auto_activate_test(),
    ok = session_test(),
    ok = file_provider_test(),
    ok = file_test(),
    nkdomain_test_util:delete_test_data(),
    ok.



token_test() ->
    % Create a token
    B1 = yaml(<<"
        data:
            key2: val2
        metadata:
            subtype: TestType
            domain: a-nktest
            annotations:
                ann1: value1
    ">>),
    {error, #{<<"reason">>:=<<"ttl_missing">>}} = api(#{verb=>create, resource=>tokens, body=>B1}),

    {created, T1} = api(#{verb=>create, resource=>tokens, body=>B1, params=>#{ttl=>2}}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"Token">>,
        <<"data">> := #{
            <<"key2">> := <<"val2">>
        },
        <<"metadata">> := #{
            <<"uid">> := T1_UID,
            <<"name">> := T1_Name,
            <<"subtype">> := <<"TestType">>,
            <<"domain">> := <<"a-nktest">>,
            <<"expiresTime">> := ET,
            <<"annotations">> := #{
                <<"ann1">> := <<"value1">>
            }
        },
        <<"status">> := #{<<"isActivated">>:=true}
    } = T1,
    Now = nklib_date:epoch(msecs),
    {ok, ET2} = nklib_date:to_epoch(ET, msecs),
    true = (ET2-Now) > 1500 andalso (ET2-Now) < 2500,

    {ok, T1} = api(#{verb=>get, domain=>"a-nktest", resource=>tokens, name=>T1_Name}),

    % Wait for the token to expire
    timer:sleep(2100),
    {error, #{<<"reason">>:=<<"actor_not_found">>}} = api(#{verb=>get, resource=>tokens, name=>T1_Name}),
    nkdomain_api_events:wait_for_save(),
    {ok, #{<<"items">>:=Events, <<"metadata">>:=#{<<"total">>:=2}}} =
        api(#{verb=>list, domain=>"a-nktest", resource=>events, params=>#{
            fieldSelector=><<"involvedObject.uid:", T1_UID/binary>>
        }}),
    [
        #{<<"reason">>:=<<"ActorDeleted">>},
        #{<<"reason">>:=<<"ActorCreated">>, <<"body">>:=#{<<"actor">>:=T1}}
    ] = Events,

    % Test consume
    {created, T2} = api(#{verb=>create, resource=>tokens, body=>B1, params=>#{ttl=>2}}),
    #{<<"metadata">>:=#{<<"name">>:=T2_Name}} = T2,
    {ok, T2} = api(#{verb=>get, domain=>"a-nktest", resource=>tokens, name=>T2_Name}),
    {ok, T2} = api(#{verb=>get, domain=>"a-nktest", resource=>tokens, name=>T2_Name, params=>#{consume=>true}}),
    {error, #{<<"reason">>:=<<"actor_not_found">>}} = api(#{verb=>get, domain=>"a-nktest", resource=>tokens, name=>T2_Name}),

    % Test deletion on load
    {created, T3} = api(#{verb=>create, resource=>tokens, body=>B1, params=>#{ttl=>2}}),
    #{<<"metadata">>:=#{<<"name">>:=T3_Name}} = T3, 
    Path = <<"/a-nktest/core/tokens/", T3_Name/binary>>,
    {true, #actor_id{pid=Pid1}} = nkservice_actor_db:is_activated(Path),

    % We kill the actor, is its activated again on load
    exit(Pid1, kill),
    timer:sleep(50),
    false = nkservice_actor_db:is_activated(Path),
    {ok, T3} = api(#{verb=>get, domain=>"a-nktest", resource=>tokens, name=>T3_Name}),
    timer:sleep(50),
    {true, #actor_id{pid=Pid2}} = nkservice_actor_db:is_activated(Path),
    false = Pid1 == Pid2,

    % We kill again but when we activate it again is expired
    exit(Pid2, kill),
    timer:sleep(2100),
    Params = #{fieldSelector=><<"metadata.name:", T3_Name/binary>>},
    % List don't activate, so it still shows the actor
    {ok, #{<<"metadata">>:=#{<<"total">>:=1}}} = api(#{verb=>list, domain=>"a-nktest", resource=>tokens, params=>Params}),
    {error, #{<<"reason">>:=<<"actor_not_found">>}} = api(#{verb=>get, domain=>"a-nktest", resource=>tokens, name=>T3_Name}),
    {ok, #{<<"metadata">>:=#{<<"total">>:=0}}} = api(#{verb=>list, domain=>"a-nktest", resource=>tokens, params=>Params}),
    ok.


config_test() ->
    api(#{verb=>delete, domain=>"a-nktest", resource=>configmaps, name=>c1}),

    B1 = yaml(<<"
        data:
            key2: val2
        metadata:
            subtype: TestType
            domain: a-nktest
            annotations:
                ann1: value1
    ">>),
    {created, C1} = api(#{verb=>create, resource=>configmaps, name=>c1, body=>B1, params=>#{ttl=>2}}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"ConfigMap">>,
        <<"data">> := #{
            <<"key2">> := <<"val2">>
        },
        <<"metadata">> := #{
            <<"subtype">> := <<"TestType">>,
            <<"domain">> := <<"a-nktest">>,
            <<"uid">> := _C1_UID,
            <<"name">> := <<"c1">>,
            <<"annotations">> := #{
                <<"ann1">> := <<"value1">>
            }
        },
        <<"status">> := #{<<"isActivated">>:=true}
    } = C1,
    {ok, C1} = api(#{verb=>get, domain=>"a-nktest", resource=>configmaps, name=>c1, params=>#{activate=>false}}),
    timer:sleep(2100),
    C2 = C1#{<<"status">>:=#{}},
    {ok, C2} = api(#{verb=>get, domain=>"a-nktest", resource=>configmaps, name=>c1, params=>#{activate=>false}}),
    ok.


task_test() ->
    api(#{verb=>deletecollection, domain=>"a-nktest", resource=>tasks, params=>#{
            fieldSelector => <<"metadata.subtype:TestType">>}}),

    B1 = yaml(<<"
        spec:
            job:
                key1: val1
            maxSecs: 2
        metadata:
            subtype: TestType
            domain: a-nktest

    ">>),
    {created, T1} = api(#{verb=>create, resource=>tasks, body=>B1}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"Task">>,
        <<"spec">> := #{
            <<"job">> := #{<<"key1">> := <<"val1">>},
            <<"maxSecs">> := 2,
            <<"maxTries">> := 3
        },
        <<"metadata">> := #{
            <<"subtype">> := <<"TestType">>,
            <<"domain">> := <<"a-nktest">>,
            <<"name">> := T1_Name
        },
        <<"status">> := #{
            <<"isActivated">> := true,
            <<"status">> := <<"start">>,
            <<"progress">> := 0,
            <<"lastTryStartTime">> := LT1,
            <<"tries">> := 1
        }
    } = T1,

    % Update state, we are on first try
    Url1 = "/domains/a-nktest/tasks/"++binary_to_list(T1_Name),
    {400, #{<<"message">> := <<"Missing field: 'status'">>}} = http_put(Url1 ++ "/_state", #{a=>1}),
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{status=>progress, progress=>50}),

    {200, T2} = http_get(Url1),
    #{
        <<"status">> := #{
            <<"status">> := <<"progress">>,
            <<"isActivated">> := true,
            <<"progress">> := 50,
            <<"lastTryStartTime">> := LT1,
            <<"tries">> := 1
        }
    } = T2,

    % Signal an error. The task will stop
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{status=>error, errorMsg=>err1}),
    timer:sleep(100),

    % We reactivate the task (second try)  and kill again
    {200, T3} = http_get(Url1),
    #{
        <<"status">> := #{
            <<"status">> := <<"start">>,
            <<"isActivated">> := true,
            <<"progress">> := 0,
            <<"lastTryStartTime">> := LT3,
            <<"tries">> := 2
        }
    } = T3,
    true = LT3 > LT1,
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{status=>error, errorMsg=>err1}),
    timer:sleep(100),

    % We reactivate the task (third try) and kill again, it will reach max tries
    {200, T4} = http_get(Url1),
    #{
        <<"status">> := #{
            <<"status">> := <<"start">>,
            <<"isActivated">> := true,
            <<"progress">> := 0,
            <<"lastTryStartTime">> := LT4,
            <<"tries">> := 3
        }
    } = T4,
    true = LT4 > LT3,
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{status=>error, errorMsg=>err1}),
    timer:sleep(100),

    % The task will stop again. If we try to restart it, it fails and it is deleted
    {422, #{<<"reason">>:=<<"task_max_tries_reached">>}} = http_get(Url1),
    {404, _} = http_get(Url1),

    % Get events
    nkdomain_api_events:wait_for_save(),
    {ok, #{<<"items">>:=Events1}} = api(#{verb=>list, domain=>"a-nktest", resource=>events, params=>#{
        fieldSelector=><<"involvedObject.kind:Task,involvedObject.name:", T1_Name/binary>>}}),
    [
        #{
            <<"reason">> := <<"ActorDeleted">>,
            <<"involvedObject">> := #{
                <<"subtype">> := <<"TestType">>
            }
        },
        #{

            <<"reason">> := <<"TaskFaillure">>,
            <<"involvedObject">> := #{
                <<"subtype">> := <<"TestType">>
            },
            <<"message">> := <<"task_max_tries_reached">>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 3}
        },
        #{
            <<"reason">> := <<"TaskError">>,
            <<"message">> := <<"err1">>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 3}
        },
        #{
            <<"reason">> := <<"TaskStart">>,
            <<"message">> := <<>>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 3}
        },
        #{
            <<"reason">> := <<"TaskError">>,
            <<"message">> := <<"err1">>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 2}
        },
        #{
            <<"reason">> := <<"TaskStart">>,
            <<"message">> := <<>>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 2}
        },
        #{
            <<"reason">> := <<"TaskError">>,
            <<"message">> := <<"err1">>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 1}
        },
        #{
            <<"reason">> := <<"TaskStart">>,
            <<"message">> := <<>>,
            <<"body">> := #{
                <<"maxTries">> := 3,
                <<"tries">> := 1,
                <<"job">> := #{<<"key1">> := <<"val1">>},
                <<"maxSecs">> := 2
            }
        },
        #{
            <<"reason">> := <<"ActorCreated">>,
            <<"body">> := #{<<"actor">> := _}
        }
    ] = Events1,

    % We create another task and wait for expire
    {created, T5} = api(#{verb=>create, resource=>tasks, body=>B1}),
    #{
        <<"metadata">> := #{
            <<"name">> := T5_Name
        },
        <<"status">> := #{
            <<"status">> := <<"start">>,
            <<"isActivated">> := true,
            <<"tries">> := 1
        }
    } = T5,
    % Wait for expire, the task will be deleted
    timer:sleep(2100),
    {404, _} = http_get(Url1),


    nkdomain_api_events:wait_for_save(),
    {ok, #{<<"items">>:=Events2}} = api(#{verb=>list, domain=>"a-nktest", resource=>events, params=>#{
        fieldSelector=><<"involvedObject.kind:Task,involvedObject.name:", T5_Name/binary>>}}),
    [
        #{
            <<"reason">> := <<"ActorDeleted">>
        },
        #{

            <<"reason">> := <<"TaskFaillure">>,
            <<"message">> := <<"task_max_time_reached">>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 1}
        },
        #{
            <<"reason">> := <<"TaskStart">>,
            <<"body">> := #{
                <<"maxTries">> := 3,
                <<"tries">> := 1,
                <<"job">> := #{<<"key1">> := <<"val1">>},
                <<"maxSecs">> := 2
            }
        },
        #{
            <<"reason">> := <<"ActorCreated">>
        }
    ] = Events2,

    % Create another instance and do a successful stop
    {created, T6} = api(#{verb=>create, resource=>tasks, body=>B1}),
    #{<<"metadata">> := #{<<"name">> := T6_Name}} = T6,
    ActorPath = <<"/a-nktest/core/tasks/", T6_Name/binary>>,
    ok = nkservice_actor_srv:sync_op(ActorPath, {update_state, #{status=>success}}),
    timer:sleep(150),
    {error, actor_not_found} = nkservice_actor:get_actor(ActorPath),
    nkdomain_api_events:wait_for_save(),
    {ok, #{<<"items">>:=Events3}} = api(#{verb=>list, domain=>"a-nktest", resource=>events, params=>#{
        fieldSelector=><<"involvedObject.kind:Task,involvedObject.subtype:TestType,involvedObject.name:", T6_Name/binary>>}}),
    %%    Ev2 = [maps:with([<<"reason">>, <<"message">>, <<"body">>], E) ||  E <- Events3],
    %%    io:format("NKLOG EV2 ~s\n", [nklib_json:encode_pretty(Ev2)]),
    [
        #{
            <<"reason">> := <<"ActorDeleted">>
        },
        #{
            <<"reason">> := <<"TaskSuccess">>,
            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 1}
        },
        #{
            <<"reason">> := <<"TaskStart">>,
            <<"body">> := #{
                <<"maxTries">> := 3,
                <<"tries">> := 1,
                <<"job">> := #{<<"key1">> := <<"val1">>},
                <<"maxSecs">> := 2
            }
        },
        #{
            <<"reason">> := <<"ActorCreated">>
        }
    ] = Events3,
    ok.


auto_activate_test() ->
    %% See nkdomain_lib:launch_auto_activated/1

    api(#{verb=>deletecollection, domain=>"a-nktest", resource=>tasks, params=>#{
        fieldSelector => <<"metadata.subtype:TestType">>}}),

    Y1 = yaml(<<"
        spec:
            maxSecs: 2
        metadata:
            subtype: TestType
            domain: a-nktest
    ">>),

    % Create 10 tasks expiring in 2 secs
    Pids1 = lists:map(
        fun(Pos) ->
            Name = nklib_util:to_binary(Pos),
            {created, T} = api(#{verb=>create, resource=>tasks, name=>Name, body=>Y1}),
            #{<<"metadata">>:=#{<<"name">>:=Name}} = T,
            P = <<"/a-nktest/core/tasks/", Name/binary>>,
            {ok, #actor_id{pid=Pid}, _} = nkservice_actor:find(P),
            true = is_pid(Pid),
            Pid
        end,
        lists:seq(1, 10)),

    % They are on DB
    {ok, #{<<"metadata">>:=#{<<"total">>:=10}}} =
        api(#{verb=>list, domain=>"a-nktest", resource=>tasks}),

    % They are already activated
    [] = nkdomain_lib:launch_auto_activated(?ROOT_SRV),

    % We kill them
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids1),
    timer:sleep(50),

    % They are reactivated by script
    % (you can experiment with values in size, for example 5)
    ActivatedIds = nkdomain_lib:launch_auto_activated(?ROOT_SRV),
    10 = length(ActivatedIds),
    [] = nkdomain_lib:launch_auto_activated(?ROOT_SRV),
    Pids2 = lists:map(
        fun(ActorId) ->
            {true, #actor_id{pid=Pid}} = nkservice_actor_db:is_activated(ActorId),
            Pid
        end,
        ActivatedIds),

    % We kill them again, and wait for expiration while unloaded
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids2),
    timer:sleep(2100),

    % They expired, but yet on db. When we reactivate them, they are deleted
    {ok, #{<<"metadata">>:=#{<<"total">>:=10}}} =
        api(#{verb=>list, domain=>"a-nktest", resource=>tasks}),
    lager:error("NKLOG START"),
    ActivatedIds = nkdomain_lib:launch_auto_activated(?ROOT_SRV),
    timer:sleep(500),
    {ok, #{<<"metadata">>:=#{<<"total">>:=0}}} =
        api(#{verb=>list, domain=>"a-nktest", resource=>tasks}),
    ok.


alarm_test() ->
    {ok, #{<<"items">>:=[_]}} = api(#{verb=>list, resource=>users}),
    {ok, #{<<"items">>:=[]}} =
        api(#{verb=>list, resource=>users, params=>#{fieldSelector=>"metadata.isInAlarm:true"}}),
    P = "/root/core/users/admin",
    {ok, []} = nkservice_actor_srv:sync_op(P, get_alarms),
    ok = nkservice_actor_srv:sync_op(P, {set_alarm, class1, #{
        <<"code">> => <<"code1">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 1}
    }}),
    ok = nkservice_actor_srv:sync_op(P, {set_alarm, class2, #{
        <<"code">> => <<"code2">>,
        <<"message">> => <<"message2">>
    }}),
    ok = nkservice_actor_srv:sync_op(P, {set_alarm, class1, #{
        <<"code">> => <<"code3">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 2}
    }}),
    {ok, #{
        <<"class1">>:=#{
            <<"code">> := <<"code3">>,
            <<"message">> := <<"message1">>,
            <<"lastTime">> := <<"20", _/binary>>,
            <<"meta">> := #{<<"a">> := 2}
        },
        <<"class2">>:=#{
            <<"code">> := <<"code2">>,
            <<"lastTime">> := <<"20", _/binary>>,
            <<"message">> := <<"message2">>
        }
    }} = nkservice_actor_srv:sync_op(P, get_alarms),

    % Not yet saved
    {ok, #{<<"items">>:=[]}} = api(#{verb=>list, resource=>users, params=>#{fieldSelector=>"metadata.isInAlarm:true"}}),
    {ok, Time} = nkservice_actor_srv:sync_op(P, get_save_time),
    true = is_integer(Time),

    % Don't wait for automatic save
    ok = nkservice_actor_srv:async_op(P, save),
    timer:sleep(50),
    {ok, #{<<"items">>:=[Admin2]}} =
        api(#{verb=>list, resource=>users, params=>#{fieldSelector=>"metadata.isInAlarm:true"}}),
    #{
        <<"metadata">> := #{
            <<"isInAlarm">> := true,
            <<"alarms">> := #{
                <<"class1">>:=#{
                    <<"code">> := <<"code3">>,
                    <<"message">> := <<"message1">>,
                    <<"lastTime">> := <<"20", _/binary>>,
                    <<"meta">> := #{<<"a">> := 2}
                },
                <<"class2">>:=#{
                    <<"code">> := <<"code2">>,
                    <<"lastTime">> := <<"20", _/binary>>,
                    <<"message">> := <<"message2">>
                }
            }
        }
    } = Admin2,

    % Clear
    ok = nkservice_actor_srv:async_op(P, clear_all_alarms),
    timer:sleep(50),
    {ok, Time2} = nkservice_actor_srv:sync_op(P, get_save_time),
    true = is_integer(Time2),
    ok = nkservice_actor_srv:async_op(P, save),
    timer:sleep(50),
    {ok, #{<<"items">>:=[]}} =
        api(#{verb=>list, resource=>users, params=>#{fieldSelector=>"metadata.isInAlarm:true"}}),
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

    {created, S1} = api(#{verb=>create, resource=>sessions, body=>Y1}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"Session">>,
        <<"data">> := #{
            <<"a">> := 1
        },
        <<"metadata">> := #{
            <<"subtype">> := <<"TestType">>,
            <<"domain">> := <<"a-nktest">>,
            <<"uid">> := _S1_UID,
            <<"name">> := <<"s1">>
        },
        <<"status">> := #{<<"isActivated">>:=true}
    } = S1,

    timer:sleep(1000),
    P = "/a-nktest/core/sessions/s1",
    {ok, {expires, Time1}} = nkservice_actor_srv:sync_op(P, get_unload_policy),
    true = (Time1 - nklib_date:epoch(msecs)) < 1000,

    lager:error("NKLOG LAUNCH"),
    {ok, #{<<"reason">>:= <<"actor_updated">>}} =
        api(#{verb=>get, domain=>"a-nktest", resource=>"sessions", name=>s1, subresource=><<"_rpc/refresh">>}),

    {ok, {expires, Time2}} = nkservice_actor_srv:sync_op(P, get_unload_policy),
    true = (Time2 - nklib_date:epoch(msecs)) > 1500,

    timer:sleep(2100),
    {error, #{<<"reason">> := <<"actor_not_found">>}} =
        api(#{verb=>get, domain=>"a-nktest", resource=>"sessions", name=>s1, subresource=><<"_rpc/refresh">>}),
    ok.


file_provider_test() ->
    api(#{verb=>deletecollection, domain=>"a-nktest", resource=>files}),
    api(#{verb=>deletecollection, domain=>"a-nktest", resource=>fileproviders}),

    Y1 = yaml(<<"
        metadata:
            name: fs1
            domain: a-nktest
    ">>),
    {error, #{<<"message">>:=<<"Missing field: 'spec'">>}} = api(#{verb=>create, resource=>fileproviders, body=>Y1}),

    Y1b = yaml(<<"
        spec:
            storageClass: unknown
        metadata:
            name: fs1
            domain: a-nktest
    ">>),
    {error, #{<<"message">>:=<<"Field 'spec.storageClass' is invalid">>}} = api(#{verb=>create, resource=>fileproviders, body=>Y1b}),


    Y2 = yaml(<<"
        spec:
            storageClass: filesystem
            encryptionAlgo: aes_cfb128
        metadata:
            name: fs1
            domain: a-nktest
    ">>),
    {error, #{<<"message">>:=<<"Missing field: 'spec.filesystemConfig'">>}} = api(#{verb=>create, resource=>fileproviders, body=>Y2}),


    YFP1 = yaml(<<"
        kind: FileProvider
        spec:
            storageClass: filesystem
            maxSize: 3
            encryptionAlgo: aes_cfb128
            hashAlgo: sha256
            filesystemConfig:
                filePath: '/tmp'
        metadata:
            name: fs1
            domain: a-nktest
    ">>),

    {created, FP1} = api(#{verb=>create, body=>YFP1}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"FileProvider">>,
        <<"spec">> := #{
            <<"storageClass">> := <<"filesystem">>,
            <<"encryptionAlgo">> := <<"aes_cfb128">>,
            <<"hashAlgo">> := <<"sha256">>,
            <<"filesystemConfig">> := #{
                <<"filePath">> := <<"/tmp">>
            }
        },
        <<"metadata">> := #{
            <<"domain">> := <<"a-nktest">>,
            <<"uid">> := FP1_UID,
            <<"name">> := <<"fs1">>
        },
        <<"status">> := #{<<"isActivated">>:=true}
    } = FP1,
    {ok, _, #{
        id := FP1_UID,
        storageClass := <<"filesystem">>,
        encryptionAlgo := aes_cfb128,
        hashAlgo := sha256,
        maxSize := 3,
        filesystemConfig := #{filePath := <<"/tmp">>}
    }} = nkdomain_file_provider_actor:op_get_spec(?ROOT_SRV, "/a-nktest/core/fileproviders/fs1"),

    SFP2 = <<"
        kind: FileProvider
        spec:
            storageClass: s3
            encryptionAlgo: aes_cfb128
            s3Config:
                scheme: http
                host: localhost
                port: 9000
                key: '5UBED0Q9FB7MFZ5EWIO'
                secret: 'CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI'
                bucket: bucket1
        metadata:
            name: fs2
            domain: a-nktest
    ">>,

    {created, _FP2} = api(#{verb=>create, body=>yaml(SFP2)}),

    SFP3 = re:replace(SFP2, <<"bucket: bucket1">>, <<"bucket: bucket2">>, [{return, binary}]),
    {error, #{<<"message">>:=<<"Tried to update immutable field: 'spec.s3Config.bucket'">>}} = api(#{verb=>update, body=>yaml(SFP3)}),

    SFP4 = re:replace(SFP2, <<"encryptionAlgo:">>, <<"#encryptionAlgo:">>, [{return, binary}]),
    {error, #{<<"message">>:=<<"Tried to update immutable field: 'spec.encryptionAlgo'">>}} = api(#{verb=>update, body=>yaml(SFP4)}),

    SFP5 = re:replace(SFP2, <<"host: localhost">>, <<"host: 127.0.0.1">>, [{return, binary}]),
    {ok, #{<<"spec">>:=#{<<"s3Config">>:=#{<<"host">>:=<<"127.0.0.1">>}}}} = api(#{verb=>update, body=>yaml(SFP5)}),
    ok.


file_test() ->
    api(#{verb=>deletecollection, domain=>"a-nktest", resource=>files}),
    {ok, #{<<"metadata">>:=#{<<"uid">>:=FS1_UID}}} =
        api(#{verb=>get, domain=>"a-nktest", resource=>"fileproviders", name=>fs1}),

    % Create a file with bodyBase64
    Body = base64:encode(<<"123">>),
    Y1 = yaml(<<"
        kind: File
        spec:
            contentType: type1
            bodyBase64: '", Body/binary, "'
            provider: /apis/core/v1a1/domains/a-nktest/fileproviders/fs1
        metadata:
            name: f1
    ">>),
    {created, F1} = api(#{verb=>create, domain=>"a-nktest", body=>Y1}),
    Hash = base64:encode(crypto:hash(sha256, <<"123">>)),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"File">>,
        <<"spec">> := #{
            <<"contentType">> := <<"type1">>,
            <<"externalId">> := F1_UID,
            <<"provider">> := <<"/apis/core/v1a1/domains/a-nktest/fileproviders/fs1">>,
            <<"size">> := 3,
            <<"password">> := _,
            <<"hash">> := Hash
        } = Spec1,
        <<"metadata">> := #{
            <<"uid">> := F1_UID,
            <<"name">> := <<"f1">>,
            <<"links">> := #{
                <<"io.netc.core.domain">> := <<"domains-", _/binary>>,
                <<"io.netc.core.file-provider">> := FS1_UID
            }
        } = Meta1,
        <<"status">> := #{<<"isActivated">> := true}
    } = F1,
    false = maps:is_key(<<"bodyBase64">>, Spec1),

    % Check it is on disk
    {ok, B2} = file:read_file(<<"/tmp/", F1_UID/binary>>),
    3 = byte_size(B2),
    true = B2 /= <<"123">>,

    % Cannot remove provider
    {error, #{<<"reason">>:=<<"actor_has_linked_actors">>}} =
        api(#{verb=>delete, domain=>"a-nktest", resource=>"fileproviders", name=>fs1}),

    % Get the body direct
    {ok, <<"type1">>, <<"123">>} = nkdomain_file_actor:op_get_body(?ROOT_SRV, F1_UID),

    % Get the object and the body inline
    {200, F1} = http_get("/domains/a-nktest/files/f1"),
    {200, F2} = http_get("/domains/a-nktest/files/f1?getBodyInline=true"),
    #{<<"spec">>:=#{<<"bodyBase64">>:=Body}} = F2,

    % Get a direct download
    {ok, {{_, 200, _}, Hds, "123"}} = httpc:request(nkdomain_test_util:http_url("/domains/a-nktest/files/f1/_download")),
    "type1" = nklib_util:get_value("content-type", Hds),


    % Cannot update file
    Y2 = maps:remove(<<"status">>, F1#{
        <<"spec">> := Spec1#{
            <<"contentType">> := <<"type2">>
        }
    }),
    {error, #{ <<"message">>:=<<"Tried to update immutable field: 'spec.contentType'">>}} =
        api(#{verb=>update, domain=>"a-nktest", name=>f1, body=>Y2}),

    % But can add annotations, etc.
    Y3 = maps:remove(<<"status">>, F1#{
        <<"metadata">> := Meta1#{
            <<"annotations">> => #{
                <<"ann1">> => <<"v1">>
            }
        }
    }),
    {ok, F3} = api(#{verb=>update, body=>Y3}),
    #{<<"metadata">> := #{<<"annotations">> := #{<<"ann1">>:=<<"v1">>}}} = F3,

    % Filesystem uses standard downloads
    {200, #{<<"url">> := Url}} = http_get("/domains/a-nktest/files/f1/_rpc/downloadLink"),
    <<"_download">> = lists:last(binary:split(Url, <<"/">>, [global])),

    % Send direct to _upload
    {ok, {{_, 400, _}, _Hds, Body4}} = httpc:request(post, {nkdomain_test_util:http_url("/domains/a-nktest/files/f1/_upload"), [], "ct2", <<"321">>}, [], []),
    #{ <<"message">> := <<"Missing field: 'provider'">>} = nklib_json:decode(Body4),
    {ok, {{_, 201, _}, _, Body5}} =
        httpc:request(post, {nkdomain_test_util:http_url("/domains/a-nktest/files/_upload?provider=/apis/core/v1a1/domains/a-nktest/fileproviders/fs1"), [], "ct2", <<"321">>}, [], []),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"File">>,
        <<"metadata">> := #{
            <<"domain">> := <<"a-nktest">>,
            <<"name">> := _,
            <<"links">> := #{
                <<"io.netc.core.domain">> := <<"domains-", _/binary>>,
                <<"io.netc.core.file-provider">> := <<"fileproviders-", _/binary>>
            }
        },
        <<"spec">> := #{
            <<"contentType">> := <<"ct2">>,
            <<"externalId">> := <<"files-", _/binary>>,
            <<"hash">> := Hash2,
            <<"password">> := _,
            <<"provider">> := <<"/apis/core/v1a1/domains/a-nktest/fileproviders/fs1">>,
            <<"size">> := 3
        }
    } = nklib_json:decode(Body5),
    Hash2 = base64:encode(crypto:hash(sha256, <<"321">>)),

    % Direct to _upload, but through provider, first one is too large
    {ok, {{_, 400, _}, _, Body6}} =
        httpc:request(post, {nkdomain_test_util:http_url("/domains/a-nktest/fileproviders/fs1/files/_upload"), [], "ct3", <<"4321">>}, [], []),
    #{<<"reason">> := <<"file_too_large">>} = nklib_json:decode(Body6),

    % Direct to _upload, but through provider
    {ok, {{_, 201, _}, _, Body7}} =
        httpc:request(post, {nkdomain_test_util:http_url("/domains/a-nktest/fileproviders/fs1/files/_upload"), [], "ct3", <<"321">>}, [], []),
    #{<<"spec">> := #{<<"contentType">>:=<<"ct3">>, <<"hash">>:=Hash2}} = nklib_json:decode(Body7),
    ok.


