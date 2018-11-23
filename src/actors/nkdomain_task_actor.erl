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

%% @doc NkDomain Token Config
%% Usage
%% - implementations must listen to update_state event to detect a new task
%%   starting or stopping, and send periodic refreshes as sync operations
%%   '{updated_state, run_state()}'
%%
%% Spec
%% ----
%% - job: map
%% - maxTries: integer
%% - maxSecs: integer
%%
%% Status
%% ------
%%
%% - lastTryStartTime: Time for last try (on disk)
%% - tries: Number of tries so far (on disk)
%% If reading from activated task, other fields ar available:
%% - status: progress, success, error
%% - progress: 0 to 100
%% - errorMsg: binary
%%

-module(nkdomain_task_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([config/0, parse/3, request/5, init/1, event/2,
         sync_op/3, async_op/2, stop/2, make_external/3]).
-export_type([event/0, run_state/0]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkservice/include/nkservice_actor_debug.hrl").
-include_lib("nkpacket/include/nkpacket.hrl").

-define(DEFAULT_MAX_TRIES, 3).
-define(DEFAULT_MAX_TASK_SECS, 60*60).

%% ===================================================================
%% Types
%% ===================================================================


-type event() ::
    {updated_state, run_state()}.


-type run_state() ::
    #{
        subtype := binary(),
        status := init | start | progress | error | success | faillure,
        progress := 0..100,
        errorMsg => binary(),
        tries := pos_integer()
    }.


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_TASKS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        auto_activate => true,
        filter_fields => [
            <<"status.lastTryStartTime">>,
            <<"status.tries">>
        ],
        sort_fields => [
            <<"status.lastTryStartTime">>,
            <<"status.tries">>
        ],
        field_type => #{
            <<"status.tries">> => integer
        }
    }.


%% @doc
parse(_SrvId, Actor, ApiReq) ->
    Syntax = #{
        <<"spec">> => #{
            <<"job">> => map,
            <<"maxTries">> => pos_integer,
            <<"maxSecs">> => pos_integer,
            '__defaults' => #{
                <<"maxTries">> => ?DEFAULT_MAX_TRIES,
                <<"maxSecs">> => ?DEFAULT_MAX_TASK_SECS
            }
        },
        '__mandatory' => [<<"spec">>]
    },
    % Set expiresTime based on maxSecs
    case nkdomain_actor_util:parse_actor(Actor, Syntax, ApiReq) of
        {ok, #actor{metadata=Meta}=Actor2} ->
            #actor{data=#{<<"spec">>:=#{<<"maxSecs">>:=MaxSecs}}, metadata=Meta} = Actor2,
            Now = nklib_date:epoch(msecs),
            {ok, Expires} = nklib_date:to_3339(Now+1000*MaxSecs, msecs),
            Meta2 = Meta#{<<"expiresTime">> => Expires},
            {ok, Actor2#actor{metadata=Meta2}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(SrvId, update, ActorId, _Config, #{subresource:=[<<"_state">>], body:=Body}) ->
    case nkservice_actor_srv:sync_op({SrvId, ActorId}, {update_state, Body}) of
        ok ->
            {status, actor_updated};
        {error, Error} ->
            {error, Error}
    end;

request(_SrvId, _Verb, _ActorId, _Config, _ApiReq) ->
    continue.


%% @doc
%% - generate initial run_state
%% - check if the numbers of tries have been exceeded,
%%   and delete the actor if exceeded
%% - program 'updated_state' event with progress=start (see event/2)
%% - save updated status
init(#actor_st{unload_policy={expires, _}, actor=Actor}=ActorSt) ->
    #actor{data=Data, metadata=Metadata} = Actor,
    ActorStatus1 = maps:get(<<"status">>, Data, #{}),
    Tries = maps:get(<<"tries">>, ActorStatus1, 0),
    #{<<"spec">> := #{<<"maxTries">>:=MaxTries}} = Data,
    RunState2 = #{
        subtype => maps:get(<<"subtype">>, Metadata, <<>>),
        status => init,
        progress => 0,
        tries => Tries+1
    },
    case Tries >= MaxTries of
        false ->
            Now = nklib_date:now_3339(msecs),
            ActorStatus2 = ActorStatus1#{
                <<"lastTryStartTime">> => Now,
                <<"tries">> => Tries+1
            },
            Data2 = Data#{<<"status">> => ActorStatus2},
            Actor2 = Actor#actor{data=Data2},
            % We save the new status to disk
            ActorSt2 = ActorSt#actor_st{
                actor = Actor2,
                run_state = RunState2,
                is_dirty = true
            },
            % We don't want to call set_run_state/2 yet, because the start
            % event would arrive before the creation event
            nkservice_actor_srv:async_op(self(), {update_state, #{status=>start}}),
            {ok, ActorSt2};
        true ->
            RunState3 = RunState2#{
                status := faillure,
                errorMsg => <<"task_max_tries_reached">>
            },
            % Allow in-queue events to be processed
            % timer:sleep(100),
            ActorSt2 = set_run_state(RunState3, ActorSt),
            ?ACTOR_LOG(warning, "max tries reached for task", [], ActorSt2),
            {delete, task_max_tries_reached}
    end;

init(_ActorSt) ->
    {error, expires_missing}.



%% @doc Called on every event launched at this actor (our's or not)
%% Used to generate API events
event({updated_state, UpdStatus}, #actor_st{actor=Actor}=ActorSt) ->
    #actor{data=Data} = Actor,
    #{
        <<"spec">> := #{<<"maxTries">> := MaxTries},
        <<"status">> := #{<<"tries">> := Tries}
    } = Data,
    #{status:=Status} = UpdStatus,
    ApiEvBody = #{
        <<"tries">> => Tries,
        <<"maxTries">> => MaxTries
    },
    ActorSt2 = case Status of
        init ->
            ActorSt;
        start ->
            % For 'start', include all spec in API event body
            Spec = maps:get(<<"spec">>, Data, #{}),
            ApiEvBody2 = maps:merge(Spec, ApiEvBody),
            ApiEv = #{reason => <<"TaskStart">>, body => ApiEvBody2},
            nkdomain_actor_util:api_event(ApiEv, ActorSt);
        progress ->
            ActorSt;
        error ->
            ErrMsg = maps:get(errorMsg, UpdStatus, <<>>),
            ApiEv = #{reason => <<"TaskError">>, message=>ErrMsg, body => ApiEvBody},
            nkdomain_actor_util:api_event(ApiEv, ActorSt);
        success ->
            ApiEv = #{reason => <<"TaskSuccess">>, body => ApiEvBody},
            nkdomain_actor_util:api_event(ApiEv, ActorSt);
        faillure ->
            ErrMsg = maps:get(errorMsg, UpdStatus, <<>>),
            ApiEv = #{reason => <<"TaskFaillure">>, message=>ErrMsg, body => ApiEvBody},
            nkdomain_actor_util:api_event(ApiEv, ActorSt)
    end,
    {ok, ActorSt2};

event(_Event, _ActorSt) ->
    continue.


%% @doc
sync_op({update_state, Body}, _From, ActorSt) ->
    {Reply, ActorSt2} = do_update_state(Body, ActorSt),
    {reply, Reply, ActorSt2};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @doc
async_op({update_state, Body}, ActorSt) ->
    {ok, ActorSt2} = do_update_state(Body, ActorSt),
    {noreply, ActorSt2};

async_op(_Op, _ActorSt) ->
    continue.


%% @doc
stop(actor_expired, ActorSt) ->
    RunState = #{
        status => faillure,
        errorMsg => <<"task_max_time_reached">>
    },
    ActorSt2 = set_run_state(RunState, ActorSt),
    ?ACTOR_LOG(warning, "max time reached for task", [], ActorSt2),
    {delete, ActorSt2};

stop(_Reason, ActorSt) ->
    {ok, ActorSt}.


%% @doc
make_external(_SrvId, #actor{data=Data, run_state=RunState}=Actor, _Vsn) when is_map(RunState) ->
    Status1 = maps:get(<<"status">>, Data, #{}),
    RunState2 = maps:with([status, progress, errorMsg], RunState),
    RunState3 = nklib_json:json_ish(RunState2),
    Status2 = maps:merge(Status1, RunState3),
    {ok, Actor#actor{data=Data#{<<"status">>=>Status2}}};

make_external(_SrvId, _Actor, _Vsn) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================

%% @doc
do_update_state(Body, ActorSt) ->
    Syntax = #{
        status => {atom, [start, progress, error, success, faillure]},
        progress => {integer, 0, 100},
        errorMsg => binary,
        '__mandatory' => [status]
    },
    case nkdomain_actor_util:parse(Body, Syntax) of
        {ok, RunState} ->
            {ok, set_run_state(RunState, ActorSt)};
        {error, Error} ->
            {{error, Error}, ActorSt}
    end.


%% @private
%% - Sets a new run_state
%% - Send event
set_run_state(NewRunState, #actor_st{run_state=RunState}=ActorSt) ->
    RunState1 = case RunState of
        undefined ->
            #{};
        _ ->
            RunState
    end,
    RunState2 = maps:merge(RunState1, NewRunState),
    Now = nklib_date:epoch(secs),
    RunState3 = RunState2#{updateEpochSecs => Now},
    case maps:get(status, RunState3) of
        success ->
            % Allow events
            nkservice_actor_srv:delayed_async_op(none, self(), delete, 100);
        error ->
            nkservice_actor_srv:delayed_async_op(none, self(), {stop, task_status_error}, 100);
        faillure ->
            nkservice_actor_srv:delayed_async_op(none, self(), delete, 100);
        _ ->
            ok
    end,
    ActorSt2 = ActorSt#actor_st{run_state=RunState3},
    Event = {updated_state, RunState3},
    % Sleep so that it won't go to the same msec
    timer:sleep(2),
    nkdomain_actor_util:event(Event, ActorSt2).








