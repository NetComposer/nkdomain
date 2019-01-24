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

%% @doc NkDomain Session Actor
%%
%% spec
%% ----
%% - ttlSecs (mandatory)


-module(nkdomain_session_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([config/0, parse/3, sync_op/3, init/2, request/3, stop/2]).


-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").



%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_SESSIONS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => []
    }.


%% @doc
parse(_SrvId, Actor, _ApiReq) ->
    Syntax = #{
        <<"spec">> => #{
            <<"ttlSecs">> => pos_integer,
            '__mandatory' => [<<"ttlSecs">>]
        },
        <<"data">> => map,
        '__mandatory' => [<<"spec">>]
    },
    case nkservice_actor_util:parse_actor(Actor, Syntax) of
        {ok, #actor{data=Data2, metadata=Meta2}=Actor2} ->
            #{<<"spec">>:=#{<<"ttlSecs">>:=Secs}} = Data2,
            Now = nklib_date:epoch(msecs),
            {ok, Expires} = nklib_date:to_3339(Now+1000*Secs, msecs),
            Meta3 = Meta2#{<<"expiresTime">> => Expires},
            {ok, Actor2#actor{metadata=Meta3}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(SrvId, ActorId, #{verb:=get, subresource:=[<<"_rpc">>, <<"refresh">>]}) ->
    case nkservice_actor_srv:sync_op({SrvId, ActorId}, refresh) of
        ok ->
            {status, actor_updated};
        {error, Error} ->
            {error, Error}
    end;

request(_SrvId, _ActorId, _ApiReq) ->
    continue.


%% @doc
init(_Op, #actor_st{unload_policy = {expires, _}}=ActorSt) ->
    % The parser shouldn't allow to get to this point
    {ok, ActorSt}.



%% @doc
sync_op(refresh, _From, ActorSt) ->
    #actor_st{actor=#actor{data=Data, metadata=Meta}=Actor} = ActorSt,
    #{<<"spec">>:=#{<<"ttlSecs">>:=Secs}} = Data,
    Now = nklib_date:epoch(msecs),
    {ok, Expires} = nklib_date:to_3339(Now+1000*Secs, msecs),
    Meta2 = Meta#{<<"expiresTime">> => Expires},
    Actor2 = Actor#actor{metadata = Meta2},
    {ok, ActorSt2} = nkservice_actor_srv:do_update(Actor2, #{}, ActorSt),
    {reply, ok, ActorSt2};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @doc If expires, delete the actor
stop(actor_expired, ActorSt) ->
    {delete, ActorSt};

stop(_Reason, ActorSt) ->
    {ok, ActorSt}.


%% ===================================================================
%% Internal
%% ===================================================================


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).