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

%% @doc NkDomain Token Actor
%%
%% Spec
%% ----
%%
%% - type: binary


-module(nkdomain_id_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([config/0, parse/3, sync_op/3, init/2, stop/2, request/3]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Actor Token: "++Txt, Args)).

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
        resource => ?RES_CORE_TOKENS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => []
    }.


%% @doc
parse(_SrvId, Actor, #{params:=Params}) ->
    Syntax = #{<<"data">> => map},
    case nkservice_actor_util:parse_actor(Actor, Syntax) of
        {ok, #actor{metadata=Meta2}=Actor2} ->
            case maps:is_key(<<"expiresTime">>, Meta2) of
                true ->
                    {ok, Actor2};
                false ->
                    case Params of
                        #{ttl:=TTL} when is_integer(TTL), TTL>0 ->
                            Now = nklib_date:epoch(msecs),
                            {ok, Expires} = nklib_date:to_3339(Now+1000*TTL, msecs),
                            Meta3 = Meta2#{<<"expiresTime">> => Expires},
                            {ok, Actor2#actor{metadata = Meta3}};
                        _ ->
                            {error, ttl_missing}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(SrvId, ActorId, #{verb:=get, subresource:=[<<"_execute">>], params:=Params}) ->
    case nkservice_actor_srv:sync_op({SrvId, ActorId}, {token_execute, Params}) of
        {ok, Reply} ->
            {status, Reply};
        {error, Error} ->
            {error, Error}
    end;

request(_SrvId, _ActorId, _ApiReq) ->
    continue.



%% @doc
init(_Op, #actor_st{unload_policy = {expires, _}}=ActorSt) ->
    % The parser shouldn't allow to get to this point
    {ok, ActorSt};

init(_Op, _ActorSt) ->
    {error, expires_missing}.


%% @doc
sync_op({token_execute, _Params}, _From, ActorSt) ->
    % actor_srv_sync_op/3 must be implemented in callback module
    {reply, {error, not_implemented}, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @doc If the token stops normally, delete it
stop(_Reason, ActorSt) ->
    {delete, ActorSt}.


%% ===================================================================
%% Internal
%% ===================================================================


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).