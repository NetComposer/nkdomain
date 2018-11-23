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

%% @doc NkDomain User Actor

-module(nkdomain_user_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([config/0, parse/3, sync_op/3, request/3, make_external/3]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Actor User: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkpacket/include/nkpacket.hrl").

-define(MAGIC_PASS, <<226,141,134,226,132,153,226,141,133>>). %%  "⍆ℙ⍅"/utf8
-define(INVALID_PASS_SLEEP, 250).


%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_USERS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => [u]
    }.


%% @doc
parse(SrvId, _Actor, _ApiReq) ->
    Fun = fun(Pass) -> {ok, store_pass(SrvId, Pass)} end,
    {syntax, #{<<"spec">>=>#{<<"password">> => Fun}}}.


%% @doc
request(SrvId, ActorId, #{verb:=get, subresource:=[<<"_checkpass">>], params:=Params}) ->
    Syntax = #{password => binary},
    case nklib_syntax:parse(Params, Syntax) of
        {ok, #{password:=Pass}, _} ->
            case nkservice_actor_srv:sync_op({SrvId, ActorId}, {check_pass, Pass}) of
                {ok, true} ->
                    {status, password_valid};
                {ok, false} ->
                    timer:sleep(?INVALID_PASS_SLEEP),
                    {status, password_invalid};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {parameter_missing, <<"password">>}}
    end;

request(_SrvId, _ActorId, _ApiReq) ->
    continue.


%% @doc
make_external(_SrvId, #actor{data=Data}=Actor, _Vsn) ->
    Spec1 = maps:get(<<"spec">>, Data, #{}),
    Spec2 = Spec1#{<<"password">> => <<>>},
    {ok, Actor#actor{data=Data#{<<"spec">>=>Spec2}}}.


%% @doc
sync_op({check_pass, Pass}, _From, ActorSt) ->
    #actor_st{srv=SrvId, actor=#actor{data=Data}} = ActorSt,
    #{<<"spec">>:=Spec} = Data,
    Stored = maps:get(<<"password">>, Spec, <<>>),
    Result =  {ok, store_pass(SrvId, Pass) == Stored},
    {reply, Result, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc Generates a password from an user password or hash
-spec store_pass(nkservice:id(), string()|binary()) ->
    binary().

store_pass(SrvId, Pass) ->
    Pass2 = to_bin(Pass),
    case binary:split(Pass2, ?MAGIC_PASS) of
        [<<>>, _] ->
            Pass2;
        _ ->
            Salt = <<"netcomposer">>,
            Iters = nkdomain_plugin:get_domain_cache(SrvId, pbkdfIters),
            {ok, Pbkdf2} = pbkdf2:pbkdf2(sha, Pass2, Salt, Iters),
            Hash = nklib_util:lhash(Pbkdf2),
            <<?MAGIC_PASS/binary, Hash/binary>>
    end.


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).