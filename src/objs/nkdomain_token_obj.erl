%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc User Object

-module(nkdomain_token_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create_referred/4, create_standalone/5]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Token "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create_referred(nkservice:id(), nkdomain:id(), #{ttl=>integer()}, map()) ->
    {ok, #{token_id=>nkdomain:obj_id(), ttl=>integer()}, pid()} | {error, term()}.

create_referred(Srv, Parent, Opts, Data) ->
    case nkdomain_obj_lib:load(Srv, Parent, #{}) of
        #obj_id_ext{obj_id=ReferredId, type=SubType} ->
            case check_ttl(SubType, Opts) of
                {ok, SecsTTL} ->
                    Obj = #{
                        parent_id => ReferredId,
                        type => ?DOMAIN_TOKEN,
                        referred_id => ReferredId,
                        subtype => SubType,
                        expires_time => nkdomain_util:timestamp() + 1000*SecsTTL,
                        ?DOMAIN_TOKEN => Data
                    },
                    case nkdomain_obj_lib:make_and_create(Srv, <<>>, Obj, #{}) of
                        {ok, #{obj_id:=TokenId}, Pid} ->
                            {ok, #{token_id=>TokenId, ttl=>SecsTTL}, Pid};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, referred_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec create_standalone(nkservice:id(), nkdomain:id(), binary(), #{ttl=>integer()}, map()) ->
    {ok, #{token_id=>nkdomain:obj_id(), ttl=>integer()}, pid()} | {error, term()}.

create_standalone(Srv, Parent, SubType, Opts, Data) ->
    case check_ttl(SubType, Opts) of
        {ok, SecsTTL} ->
            Obj = #{
                parent_id => Parent,
                type => ?DOMAIN_TOKEN,
                subtype => SubType,
                expires_time => nkdomain_util:timestamp() + 1000*SecsTTL,
                ?DOMAIN_TOKEN => Data
            },
            case nkdomain_obj_lib:make_and_create(Srv, <<>>, Obj, #{}) of
                {ok, #{obj_id:=TokenId}, Pid} ->
                    {ok, #{token_id=>TokenId, ttl=>SecsTTL}, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc
check_ttl(Type, Opts) ->
    Mod = nkdomain_all_types:get_module(Type),
    Info = Mod:object_get_info(),
    DefTTL = maps:get(default_token_ttl, Info, ?DEF_TOKEN_TTL),
    MaxTTL = maps:get(max_token_ttl, Info, ?MAX_TOKEN_TTL),
    case maps:get(ttl, Opts, DefTTL) of
        TTL when TTL>=0, TTL < MaxTTL ->
            {ok, TTL};
        _ ->
            {error, invalid_token_ttl}
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_TOKEN,
        remove_after_stop => true
    }.

%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 5000,
        tree_id => <<"domain_tree_sessions_tokens">>
    }.


%% @private
object_mapping() ->
    disabled.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    any.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_TOKEN, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_send_event(_Event, Session) ->
    {ok, Session}.


%% @private
object_api_cmd(Cmd, Req, State) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_TOKEN, Req, State).


%% @private
object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op(_Op, _Session) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================
