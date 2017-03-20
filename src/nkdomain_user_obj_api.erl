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

%% @doc User Object API
-module(nkdomain_user_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include("nkdomain.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd('', login, #{id:=User}=Data, #{srv_id:=SrvId}=State) ->
    Password = maps:get(password, Data, <<>>),
    case nkdomain_user_obj:login(SrvId, User, Password, #{}) of
        {ok, UserId} ->
            Meta = maps:get(meta, Data, #{}),
            {login, #{obj_id=>UserId}, UserId, Meta, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', get_token, #{id:=User}=Data, #{srv_id:=SrvId}=State) ->
    Password = maps:get(password, Data, <<>>),
    case nkdomain_user_obj:login(SrvId, User, Password, #{}) of
        {ok, UserId} ->
            {ok, #{obj_id=>UserId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', create, Data, State) ->
    nkdomain_util:api_create(?DOMAIN_USER, Data, State);

cmd('', remove, Data, State) ->
    nkdomain_util:api_remove(Data, State);

cmd('', update, Data, State) ->
    nkdomain_util:api_update(Data, State);

cmd(_Sub, _Cmd, _Data, State) ->
    {error, not_implemented, State}.
