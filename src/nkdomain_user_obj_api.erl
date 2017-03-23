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
    LoginMeta1 = maps:with([session_type, session_id, local, remote], State),
    LoginMeta2 = LoginMeta1#{
        password => maps:get(password, Data, <<>>),
        login_meta => maps:get(meta, Data, #{}),
        session_pid => self()
    },
    case nkdomain_user_obj:login(SrvId, User, LoginMeta2) of
        {ok, UserId, SessId, LoginMeta3} ->
            {login, #{obj_id=>UserId, session_id=>SessId}, UserId, LoginMeta3, State};
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

cmd('', find_referred, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
    Search = nkdomain_user_obj:find_referred(SrvId, Id, Data),
    nkdomain_util:search_api(Search, State);

cmd('', create, #{obj_name:=Name, user:=User}, #{srv_id:=SrvId}=State) ->
    case nkdomain_user_obj:create(SrvId, Name, User) of
        {ok, ObjId, _Pid} ->
            {ok, #{obj_id=>ObjId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', Cmd, Data, State) ->
    nkdomain_util:api_common(?DOMAIN_USER, Cmd, Data, State);

cmd(_Sub, _Cmd, _Data, State) ->
    {error, not_implemented, State}.
