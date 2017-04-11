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
cmd('', create, Data, State) ->
    #{obj_name:=Name, ?DOMAIN_USER_ATOM:=User} = Data,
    #{srv_id:=SrvId, domain:=Domain} = State,
    case nkdomain_user_obj:create(SrvId, Domain, Name, User) of
        {ok, ObjId, Path, _Pid} ->
            {ok, #{obj_id=>ObjId, path=>Path}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', login, #{id:=User}=Data, #{srv_id:=SrvId}=State) ->
    LoginMeta1 = maps:with([session_type, session_id, local, remote], State),
    LoginMeta2 = LoginMeta1#{
        password => maps:get(password, Data, <<>>),
        login_meta => maps:get(meta, Data, #{}),
        session_pid => self()
    },
    case nkdomain_user_obj:login(SrvId, User, LoginMeta2) of
        {ok, UserId, SessId, LoginMeta3} ->
            Reply = #{obj_id=>UserId, session_id=>SessId},
            State2 = case nkdomain_util:get_service_domain(SrvId) of
                undefined -> State;
                Domain -> nkdomain_api_util:add_id(?DOMAIN_DOMAIN, Domain, State)
            end,
            State3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, State2),
            State4 = nkdomain_api_util:add_id(?DOMAIN_SESSION, SessId, State3),
            {login, Reply, UserId, LoginMeta3, State4};
        {error, Error} ->
            {error, Error, State}
    end;

%%cmd('', find_referred, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
%%    case nkdomain_api_util:getid(?DOMAIN_USER, Data, State) of
%%        {ok, Id} ->
%%            Search = nkdomain_user_obj:find_referred(SrvId, Id, Data),
%%            nkdomain_api_util:search(Search, State);
%%        Error ->
%%            Error
%%    end;

cmd(Sub, Cmd, Data, State) ->
    nkdomain_api_util:cmd_common(Sub, Cmd, Data, ?DOMAIN_USER, State).
