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
-module(nkdomain_session_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

%% ===================================================================
%% API
%% ===================================================================

cmd(<<"start">>, #nkreq{session_module=nkapi_server}=Req) ->
    #nkreq{
        session_id = SessId,
        session_pid = SessPid,
        session_meta = SessMeta
    } = Req,
    SessMeta2 = maps:with([local, remote], SessMeta),
    SessMeta3 = SessMeta2#{session_id=>SessId, session_link=>{nkapi_server, SessPid}},
    Req2 = Req#nkreq{session_meta=SessMeta3},
    case nkdomain_api_util:session_login(Req2) of
        {ok, DomainId, UserId, SessId, _SessPid, Req3} ->
            Reply = #{domain_id=>DomainId, user_id=>UserId, session_id=>SessId},
            {ok, Reply, Req3};
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:cmd(Cmd, ?DOMAIN_SESSION, Req).
