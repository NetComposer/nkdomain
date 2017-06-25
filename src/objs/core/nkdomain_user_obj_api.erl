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

-export([cmd/2]).


-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% API
%% ===================================================================

%% @doc WS login
cmd(<<"login">>, #nkreq{conn_id=Pid, session_module=nkapi_server, user_meta=UserMeta}=Req) ->
    #nkreq{data=#{id:=User}=Data, srv_id=SrvId, session_id=SessId, session_meta=SessMeta} = Req,
    Auth = #{password => maps:get(password, Data, <<>>)},
    case get_domain(User, Req) of
        {ok, DomainId} ->
            case nkdomain_user_obj:auth(SrvId, User, Auth) of
                {ok, UserId} ->
                    LoginMeta = maps:get(meta, Data, #{}),
                    SessOpts1 = maps:with([local, remote], SessMeta),
                    SessOpts2 = SessOpts1#{
                        session_id => SessId,
                        login_meta => LoginMeta,
                        api_server_pid => Pid
                    },
                    case nkdomain_session_obj:start(SrvId, DomainId, UserId, SessOpts2) of
                        {ok, SessId, _Pid} ->
                            Reply = #{user_id=>UserId, session_id=>SessId},
                            UserMeta1 = UserMeta#{login_meta=>LoginMeta},
                            UserMeta2 = nkdomain_api_util:add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
                            UserMeta3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, UserMeta2),
                            UserMeta4 = nkdomain_api_util:add_id(?DOMAIN_SESSION, SessId, UserMeta3),
                            {login, Reply, UserId, UserMeta4};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

%% @doc HTTP login
cmd(<<"login">>, #nkreq{session_module=nkapi_server_http}=Req) ->
    #nkreq{data=#{id:=User}=Data, srv_id=SrvId, session_id=SessId, session_meta=SessMeta} = Req,
    Auth = #{password => maps:get(password, Data, <<>>)},
    case get_domain(User, Req) of
        {ok, DomainId} ->
            case nkdomain_user_obj:auth(SrvId, User, Auth) of
                {ok, UserId} ->
                    TokenData1 = maps:with([local, remote], SessMeta),
                    TokenData2 = TokenData1#{
                        session_id => SessId,
                        login_meta => maps:get(meta, Data, #{})
                    },
                    TokenOpts = maps:with([ttl], Data),
                    case nkdomain_user_obj:make_token(SrvId, DomainId, UserId, TokenOpts, TokenData2) of
                        {ok, TokenId, TTL} ->
                            {ok, #{token_id=>TokenId, ttl=>TTL}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_USER, Req).


%% ===================================================================
%% Private
%% ===================================================================

%% @private
get_domain(_User, #nkreq{data = #{domain_id:=Domain}, srv_id=SrvId}) ->
    load_domain(SrvId, Domain);

get_domain(User, #nkreq{srv_id=SrvId}) ->
    case nkdomain_lib:find(SrvId, User) of
        #obj_id_ext{path=Path} ->
            {ok, Domain, _} = nkdomain_util:get_parts(?DOMAIN_USER, Path),
            load_domain(SrvId, Domain);
        {error, Error} ->
            {error, Error}
    end.


%% @private
load_domain(SrvId, Domain) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{obj_id=DomainId} ->
            {ok, DomainId};
        {error, _} ->
            {error, {domain_unknown, Domain}}
    end.

