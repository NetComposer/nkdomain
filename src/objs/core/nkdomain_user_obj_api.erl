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

%% @doc
cmd(<<"login">>, #nkreq{session_module=nkapi_server, user_meta=UserMeta}=Req) ->
    #nkreq{data=#{id:=User}=Data, srv_id=SrvId, session_id=SessId, session_meta=SessMeta} = Req,
    case get_domain(Req) of
        {ok, DomainId} ->
            LoginOpts1 = maps:with([local, remote], SessMeta),
            LoginMeta = maps:get(meta, Data, #{}),
            LoginOpts2 = LoginOpts1#{
                session_id => SessId,
                domain_id => DomainId,
                password => maps:get(password, Data, <<>>),
                login_meta => LoginMeta,
                api_server_pid => self()
            },
            case nkdomain_user_obj:login(SrvId, User, LoginOpts2) of
                {ok, UserId, SessId} ->
                    Reply = #{obj_id=>UserId, session_id=>SessId},
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

cmd(<<"login">>, #nkreq{session_module=nkapi_server_http}=Req) ->
    #nkreq{data=#{id:=User}=Data, srv_id=SrvId, session_id=SessId, session_meta=SessMeta} = Req,
    case get_domain(Req) of
        {ok, DomainId} ->
            LoginMeta1 = maps:with([local, remote], SessMeta),
            LoginMeta2 = LoginMeta1#{
                session_id => SessId,
                domain_id => DomainId,
                password => maps:get(password, Data, <<>>),
                login_meta => maps:get(meta, Data, #{})
            },
            LoginMeta3 = case Data of
                #{ttl:=TTL} -> LoginMeta2#{ttl=>TTL};
                _ -> LoginMeta2
            end,
            case nkdomain_user_obj:token(SrvId, User, LoginMeta3) of
                {ok, Reply} ->
                    {ok, Reply};
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
get_domain(#nkreq{data = #{domain_id:=Domain}, srv_id=SrvId}) ->
    case nkdomain_obj_lib:find(SrvId, Domain) of
        #obj_id_ext{obj_id=DomainId} ->
            {ok, DomainId};
        {error, _} ->
            {error, domain_unknown}
    end;

get_domain(#nkreq{srv_id=SrvId}) ->
    case nkdomain_util:get_service_domain(SrvId) of
        undefined ->
            {error, domain_unknown};
        DomainId ->
            {ok, DomainId}
    end.


