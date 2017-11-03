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
-module(nkdomain_user_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).


-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd(<<"login">>, Req) ->
    nkdomain_session_obj:object_api_cmd(<<"start">>, Req);

cmd(<<"get_name">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_USER, Data, Req) of
        {ok, Id} ->
            nkdomain_user:get_name(Id);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_info">>, #nkreq{srv_id=SrvId, data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_USER, Data, Req) of
        {ok, Id} ->
            SrvId2 = maps:get(srv_id, Data, SrvId),
            Opts = maps:with([domain_id, session_types], Data),
            nkdomain_user:get_info(Id, Opts#{srv_id=>SrvId2});
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_token">>, Req) ->
    #nkreq{
        session_id = SessId,
        session_meta = SessMeta
    } = Req,
    SessMeta2 = maps:with([session_id, local, remote], SessMeta),
    SessMeta3 = SessMeta2#{session_id=>SessId},
    Req2 = Req#nkreq{session_meta = SessMeta3},
    case nkdomain_api_util:token_login(Req2) of
        {ok, TokenId, TTL} ->
            {ok, #{<<"token_id">>=>TokenId, <<"ttl">>=>TTL}};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"add_push_device">>, #nkreq{srv_id=SrvId, data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_USER, Data, Req) of
        {ok, Id} ->
            case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
                {ok, DomainId} ->
                    #{device_id:=DeviceId, push_data:=PushData} = Data,
                    SrvId2 = maps:get(srv_id, Data, SrvId),
                    nkdomain_user:add_push_device(Id, DomainId, SrvId2, DeviceId, PushData);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"remove_push_device">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_USER, Data, Req) of
        {ok, Id} ->
            #{device_id:=DeviceId} = Data,
            nkdomain_user:remove_push_device(Id, DeviceId);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_status">>, #nkreq{srv_id=SrvId, data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_USER, Data, Req) of
        {ok, Id} ->
            case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
                {ok, DomainId} ->
                    SrvId2 = maps:get(srv_id, Data, SrvId),
                    nkdomain_user:get_status(Id, SrvId2, DomainId);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"set_status">>, #nkreq{srv_id=SrvId, data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_USER, Data, Req) of
        {ok, Id} ->
            case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
                {ok, DomainId} ->
                    #{status:=Status} = Data,
                    SrvId2 = maps:get(srv_id, Data, SrvId),
                    nkdomain_user:set_status(Id, SrvId2, DomainId, Status);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:cmd(Cmd, ?DOMAIN_USER, Req).


%% ===================================================================
%% Private
%% ===================================================================
