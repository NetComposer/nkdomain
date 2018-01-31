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

%% @doc Session Object API
-module(nkadmin_session_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include("nkdomain.hrl").


-define(ADMIN_DEF_EVENT_TYPES, [
    <<"update_elements">>,
    <<"unloaded">>
]).


%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd(<<"find">>, _Req) ->
    {error, session_not_found};

cmd(<<"create">>, Req) ->
    cmd(<<"start">>, Req);

cmd(<<"start">>, #nkreq{session_module=nkapi_server, session_id=WsSessId}=Req) ->
    #nkreq{data=Data, session_pid=Pid, user_id=UserId} = Req,
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            {ok, _, _, DomainPath, _} = nkdomain:find(DomainId),
            % TODO: properly add these new filters
            ExtraFilters = case DomainPath of
                <<"/sphera", _/binary>> ->
                    [{'not', {<<"path">>, prefix, <<"/sipstorm">>}}, {'not', {<<"path">>, prefix, <<"/dkv">>}}];
                <<"/dkv", _/binary>> ->
                    [{'not', {<<"path">>, prefix, <<"/sipstorm">>}}, {'not', {<<"path">>, prefix, <<"/sphera">>}}];
                <<"/sipstorm", _/binary>> ->
                    [{'not', {<<"path">>, prefix, <<"/sphera">>}}, {'not', {<<"path">>, prefix, <<"/dkv">>}}];
                _ ->
                    [{'not', {<<"path">>, prefix, <<"/sphera">>}}, {'not', {<<"path">>, prefix, <<"/dkv">>}}]
            end,
            Opts1 = maps:with([domain_id, url, language], Data),
            Opts2 = Opts1#{
                session_link => {nkapi_server, Pid},
                session_events => maps:get(session_events, Data, ?ADMIN_DEF_EVENT_TYPES),
                extra_filters => ExtraFilters,
                http_auth_id => WsSessId            % To get files
            },
            case nkadmin_session_obj:start(DomainId, UserId, Opts2) of
                {ok, SessId, _Pid, Updates} ->
                    Req2 = nkdomain_api_util:add_id(?DOMAIN_ADMIN_SESSION, SessId, Req),
                    {ok, Updates#{obj_id=>SessId}, Req2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"stop">>, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, SessId} ->
            nkdomain:unload(SessId, user_stop);
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"switch_domain">>, #nkreq{data=#{domain_id:=DomId}=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, SessId} ->
            Url = maps:get(url, Data, <<>>),
            case nkadmin_session_obj:switch_domain(SessId, DomId, Url) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"element_action">>, #nkreq{data=Data}=Req) ->
    #{element_id:=ElementId, action:=Action} = Data,
    Value = maps:get(value, Data, <<>>),
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, SessId} ->
            case nkadmin_session_obj:element_action(SessId, ElementId, Action, Value) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_data">>, #nkreq{data=Data}=Req) ->
    #{element_id:=ElementId} = Data,
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, SessId} ->
            case nkadmin_session_obj:get_data(SessId, ElementId, Data) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_chart_data">>, #nkreq{data=Data}=Req) ->
    #{element_id:=ElementId} = Data,
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, SessId} ->
            case nkadmin_session_obj:get_chart_data(SessId, ElementId, Data) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:cmd(Cmd, ?DOMAIN_ADMIN_SESSION, Req).



%% ===================================================================
%% Internal
%% ===================================================================

