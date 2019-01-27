%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

-module(nkdomain_rpc_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([rpc9_parse/4, rpc9_allow/4, rpc9_request/4, rpc9_event/4]).


%% ===================================================================
%% API callbacks
%% ===================================================================


%% @doc
rpc9_parse(<<"core/login", _Cmd/binary>>, _Data, _Req, State) ->
    {syntax, #{token=>binary, '__mandatory'=>[token]}, State};

rpc9_parse(<<"core/", _Cmd/binary>>, _Data, _Req, _State) ->
    continue;

rpc9_parse(_Cmd, _Data, _Req, _State) ->
    continue.


%% @doc
rpc9_allow(<<"core/login">>, #{token:=Token}, _Req, #{baseSrv:=SrvId}) ->
    case nkdomain_plugin:get_domain_cache(SrvId, adminToken) of
        Token ->
            true;
        _ ->
            false
    end;

rpc9_allow(<<"core/", _Cmd/binary>>, _Data, _Req, _State) ->
    false;

rpc9_allow(_Cmd, _Data, _Req, _State) ->
    continue.


%% @doc
%% Server requests
rpc9_request(<<"core/login">>, _Data, _Req, State) ->
    {login, <<"middle9">>, #{}, State};

rpc9_request(<<"core/", _Cmd/binary>>, _Data, _Req, _State) ->
    {error, not_implemented};

rpc9_request(_Cmd, _Data, _Req, _State) ->
    continue.


%% @doc
rpc9_event(Event, _Data, Req, _State) ->
    lager:error("NKLOG EV ~p ~p ~p", [Event, Req]),
    continue.
