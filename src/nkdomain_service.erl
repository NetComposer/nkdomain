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

%% @doc NkDomain service callback module
-module(nkdomain_service).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([syntax/0, config/2]).


%% ===================================================================
%% Public functions
%% ===================================================================


%% @private
syntax() ->
    #{
        listen_ip => host,
        listen_port => {integer, 1, 65535},
        listen_path => basepath,
        listen_secure => boolean,
        start_api_server => boolean,
        start_admin => boolean,
        db_store => binary,
        user_password_pbkdf2_iters => {integer, 1, none},
        default_store_id => binary,
        db_clusters => {list, map},
        '__defaults' => #{
            listen_ip => <<"127.0.0.1">>,
            listen_port => 9301,
            listen_path => <<"/">>,
            listen_secure => false,
            start_api_server => true,
            start_admin => true
        },
        '__mandatory' => [db_store]
    }.


%% @private
config(DomCfg, Config) ->
    #{
        listen_ip := Host,
        listen_port := Port,
        listen_path := Path,
        listen_secure := Secure,
        start_api_server := ApiServer,
        start_admin := Admin
    } = DomCfg,
    BinPort = nklib_util:to_binary(Port),
    Http = case Secure of true -> <<"https">>; false -> <<"http">> end,
    Ws = case Secure of true -> <<"wss">>; false -> <<"ws">> end,
    BaseHttp = <<Http/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    BaseWs = <<Ws/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    Config2 = case ApiServer of
        true ->
            Config#{api_server => <<BaseHttp/binary, "/_api, ", BaseWs/binary, "/_api/ws">>};
        false ->
            Config
    end,
    Config3 = case Admin of
        true ->
            Config2#{admin_url => <<BaseHttp/binary, "/_admin">>};
        false ->
            Config2
    end,
    Cache = case Config of
        #{nkdomain_store:=Store} -> Store;
        _ -> undefined
    end,
    {ok, Config3, Cache}.

