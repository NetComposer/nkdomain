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

%% @doc Elasticsearch plugin
-module(nkdomain_store_pgsql_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).
-export([plugin_start/2, plugin_stop/2]).



%%-define(LLOG(Type, Txt, Args),
%%    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%%-record(es_config, {
%%    index,
%%    type,
%%    archive_index
%%}).


%% ===================================================================
%% Public
%% ===================================================================


%% ===================================================================
%% Plugin Callbacks
%% ===================================================================

plugin_deps() ->
    [nkdomain].


plugin_syntax() ->
    #{
        domain_pgsql_url => binary,
        domain_pgsql_database =>  binary
    }.


plugin_config(#{domain_pgsql_url:=Url}=Config, #{id:=SrvId}) ->
    #{
        domain_pgsql_url := Url
    } = Config,
    case nkpacket:parse_urls(pgsql, [tcp, tls], Url) of
        {ok, Conns} ->
            ServerId1 = <<(nklib_util:to_binary(SrvId))/binary, "_store_pgsql">>,
            ServerId2 = binary_to_atom(ServerId1, utf8),
            {ok, Config#{domain_pgsql_conns=>Conns, domain_pgsql_server_id=>ServerId2}, ServerId2};
        {error, Error} ->
            {error, Error}
    end;

plugin_config(Config, _Service) ->
    {ok, Config}.


plugin_start(#{domain_pgsql_conns:=Conns, domain_pgsql_server_id:=Name}=Config, #{id:=SrvId}) ->
    Db = maps:get(domain_pgsql_database, Config, <<"nkobjects">>),
    {ok, _} = nkservice_srv:start_proc(SrvId, Name, nkdomain_store_pgsql_server, [SrvId, Name, Db, Conns]),
    {ok, Config};

plugin_start(Config, _Service) ->
    {ok, Config}.


plugin_stop(Config, #{id:=_Id}) ->
    {ok, Config}.



%% ===================================================================
%% Store callbacks
%% ===================================================================




%% ===================================================================
%% Util
%% ===================================================================
