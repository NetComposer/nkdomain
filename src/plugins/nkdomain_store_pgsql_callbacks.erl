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


plugin_config(#{domain_pgsql_url:=Url}=Config, _Service) ->
    #{
        domain_pgsql_url := Url
    } = Config,
    case nkpacket:parse_urls(pgsql, [tcp, tls], Url) of
        {ok, Conns} ->
            {ok, Config#{domain_pgsql_conns=>Conns}};
        {error, Error} ->
            {error, Error}
    end;

plugin_config(Config, _Service) ->
    {ok, Config}.


plugin_start(#{domain_pgsql_conns:=Conns}=Config, #{id:=Id}) ->
    Db = maps:get(domain_pgsql_database, Config, <<"nkobjects">>),
    case Conns of
        [{[{undefined, tcp, Ip, Port}|_], Opts}|_] ->
            ConnOpts = [
                {database, Db},
                {host, nklib_util:to_host(Ip)},
                {port, Port},
                {user, maps:get(user, Opts, <<"root">>)},
                {password, maps:get(password, Opts, <<"">>)},
                %{connect_timeout, ConnectTimeout},
                {as_binary, true}
            ],
            PoolOpts = [
                {name, {local, pgsql_pool}},
                {worker_module, nkdomain_store_pgsql_worker},
                {size, 5},
                {max_overflow, 10}
            ],
            Spec = poolboy:child_spec(pgsql_pool, PoolOpts, ConnOpts),
            {ok, _} = nkservice_srv:start_proc(Id, Spec),
            lager:error("NKLOG CONN1"),
            {ok, Config};
        _ ->
            {ok, Config}
    end;

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
