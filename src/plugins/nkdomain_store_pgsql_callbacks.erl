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
        nkdomain_store_pgsql =>
            {list, #{
                id => binary,
                url => binary,
                '__mandatory' => [url]
            }}
    }.


plugin_config(#{nkdomain_store_pgsql:=List}=Config, #{id:=SrvId}) ->
    case parse_stores(List, #{}) of
        {ok, ConnMap} ->
            ServerId = nklib_util:to_atom(<<(nklib_util:to_binary(SrvId))/binary, "_nkdomain_store_pgsql">>),
            {ok, Config#{nkdomain_store_pgsql_stores=>{ServerId, ConnMap}}, ServerId};
        {error, Error} ->
            {error, Error}
    end;

plugin_config(Config, _Service) ->
    {ok, Config}.


plugin_start(#{nkdomain_store_pgsql_stores:={ServerId, ConnMap}}=Config, #{id:=SrvId}) ->
    {ok, _} = nkservice_srv:start_proc(SrvId, ServerId, nkdomain_store_pgsql_server, [SrvId, ServerId, ConnMap]),
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

parse_stores([], Acc) ->
    {ok, Acc};

parse_stores([#{url:=Url}=Map|Rest], Acc) ->
    case nkpacket:parse_urls(pgsql, [tcp, tls], Url) of
        {ok, Conns} ->
            Id = maps:get(id, Map, <<"main">>),
            case maps:is_key(Id, Acc) of
                false ->
                    parse_stores(Rest, Acc#{Id=>Conns});
                true ->
                    {error, duplicated_id}
            end;
        {error, Error} ->
            {error, Error}
    end.
