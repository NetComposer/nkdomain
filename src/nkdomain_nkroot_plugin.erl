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
-module(nkdomain_nkroot_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_syntax/0, plugin_config/2, plugin_listen/2]).
-export([init/1, syntax/0]).


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN ROOT Plugin: "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Plugin callbacks
%% ===================================================================


%% @private
plugin_deps() ->
    [nkdomain].


%% @private
plugin_syntax() ->
    #{
        nkdomain => syntax()
    }.


%% @private
plugin_config(#{nkdomain:=_}=Config, Service) ->
    config(Config, Service);

plugin_config(Config, _Service) ->
    {ok, Config}.


%% @private
plugin_listen(_Config, #{id:=_SrvId}) ->
    [].
%%    case Config of
%%        #{graphql_url:=Url} ->
%%            nkdomain_graphql:get_listen(SrvId, Url, Config);
%%        _ ->
%%            []
%%    end.



%% ===================================================================
%% Config
%% ===================================================================



%% @private
%% Called from nkdomain_nkroot_callbacks:plugin_syntax/0
syntax() ->
    #{
        server_vsn => binary,
        start_nkroot => boolean,
        wait_db_secs => {integer, 1, 3600},
        wait_db_retries => {integer, 0, 100},
        load_schema => boolean,
        listen_ip => host,
        listen_port => {integer, 1, 65535},
        listen_path => basepath,
        listen_secure => boolean,
        start_api_server => boolean,
        start_admin => boolean,
        start_rest => boolean,
        db_clusters => {list, map},
        db_store => binary,
        event_store => binary,
        log_store => binary,
        default_store_id => binary,
        default_file_store => binary,
        default_mail_provider => binary,
        default_transcoder => binary,
        default_image_processor => binary,
        start_services => {list, binary},
        '__defaults' => #{
            server_vsn => <<"v0.0.3">>,
            start_nkroot => false,
            wait_db_secs => 0,
            wait_db_retries => 0,
            load_schema => false,
            listen_ip => <<"127.0.0.1">>,
            listen_port => 9301,
            listen_path => <<"/">>,
            listen_secure => false,
            start_api_server => true,
            start_admin => true,
            start_rest => true
        },
        '__mandatory' => [db_store]
    }.


%% @private
%% Called from nkdomain_nkroot_callbacks:plugin_config/2
%% If start_api_server is true, we will listen for http and ws APIs (/_api and /_api/ws)
%% If start_rest is true, we listen on / and can user nkservice_rest_http (in nkdomain_nkroot_callbacks)
%% If start_admin is true, we listen on /_admin for webserver and /admin/_ws for API
config(Config, _Service) ->
    #{nkdomain:=Env} = Config,
    #{
        listen_ip := Host,
        listen_port := Port,
        listen_path := Path,
        listen_secure := Secure,
        start_api_server := ApiServer,
        start_admin := Admin,
        start_rest := Rest
    } = Env,
    BinPort = nklib_util:to_binary(Port),
    Http = case Secure of true -> <<"https">>; false -> <<"http">> end,
    Ws = case Secure of true -> <<"wss">>; false -> <<"ws">> end,
    BaseHttp = <<Http/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    BaseWs = <<Ws/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    Services = maps:get(start_services, Env, []),
    Config1 = Config#{start_services => Services},
    Config2 = case ApiServer of
        true ->
            ApiObj = #{
                id => <<"nkroot">>,
                url => <<BaseHttp/binary, "/_api, ", BaseWs/binary, "/_api/ws">>
            },
            nkservice_util:add_config_obj(nkapi_server, ApiObj, Config1);
        false ->
            Config1
    end,
    Config3 = case Admin of
        true ->
            Config2#{
                nkadmin => #{
                    webserver_url => <<BaseHttp/binary, "/_admin">>
                    %api_url => <<BaseWs/binary, "/_admin/_api/_ws">>
                }
            };
        false ->
            Config2
    end,
    Rest1 = case Rest of
        true ->
            [BaseHttp];
        _ ->
            []
    end,
    Config4 = case nklib_util:bjoin(Rest1, <<", ">>) of
        <<>> ->
            Config3;
        RestUrl ->
            RestObj = #{
                id => <<"nkroot">>,
                url => RestUrl
            },
            nkservice_util:add_config_obj(nkservice_rest, RestObj, Config3)
    end,
    %% Plugin nkdomain_store_es should have inserted already its
    %% configuration
    DbStore = case Config4 of
        #{nkdomain_db_store:=DbStore0} ->
            DbStore0;
        _ ->
            error(missing_nkdomain_db_store)
    end,
    Cache = #nkdomain_config_cache{
        db_store = DbStore,
        file_store = maps:get(default_file_store, Env, <<>>),
        email_provider = maps:get(default_file_store, Env, <<>>),
        transcoder_server = maps:get(default_transcoder, Env, <<>>),
        image_processor = maps:get(default_image_processor, Env, <<>>)
    },
    Config5 = add_graphql(Config4),
    {ok, Config5, Cache}.



%% @private
%% Called from nkdomain_nkroot_callbacks:service_init/2
init(State) ->
    case ?CALL_NKROOT(object_db_init, [State]) of
        {ok, State2} ->
            nkdomain_node:nkroot_started(),
            {ok, State2};
        {error, Error} ->
            lager:error("NkDOMAIN: could not start db store: ~p", [Error]),
            {stop, Error}
    end.



%% ===================================================================
%% Internal - File Stores
%% ===================================================================


add_graphql(Config) ->
    RestObj = #{
        id => <<"nkroot_graphql">>,
        url => <<"http://all:9500">>
    },
    nkservice_util:add_config_obj(nkservice_rest, RestObj, Config).
