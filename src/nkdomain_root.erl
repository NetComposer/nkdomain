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

%% @doc Root Domain
-module(nkdomain_root).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/0, start/0, stop/0, admin_create/0]).

-include("nkdomain.hrl").


%% ===================================================================
%% Private
%% ===================================================================

%% @doc
create() ->
    Obj = #{
        type => ?DOMAIN_DOMAIN,
        obj_id => <<"root">>,
        path => <<"/">>,
        domain_id => <<>>,
        description => <<"NetComposer">>,
        created_time => nkdomain_util:timestamp()
    },
    nkdomain_store_search:save_obj(root, Obj).



%% @doc Starts the root service
start() ->
    ListenIp = nkdomain_app:get(listen_ip),
    ListenPort = nklib_util:to_binary(nkdomain_app:get(listen_port)),
    ListenSecure = nkdomain_app:get(listen_secure),
    Http = case ListenSecure of true -> <<"https">>; false -> <<"http">> end,
    Ws = case ListenSecure of true -> <<"wss">>; false -> <<"ws">> end,
    BaseHttp = <<Http/binary, "://", ListenIp/binary, ":", ListenPort/binary>>,
    BaseWs = <<Ws/binary, "://", ListenIp/binary, ":", ListenPort/binary>>,
    Spec1 = #{
        plugins => [nkdomain, nkdomain_store_es, nkdomain_store_pgsql, nkadmin, nkchat],
        domain => <<"root">>,
        domain_default_store_id => "/file.stores/local",
        domain_elastic_url => nkdomain_app:get(elastic_url),


        % webserver_url => "https://127.0.0.1:1234",
        % webserver_path => "/tmp",
        admin_url => <<BaseHttp/binary, "/_admin">>,
        rest_url => BaseHttp,
        % API HTTP Requests will go to SrvId:api_server_http_auth
        api_server => <<BaseHttp/binary, "/_api, ", BaseWs/binary, "/_api/ws">>,
        debug => [
            %% {nkapi_client, #{nkpacket=>true}},
            %% nkapi_server,
            nkelastic,
            %%{nkelastic, full}
            {nkdomain_obj, #{types=>[<<"med.encounter">>]}}
        ]
    },
    User = nkdomain_app:get(elastic_user),
    Pass = nkdomain_app:get(elastic_pass),
    Spec2 = case is_binary(User) and is_binary(Pass) of
        true ->
            Spec1#{
                domain_elastic_user => User,
                domain_elastic_pass => Pass
            };
        false ->
            Spec1
    end,
    Spec3 = case nkdomain_app:get(store_pgsql) of
        undefined ->
            Spec2;
        PgSqlUrl ->
            Spec2#{nkdomain_store_pgsql => PgSqlUrl}
    end,
    case nkservice:start(root, Spec3) of
        {ok, _} ->
            lager:info("Root service started: ~p", [Spec2]);
        {error, Error} ->
            lager:error("Could not start root service: ~p (~p)", [Error, Spec2]),
            error(start_root_error)
    end.


%% @doc
stop() ->
    nkdomain_obj:unload_all(),
    timer:sleep(1000),
    nkservice:stop(root).


%% @doc
admin_create() ->
    Obj = #{
        obj_id => <<"admin">>,
        domain_id => <<"root">>,
        type => ?DOMAIN_USER,
        ?DOMAIN_USER => #{
            name => <<"Admin">>,
            surname => <<"User">>,
            password => "1234"
        }
    },
    nkdomain_obj_lib:make_and_create(root, <<"admin">>, Obj, #{}).










