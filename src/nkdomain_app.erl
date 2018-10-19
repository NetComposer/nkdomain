%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc Domain Application Module
-module(nkdomain_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(application).

-export([start/0, start/1, start/2, stop/1]).
-export([start_service/0, stop_service/0, reload_service/0]).
-export([get/1, get/2, put/2, del/1]).
-export([plugins/0]).

-include_lib("nkservice/include/nkservice.hrl").
-include("nkdomain.hrl").

-define(APP, nkdomain).

-compile({no_auto_import,[get/1, put/2]}).

%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts NkDOMAIN stand alone.
-spec start() -> 
    ok | {error, Reason::term()}.

start() ->
    start(permanent).


%% @doc Starts NkDOMAIN stand alone.
-spec start(permanent|transient|temporary) -> 
    ok | {error, Reason::term()}.

start(Type) ->
    % nkdist_util:ensure_dir(),
    case nklib_util:ensure_all_started(?APP, Type) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.


%% @private OTP standard start callback
%% Last application must call maybe_start_nkroot
start(_Type, _Args) ->
    Syntax = #{
        baseSrv => binary,
        adminPass => binary,
        apiUrl => binary,
        makeGraphqlSchema => boolean,
        graphiqlUrl => binary,
        dbDriver => {atom, [cockroachdb, elasticsearch]},
        dbUrl => binary,
        dbDatabase => binary,
        serviceDbHeartbeat => boolean,
        serviceDbMaxHeartbeatTime => pos_integer,  % secs
        '__mandatory' => [baseSrv, dbDriver, dbUrl, dbDatabase]
    },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            BaseSrvId = case get(baseSrv) of
                <<>> ->
                    ?ROOT_SRV;
                D0 ->
                    D1 = list_to_binary([D0, $., nklib_util:to_binary(?ROOT_SRV)]),
                    binary_to_atom(D1, utf8)
            end,
            put(baseSrv, BaseSrvId),
            {ok, Pid} = nkdomain_sup:start_link(),
            ok = nkservice_util:register_package(?PACKAGE_CLASS_DOMAIN, nkdomain),
            {ok, Vsn} = application:get_key(nkdomain, vsn),
            lager:info("NkDOMAIN v~s has started.", [Vsn]),
            start_service(),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.



%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @doc Starts the base service
%% In nkdomain_plugin we will create the actors
start_service() ->
    BaseSrvId = get(baseSrv),
    cockroachdb = get(dbDriver),
    Spec = #{
        plugins => plugins(),
        debug_actors => [all, core, "core:user"],
        domain => <<"root">>,
        packages => [
            #{
                id => db,
                class => 'PgSQL',
                config => #{
                    targets => [
                        #{
                            url => get(dbUrl),
                            pool => 16
                        }
                    ],
                    actorPersistence => true,
                    database => get(dbDatabase),
                    debug => true
                }
            },
            #{
                id => ?DOMAIN_PKG_ID_FILE,
                class => 'File'
            },
%%            #{
%%                id => ?DOMAIN_PKG_ID_NOTIFY,
%%                class => 'Notify'
%%            },
            #{
                id => ?DOMAIN_PKG_ID,
                class => 'Domains',
                config => #{
                    apiUrl => get(apiUrl),
                    apiUrlOpts => #{},
                    apiDebug => [erlang, http, nkpacket],
                    % apiDebug => [erlang, http, nkpacket],
                    % Options used by graphql plugin
                    makeGraphqlSchema => get(makeGraphqlSchema, true),
                    graphiqlUrl => get(graphiqlUrl, undefined)
                }
            }
        ]
    },
    ok = nkservice:start(BaseSrvId, Spec).


stop_service() ->
    BaseSrvId = get(baseSrv),
    nkservice:stop(BaseSrvId).


reload_service() ->
    BaseSrvId = get(baseSrv),
    case nkservice:update(BaseSrvId, #{}) of
        ok ->
            case ?CALL_SRV(BaseSrvId, service) of
                #{packages:=#{?DOMAIN_PKG_ID:=#{config:=#{makeGraphqlSchema:=true}}}} ->
                    lager:info("loading GraphQL schema"),
                    nkservice_graphql:load_schema(BaseSrvId);
                _ ->
                    ok
            end;
        {error, Error} ->
            {error, Error}
    end.


plugins() ->
    [
        nknotify_sms_aws_sns,
        nknotify_sms_twilio,
        nknotify_sms_netelip,
        nknotify_push_gorush,
        nknotify_push_aws_sns,
        nknotify_push_twilio,
        nknotify_fax_twilio,
        nknotify_mail_smtp
    ].




%% @doc gets a configuration value
get(Key) ->
    get(Key, undefined).


%% @doc gets a configuration value
get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).


%% @doc updates a configuration value
put(Key, Value) ->
    nklib_config:put(?APP, Key, Value).


%% @doc updates a configuration value
del(Key) ->
    nklib_config:del(?APP, Key).

