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

%% @doc Domain Application Module
-module(nkdomain_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(application).

-export([start/0, start/1, start/2, stop/1]).
-export([get/1, put/2, del/1]).
-export([register_types/0]).

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
    nkdist_util:ensure_dir(),
    case nklib_util:ensure_all_started(?APP, Type) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.


%% @private OTP standard start callback
start(_Type, _Args) ->
    Syntax = #{
        start_root => boolean,
        elastic_url => binary,
        elastic_user => binary,
        elastic_pass => binary,
        api_server => binary,

        role_proxy_timeout => {integer, 1, none},
        user_password_pbkdf2_iters => {integer, 1, none},
        '__defaults' => #{
            start_root => false,
            elastic_url => <<"http://127.0.0.1:9200/">>,
            api_server => <<"ws:all:9202/api/ws, http:all:9202/api">>,

            role_proxy_timeout => 10000,
            user_password_pbkdf2_iters => 1
        }
    },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkdomain_sup:start_link(),
            %% ok = riak_core_ring_events:add_guarded_handler(nkdomain_ring_handler, []),
            {ok, Vsn} = application:get_key(nkdomain, vsn),
            lager:info("NkDOMAIN v~s has started.", [Vsn]),
            register_types(),
            case get(start_root) of
                true ->
                    spawn_link(
                        fun() ->
                            timer:sleep(2000),
                            nkdomain_root:start()
                        end);
                false ->
                    lager:warning("Root domain not started")
            end,
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.



%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @doc Register our types
register_types() ->
    ok = nkdomain_types:register(nkdomain_domain_obj),
    ok = nkdomain_types:register(nkdomain_user_obj),
    ok = nkdomain_types:register(nkdomain_session_obj),
    ok = nkdomain_types:register(nkdomain_config_obj),
    ok = nkdomain_types:register(nkdomain_token_obj),
    ok = nkdomain_types:register(nkmail_mail_obj).


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


