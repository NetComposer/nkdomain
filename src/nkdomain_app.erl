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
    % nkdist_util:ensure_dir(),
    case nklib_util:ensure_all_started(?APP, Type) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.


%% @private OTP standard start callback
start(_Type, _Args) ->
    Syntax = nkdomain_nkroot:syntax(),
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkdomain_sup:start_link(),
            {ok, Vsn} = application:get_key(nkdomain, vsn),
            lager:info("NkDOMAIN v~s has started.", [Vsn]),
            nkdomain_i18n:reload(),
            register_types(),
            case get(start_nkroot) of
                true ->
                    spawn_link(
                        fun() ->
                            timer:sleep(1000),
                            case nkdomain_nkroot:start() of
                                {ok, _} ->
                                    lager:info("NkDOMAIN root started"),
                                    %% TODO HACK to start C4 provisionally
                                    ok = sipstorm_c4_util:start();
                                {error, Error} ->
                                    lager:error("NkDOMAN root could not start: ~p", [Error]),
                                    error(service_start_error)
                            end
                        end);
                false ->
                    lager:warning("NkDOMAIN root domain not started")
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
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_domain_obj),
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_user_obj),
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_session_obj),
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_config_obj),
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_token_obj),

    ok = nkdomain_all_types:register(?NKSRV, nkdomain_mail_obj),
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_mail_provider_obj),

    ok = nkdomain_all_types:register(?NKSRV, nkdomain_file_store_obj),
    ok = nkdomain_all_types:register(?NKSRV, nkdomain_file_obj),

    ok = nkdomain_all_types:register(?NKSRV, nkadmin_session_obj).



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


