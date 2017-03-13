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
-export([start_root/0]).
-export([get/1, put/2, del/1]).

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
        user_timeout => {integer, 1, none},
        alias_timeout => {integer, 1, none},
        token_timeout => {integer, 1, none},
        role_proxy_timeout => {integer, 1, none},
        user_password_pbkdf2_iters => {integer, 1, none},
        syntax_callback_mod => atom,
        syntax_callback_fun => atom,
        '__defaults' => #{
            start_root => false,
            elastic_url => <<"http://127.0.0.1:9200/">>,
            user_timeout => 5000,
            alias_timeout => 5000,
            token_timeout => 60 * 60 * 1000,
            role_proxy_timeout => 10000,
            user_password_pbkdf2_iters => 1,
            syntax_callback_mod => nkservice_util,
            syntax_callback_fun => get_syntax
        }
    },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            SyntaxMod = get(syntax_callback_mod),
            SyntaxFun = get(syntax_callback_fun),
            code:ensure_loaded(SyntaxMod),
            put(syntax_callback, {SyntaxMod, SyntaxFun}),
            {ok, Pid} = nkdomain_sup:start_link(),
            %% ok = riak_core_ring_events:add_guarded_handler(nkdomain_ring_handler, []),
            {ok, Vsn} = application:get_key(nkdomain, vsn),
            lager:info("NkDOMAIN v~s has started.", [Vsn]),
            register_types(),
            case get(start_root) of
                true ->
                    spawn_link(
                        fun() ->
                            timer:sleep(5000),
                            start_root()
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
    ok = nkdomain_types:register_type(nkdomain_domain),
    ok = nkdomain_types:register_type(nkdomain_user).


%% @doc Starts the root service
start_root() ->
    Spec1 = #{
        plugins => [nkelastic, nkdomain, nkchat, nkdomain_store_es],
        domain_elastic_url => get(elastic_url),
        debug => [
            {nkdomain_obj, all}
        ]
    },
    User = get(elastic_user),
    Pass = get(elastic_pass),
    Spec2 = case is_binary(User) and is_binary(Pass) of
        true ->
            Spec1#{
                domain_elastic_user => User,
                domain_elastic_pass => Pass
            };
        false ->
            Spec1
    end,
    case nkservice:start(root, Spec2) of
        {ok, _} ->
            lager:info("Root service started"),
            case nkdomain_obj:load(root, <<"root">>, #{}) of
                {ok, nkdomain_domain, <<"root">>, _Pid} ->
                    ok;
                {error, Error} ->
                    lager:error("Could not load ROOT domain: ~p", [Error])
            end;
        {error, Error} ->
            lager:error("Could not start root service: ~p", [Error]),
            error(start_root_error)
    end.

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


