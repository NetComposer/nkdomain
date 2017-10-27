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


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Callbacks: "++Txt, Args)).

%%-include("nkdomain.hrl").
%%-include_lib("nkapi/include/nkapi.hrl").
%%-include_lib("nkevent/include/nkevent.hrl").
%%-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.


%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [].


%% @private
plugin_syntax() ->
    #{
        nkdomain => nkdomain_nkroot:syntax()
    }.


%% @private
plugin_config(#{nkdomain:=_}=Config, Service) ->
    nkdomain_nkroot:config(Config, Service);

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

