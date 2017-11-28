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

%% @doc Elasticsearch Events plugin
-module(nkdomain_event_es_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).


-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).




%% ===================================================================
%% Plugin Callbacks
%% ===================================================================

plugin_deps() ->
    [nkdomain_store_es].


%% Other plugins can also parse db_clusters
plugin_syntax() ->
    #{
        nkdomain_event_es => #{
            db_store => binary,
            '__mandatory' => [db_store]
        }
    }.


plugin_config(#{nkdomain_event_es:=NkDomain}=Config, _Service) ->
    #{db_store:=DbStore} = NkDomain,
    {ok, Config, {db_store, DbStore}};

plugin_config(_Config, _Service) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================
