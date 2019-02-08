%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain utilities
-module(nkdomain_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([add_api_plugin/1, get_api_spec/2]).
-export([add_rpc9_plugin/1, get_rpc9_spec/2]).


%% ===================================================================
%% Types
%% ===================================================================


%% @doc Adds a plugin to the RPC9 processing (see middle9_sup)
-spec add_rpc9_plugin(module()) -> ok | {error, term()}.
add_rpc9_plugin(Plugin) when is_atom(Plugin) ->
    ok = nklib_config:add(?MODULE, rpc9_plugins, Plugin).


%% @private
get_rpc9_spec(SrvId, #{url:=_}=Spec) ->
    Plugins = nklib_config:get(?MODULE, rpc9_plugins, []),
    Spec2 = Spec#{
        plugins => [nkdomain_rpc|Plugins],
        user_state => #{base_srv => SrvId}
    },
    lager:notice("NkDOMAIN Starting RPC9 listener (~p)", [Spec2]),
    nkrpc9_server:get_sup_spec(nkdomain_rpc_srv, Spec2).


%% @doc Adds a plugin to the API processing
-spec add_api_plugin(module()) -> ok | {error, term()}.
add_api_plugin(Plugin) when is_atom(Plugin) ->
    ok = nklib_config:add(?MODULE, api_plugins, Plugin).


%% @private
get_api_spec(SrvId, #{url:=_}=Spec) ->
    Plugins = nklib_config:get(?MODULE, api_plugins, []),
    Spec2 = Spec#{
        plugins => [nkdomain_api|Plugins],
        user_state => #{base_srv => SrvId}
    },
    lager:notice("NkDOMAIN Starting API listener (~p)", [Spec2]),
    nkrest:get_sup_spec(nkdomain_api_srv, Spec2).


