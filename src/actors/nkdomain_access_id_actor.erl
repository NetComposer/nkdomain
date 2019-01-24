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

%% @doc NkDomain Access Id Actor
%%
-module(nkdomain_access_id_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([config/0, parse/3]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").



%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_ACCESS_IDS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        camel => <<"AccessId">>,
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => []
    }.


%% @doc
parse(_SrvId, _Actor, _Params) ->
    {syntax, #{<<"data">> => map}}.



%% ===================================================================
%% Internal
%% ===================================================================

