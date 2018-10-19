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

%% @doc NkDomain Event Actor
-module(nkdomain_event_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(nkdomain_actor).

-export([config/0]).
-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Plugin: "++Txt, Args)).

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Actor behaviour
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_EVENTS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        camel => ?KIND_CORE_EVENT,
        verbs => [delete, deletecollection, get, list, watch],
        short_names => [ev],
        activable => false,
        filter_fields => [
            <<"reason">>,
            <<"count">>,
            <<"firstTimestamp">>,
            <<"lastTimestamp">>,
            <<"involvedObject.uid">>,
            <<"involvedObject.domain">>,
            <<"involvedObject.apiVersion">>,
            <<"involvedObject.kind">>,
            <<"involvedObject.name">>,
            <<"involvedObject.subtype">>,
            <<"involvedObject.resourceVersion">>
        ],
        sort_fields => [
            <<"reason">>,
            <<"count">>,
            <<"firstTimestamp">>,
            <<"lastTimestamp">>,
            <<"involvedObject.domain">>,
            <<"involvedObject.apiVersion">>,
            <<"involvedObject.kind">>,
            <<"involvedObject.name">>,
            <<"involvedObject.subtype">>
        ],
        field_type => #{
            <<"count">> => integer
        }
    }.


