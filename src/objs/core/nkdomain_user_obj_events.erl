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

%% @doc User Object

-module(nkdomain_user_obj_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([event/2]).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @private
event({login, SessId, Meta}, State) ->
    {event, {login, #{session_id=>SessId, meta=>Meta}}, State};

event({session_status_updated, Type, SessId, Status}, State) ->
    {event, {session_status_updated, #{session_type=>Type, session_id=>SessId, status=>Status}}, State};

event({session_started, Type, SessId}, State) ->
    {event, {session_started, #{session_type=>Type, session_id=>SessId}}, State};

event({session_stopped, Type, SessId}, State) ->
    {event, {session_stopped, #{session_type=>Type, session_id=>SessId}}, State};

event(_Event, State) ->
    {ok, State}.

