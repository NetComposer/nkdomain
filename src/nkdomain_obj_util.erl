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


%% @doc Basic Obj utilities
-module(nkdomain_obj_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([event/2, status/2]).

-include("nkdomain.hrl").



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
event(Event, #obj_session{link_events=Links}=Session) ->
    {ok, #obj_session{}=Session2} = do_event(Links, Event, Session),
    Session2.


%% @private
do_event([], Event, #obj_session{srv_id=SrvId}=Session) ->
    {ok, #obj_session{}} = SrvId:object_event(Event, Session);

do_event([Link|Rest], Event, #obj_session{srv_id=SrvId}=Session) ->
    {ok, Session2} = SrvId:object_reg_event(Link, Event, Session),
    do_event(Rest, Event,  Session2).


%% @doc
status(Status, #obj_session{status=Status}=Session) ->
    Session;

status(Status, Session) ->
    Session2 = Session#obj_session{status=Status},
    event({status, Status}, Session2).

