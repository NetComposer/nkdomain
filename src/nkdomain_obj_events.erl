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

-module(nkdomain_obj_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([event/2]).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================

%% Common object events are captured at Server:object_event/2, and,
%% if the function object_send_event/2 is defined in type's module,
%% it is called to send the event to the API
%% If not defined, or it doesn't manage the event, this module is called
%%
%% To send events, objects call nkdomain_obj_util:event/2, that sends an Erlang-level event
%% They are then captured as showed before.




%% @private
event({status, Status}, Session) when is_atom(Status); is_binary(Status) ->
    {event, updated_status, #{status=>Status}, Session};

event({status, {Status, Reason}}, Session) when is_atom(Status); is_binary(Status) ->
    #obj_session{srv_id=SrvId} = Session,
    {Code, Txt} = nkapi_util:api_error(SrvId, Reason),
    {event, updated_status, #{status=>Code, reason=>Txt}, Session};

event(saved, Session) ->
    {event, object_saved, #{}, Session};

event({updated, Update}, Session) ->
    {event, object_updated, #{update=>Update}, Session};

event(deleted, Session) ->
    {event, object_deleted, #{}, Session};

event({info, Info, Body}, Session) when is_map(Body) ->
    {event, object_info, Body#{info=>Info}, Session};

event({event, Event, Body}, Session) when is_map(Body) ->
    {event, Event, Body, Session};

event({enabled, Enabled}, Session) ->
    {event, object_enabled, #{enabled=>Enabled}, Session};

event({child_created, Type, ObjId}, Session) ->
    {event, object_child_created, #{type=>Type, obj_id=>ObjId}, Session};

event({child_loaded, Type, ObjId}, Session) ->
    {event, object_child_loaded, #{type=>Type, obj_id=>ObjId}, Session};

event({child_unloaded, Type, ObjId}, Session) ->
    {event, object_child_unloaded, #{type=>Type, obj_id=>ObjId}, Session};

event(_Event, Session) ->
    {ok, Session}.







