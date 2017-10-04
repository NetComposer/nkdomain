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
event(created, #?STATE{obj=#{domain_id:=DomainId}}=State) ->
    {event, {created, #{domain_id=>DomainId}}, State};

event(loaded, State) ->
    {event, loaded, State};

event({status, Status}, State) when is_atom(Status); is_binary(Status) ->
    {event, {updated_status, #{status=>Status}}, State};

event({status, {Status, Reason}}, State) when is_atom(Status); is_binary(Status) ->
    #?STATE{callback_srv_id=SrvId} = State,
    {Code, Txt} = nkservice_util:error(SrvId, Reason),
    {event, {updated_status, #{status=>Code, reason=>Txt}}, State};

event(saved, State) ->
    {event, saved, State};

event({updated, Update}, State) ->
    {event, {updated, #{update=>Update}}, State};

event(deleted, State) ->
    {event, deleted, State};

event({info, Info, Body}, State) when is_map(Body) ->
    {event, {object_info, Body#{info=>Info}}, State};

event({event, Event, Body}, State) when is_map(Body) ->
    {event, {Event, Body}, State};

event({enabled, Enabled}, State) ->
    {event, {object_enabled, #{enabled=>Enabled}}, State};

event({child_created, Type, ObjId}, State) ->
    {event, {child_created, #{type=>Type, obj_id=>ObjId}}, State};

event({child_loaded, Type, ObjId, _Pid}, State) ->
    {event, {child_loaded, #{type=>Type, obj_id=>ObjId}}, State};

event({child_unloaded, Type, ObjId}, State) ->
    {event, {child_unloaded, #{type=>Type, obj_id=>ObjId}}, State};

event({unloaded, Reason}, State) ->
    #?STATE{callback_srv_id=SrvId} = State,
    {Code, Txt} = nkservice_util:error(SrvId, Reason),
    {event, {unloaded, #{code=>Code, reason=>Txt}}, State};

event(_Event, State) ->
    {ok, State}.







