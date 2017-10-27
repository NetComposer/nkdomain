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
-module(nkdomain_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([error/1, i18n/3]).
-export([admin_tree_categories/2, admin_tree_get_category/2, admin_event/3,
         admin_element_action/5, admin_get_data/3]).
-export([object_send_push/3]).
-export([object_init/1, object_terminate/2, object_stop/2,
         object_event/2, object_reg_event/4, object_sync_op/3, object_async_op/2,
         object_save/1, object_delete/1, object_link_down/2, object_enabled/2,
         object_handle_call/3, object_handle_cast/2, object_handle_info/2, object_conflict_detected/4]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN SRV Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.
-type state() :: #obj_state{}.



%% ===================================================================
%% Error
%% ===================================================================

%% @doc
error(Error) ->
    ?CALL_NKROOT(error, [Error]).


%% @doc 
i18n(SrvId, Key, Lang) ->
    case nklib_i18n:get(SrvId, Key, Lang) of
        <<>> when SrvId == ?NKROOT ->
            <<>>;
        <<>> ->
            nklib_i18n:get(?NKROOT, Key, Lang);
        Text ->
            Text
    end.


%% ===================================================================
%% Admin
%% ===================================================================


%% @private
admin_tree_categories(Data, Session) ->
    ?CALL_NKROOT(admin_tree_categories, [Data, Session]).


%% @doc
admin_tree_get_category(Category, Session) ->
    ?CALL_NKROOT(admin_tree_get_category, [Category, Session]).

%% @doc
admin_event(Event, Updates, Session) ->
    ?CALL_NKROOT(admin_event, [Event, Updates, Session]).

%% @doc
admin_element_action(ElementIdParts, Action, Value, Updates, Session) ->
    ?CALL_NKROOT(admin_element_action, [ElementIdParts, Action, Value, Updates, Session]).


%% @doc
admin_get_data(ElementId, Spec, Session) ->
    ?CALL_NKROOT(admin_get_data, [ElementId, Spec, Session]).


%% ===================================================================
%% Push
%% ===================================================================


%% @doc
-spec object_send_push(nkdomain_user_obj:push_device_id(),
                       nkdomain_user_obj:push_device(), nkdomain_user_obj:push_msg()) ->
    ok | {error, term()}.

object_send_push(_PushDeviceId, _PushDevice, _PushMsg) ->
    lager:notice("NkDOMAIN unimplemented push: ~p", [_PushMsg]),
    {error, not_implemented}.



%% ===================================================================
%% Object-process related callbacks
%% ===================================================================


%% @doc Called when a new session starts
-spec object_init(state()) ->
    {ok, state()} | {stop, Reason::term()}.

object_init(State) ->
    ?CALL_NKROOT(object_init, [State]).


%% @doc Called when the session stops
-spec object_terminate(Reason::term(), state()) ->
    {ok, state()}.

object_terminate(Reason, State) ->
    ?CALL_NKROOT(object_terminate, [Reason, State]).


%% @private
-spec object_stop(nkservice:error(), state()) ->
    {ok, state()} | continue().

object_stop(Reason, State) ->
    ?CALL_NKROOT(object_stop, [Reason, State]).


%%  @doc Called to send an event
-spec object_event(nkdomain_obj:event(), state()) ->
    {ok, state()} | continue().

object_event(Event, State) ->
    ?CALL_NKROOT(object_event, [Event, State]).


%% @doc Called when an event is sent, for each registered process to the session
%% The events are 'erlang' events (tuples usually)
-spec object_reg_event(nklib:link(), term(), nkdomain_obj:event(), state()) ->
    {ok, state()} | continue().

object_reg_event(Link, Data, Event, State) ->
    ?CALL_NKROOT(object_reg_event, [Link, Data, Event, State]).


%% @doc
-spec object_sync_op(term(), {pid(), reference()}, state()) ->
    {reply, Reply::term(), session} | {reply_and_save, Reply::term(), session} |
    {noreply, state()} | {noreply_and_save, session} |
    {stop, Reason::term(), Reply::term(), state()} |
    {stop, Reason::term(), state()} |
    continue().

object_sync_op(Op, From, State) ->
    ?CALL_NKROOT(object_sync_op, [Op, From, State]).


%% @doc
-spec object_async_op(term(), state()) ->
    {noreply, state()} | {noreply_and_save, session} |
    {stop, Reason::term(), state()} |
    continue().

object_async_op(Op, State) ->
    ?CALL_NKROOT(object_async_op, [Op, State]).


%% @doc Called to save the object to disk
-spec object_save(state()) ->
    {ok, state(), Meta::map()} | {error, term(), state()}.

object_save(State) ->
    ?CALL_NKROOT(object_save, [State]).


%% @doc Called to save the remove the object from disk
-spec object_delete(state()) ->
    {ok, state(), Meta::map()} | {error, term(), state()}.

object_delete(State) ->
    ?CALL_NKROOT(object_delete, [State]).


%% @doc Called when a linked process goes down
-spec object_link_down(event|{child, nkdomain:obj_id()}|{usage, nklib_links:link()}, state()) ->
    {ok, state()}.

object_link_down(Link, State) ->
    ?CALL_NKROOT(object_link_down, [Link, State]).


%% @doc Called when an object is enabled/disabled
-spec object_enabled(boolean(), state()) ->
    {ok, state()}.

object_enabled(Enabled, State) ->
    ?CALL_NKROOT(object_enabled, [Enabled, State]).


%% @doc
-spec object_handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()} |
    {stop, term(), term(), state()} | {stop, term(), state()} | continue.

object_handle_call(Msg, From, State) ->
    ?CALL_NKROOT(object_handle_call, [Msg, From, State]).


%% @doc
-spec object_handle_cast(term(), state()) ->
    {noreply, state()} | {stop, term(), state()} | continue.

object_handle_cast(Msg, State) ->
    ?CALL_NKROOT(object_handle_cast, [Msg, State]).


%% @doc
-spec object_handle_info(term(), state()) ->
    {noreply, state()} | {stop, term(), state()} | continue.

object_handle_info(Msg, State) ->
    ?CALL_NKROOT(object_handle_info, [Msg, State]).


%% @doc
-spec object_conflict_detected(nkservice:id(), nkdomain:type(), pid(), state()) ->
    {noreply, state()} | {stop, term(), state()} | continue.

object_conflict_detected(SrvId, Type, Pid, State) ->
    ?CALL_NKROOT(object_conflict_detected, [SrvId, Type, Pid, State]).






