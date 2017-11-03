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
-export([object_admin_info/1]).
-export([object_syntax/1, object_create/5, object_parse/2, object_update/1]).
-export([object_check_active/2, object_do_expired/1]).
-export([object_send_push/3]).
-export([object_init/1, object_terminate/2, object_stop/2,
         object_event/2, object_reg_event/4, object_sync_op/3, object_async_op/2,
         object_save/1, object_delete/1, object_link_down/2, object_enabled/2, object_next_status_timer/1,
         object_handle_call/3, object_handle_cast/2, object_handle_info/2, object_conflict_detected/3]).
-export([object_db_init/1, object_db_read/1, object_db_save/1, object_db_delete/1]).
-export([object_db_find_obj/1, object_db_search/1, object_db_search_alias/1,
         object_db_search_types/2, object_db_search_all_types/2,
         object_db_search_childs/2, object_db_search_all_childs/2, object_db_search_agg_field/4,
         object_db_delete_all_childs/2, object_db_clean/0]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN SRV Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: nkdomain:obj_id().
-type type() :: nkdomain:type().
-type path() :: nkdomain:path().
-type continue() :: continue | {continue, list()}.
-type state() :: #obj_state{}.



%% ===================================================================
%% Error
%% ===================================================================


%% @doc
error({body_too_large, Size, Max})      -> {"Body too large (size is ~p, max is ~p)", [Size, Max]};
error({could_not_load_parent, Id})      -> {"Object could not load parent '~s'", [Id]};
error({could_not_load_domain, Id})      -> {"Object could not load domain '~s'", [Id]};
error(domain_unknown)                   -> "Unknown domain";
error({domain_unknown, D})              -> {"Unknown domain '~s'", [D]};
error(domain_invalid)                   -> "Invalid domain";
error(domains_name_cannot_change)       -> "ObjName cannot be updated for domains";
error({email_duplicated, E})            -> {"Duplicated email '~s'", [E]};
error(element_action_unknown)           -> "Unknown element action";
error({file_not_found, F})              -> {"File '~s' not found", [F]};
error(invalid_content_type)             -> "Invalid Content-Type";
error({invalid_name, N})                -> {"Invalid name '~s'", [N]};
error(invalid_object_id)                -> "Invalid object id";
error(invalid_object_type)              -> "Invalid object type";
error(invalid_sessionn)                 -> "Invalid session";
error({invalid_type, T})                -> {"Invalid type '~s'", [T]};
error(token_invalid)                    -> "Invalid token";
error(token_invalid_ttl)                -> "Invalid token TTL";
error(member_already_present)           -> "Member is already present";
error(member_not_found)                 -> "Member not found";
error(member_invalid)                   -> "Invalid member";
error(multiple_ids)                     -> "Multiple matching ids";
error(missing_auth_header)              -> "Missing authentication header";
error({module_failed, Module})          -> {"Module '~s' failed", [Module]};
error(object_access_not_allowed)        -> "Object access is not allowed";
error(object_already_exists)            -> "Object already exists";
error(object_clean_process)             -> "Object cleaned (process stopped)";
error(object_clean_expire)              -> "Object cleaned (expired)";
error(object_consumed)                  -> "Object is consumed";
error({object_consumed, R})             -> {"Object is consumed: ~s", [R]};
error(object_deleted) 		            -> "Object removed";
error(object_expired) 		            -> "Object expired";
error(object_has_childs) 		        -> "Object has childs";
error({object_load_error, Error}) 		-> {"Object load error: '~p'", [Error]};
error(object_is_already_loaded)         -> "Object is already loaded";
error(object_is_disabled) 		        -> "Object is disabled";
error(object_is_stopped) 		        -> "Object is stopped";
error(object_not_found) 		        -> "Object not found";
error(object_not_started) 		        -> "Object is not started";
error(object_path_invalid)              -> "Invalid object path";
error({object_path_invalid, P})         -> {"Invalid object path '~s'", [P]};
error(object_parent_conflict) 	        -> "Object has conflicting parent";
error(object_stopped) 		            -> "Object stopped";
error(operation_invalid) 	            -> "Invalid operation";
error(operation_token_invalid) 	        -> "Operation token is invalid";
error(parent_is_disabled) 		        -> "Parent is disabled";
error(parent_not_found) 		        -> "Parent not found";
error(parent_stopped) 		            -> "Parent stopped";
error(parse_error)   		            -> "Object parse error";
error(service_down)                     -> "Service is down";
error(session_already_present)          -> "Session is already present";
error(session_not_found)                -> "Session not found";
error(session_is_disabled)              -> "Session is disabled";
error(session_type_unsupported)         -> "Session type not supported";
error({srv_id_invalid, S})              -> {"Invalid service ~s", [S]};
error(sso_not_available)                -> "SSO is not available";
error(status_not_defined)               -> "Status is not defined";
error(store_id_invalid)                 -> "Invalid Store Id";
error(store_id_missing)                 -> "Missing Store Id";
error(token_down)                       -> "Token process is down";
error(url_unknown)      		        -> "Unknown url";
error(user_is_disabled) 		        -> "User is disabled";
error(user_unknown)                     -> "Unknown user";
error(db_not_defined)                   -> "Object database not defined";
error(_)   		                        -> continue.


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
    nkdomain_admin_tree:categories(Data, Session).


%% @doc
admin_tree_get_category(Category, Session) ->
    nkdomain_admin_tree:get_category(Category, Session).


%% @doc
admin_event(#nkevent{class = ?DOMAIN_EVENT_CLASS}=Event, Updates, Session) ->
    nkdomain_admin_tree:event(Event, Updates, Session);

admin_event(_Event, _Updates, _Session) ->
    continue.


%% @doc
admin_element_action(ElementIdParts, Action, Value, Updates, Session) ->
    nkdomain_admin_tree:element_action(ElementIdParts, Action, Value, Updates, Session).


%% @doc
admin_get_data(ElementIdParts, Spec, Session) ->
    nkdomain_admin_util:get_data(ElementIdParts, Spec, Session).


%% ===================================================================
%% Object related callbacks
%% ===================================================================


%% @doc Object syntax
-spec object_syntax(load|update|create) ->
    {nklib_syntax:syntax(), nklib_syntax:parse_opts()}.

object_syntax(load) ->
    Syntax = #{
        vsn => binary,
        obj_id => binary,
        type => binary,
        path => binary,
        obj_name => binary,
        domain_id => binary,
        parent_id => binary,
        srv_id => binary,
        subtype => {list, binary},
        created_by => binary,
        created_time => integer,
        updated_by => binary,
        updated_time => integer,
        enabled => boolean,
        active => boolean,                    % Must be loaded to exist
        expires_time => integer,
        destroyed => boolean,
        destroyed_time => integer,
        destroyed_code => binary,
        destroyed_reason => binary,
        name => binary,
        description => binary,
        tags => {list, binary},
        aliases => {list, binary},
        icon_id => binary,
        next_status_time => integer,
        '_schema_vsn' => any,
        '_store_vsn' => any,
        '__mandatory' => [type, obj_id, domain_id, path, created_time]
    },
    {Syntax, #{}};

object_syntax(update) ->
    Syntax = #{
        type => ignore,             % Do not count as unknown is updates
        srv_id => ignore,           % "
        enabled => boolean,
        name => binary,
        description => binary,
        tags => {list, binary},
        aliases => {list, binary},
        icon_id => binary
    },
    {Syntax, #{}};

object_syntax(create) ->
    % For creation of objects, no mandatory is checked, since the creation function
    % will check all mandatory fields
    {Base, Opts} = object_syntax(load),
    {Base#{'__mandatory':=[]}, Opts}.



%% @doc Creates a new object (called from API)
-spec object_create(nkservice:id(), nkdomain:id(), nkdomain:type(), nkdomain:id(), map()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

object_create(SrvId, DomainId, Type, UserId, Obj) ->
    case nkdomain_lib:get_module(Type) of
        undefined ->
            {error, unknown_type};
        Module ->
            Obj2 = Obj#{
                type => Type,
                created_by => UserId,
                domain_id => DomainId,
                srv_id => SrvId
            },
            case erlang:function_exported(Module, object_create, 1) of
                true ->
                    % If the objects has its own object creation function,
                    % parse it so that it can be useful
                    case ?CALL_NKROOT(object_parse, [create, Obj2]) of
                        {ok, Obj3, _} ->
                            Module:object_create(Obj3);
                        {error, Error} ->
                            {error, Error}
                    end;
                false ->
                    nkdomain_obj_make:create(Obj2)
            end
    end.



%% @doc Called to parse an object's syntax from a map
%% create: called when creating an object, all fields are allowed but no mandatory,
%%         since the creation functions will fill in all mandatory fields
%% load:   used when loading an object from disk
%% update: used when updating an object, only fields allowed to be updated
%%         must be included
-spec object_parse(create|load|update, map()) ->
    {ok, nkdomain:obj(), Unknown::[binary()]} | {error, term()}.

object_parse(Mode, Map) ->

    Type = case Map of
        #{<<"type">>:=Type0} -> Type0;
        #{type:=Type0} -> Type0;
        _ -> <<>>
    end,
    case nkdomain_lib:get_module(Type) of
        undefined ->
            {error, {invalid_type, Type}};
        Module ->
            case Module:object_parse(Mode, Map) of
                {ok, Obj2, UnknownFields} ->
                    {ok, Obj2, UnknownFields};
                {error, Error} ->
                    {error, Error};
                {type_obj, TypeObj, UnknownFields1} ->
                    {BaseSyn, Opts} = ?CALL_NKROOT(object_syntax, [Mode]),
                    case nklib_syntax:parse(Map, BaseSyn#{Type=>ignore}, Opts) of
                        {ok, Obj, UnknownFields2} ->
                            {ok, Obj#{Type=>TypeObj}, UnknownFields1++UnknownFields2};
                        {error, Error} ->
                            {error, Error}
                    end;
                Syntax when is_map(Syntax) ->
                    Syntax2 = case Mode==create orelse Mode==load of
                        true ->
                            Defaults1 = maps:get('_defaults', Syntax, #{}),
                            Defaults2 = Defaults1#{vsn => <<"1">>},
                            Syntax#{vsn => binary, '_defaults' => Defaults2};
                        false ->
                            Syntax
                    end,
                    {BaseSyn, Opts} = ?CALL_NKROOT(object_syntax, [Mode]),
                    nklib_syntax:parse(Map, BaseSyn#{Type=>Syntax2}, Opts);
                any ->
                    erlang:error(any)
            end
    end.


%% @doc
-spec object_admin_info(nkdomain:type()) ->
    nkdomain_admin:object_admin_info().

object_admin_info(Type) ->
    Module = nkdomain_lib:get_module(Type),
    case erlang:function_exported(Module, object_admin_info, 0) of
        true ->
            Module:object_admin_info();
        false ->
            #{}
    end.




%% @doc
%% Called after creation or modification of object to add fields, etc.
-spec object_update(nkdomain:obj()) ->
    ok | continue | {ok, nkdomain:obj()} | {error, term()}.

object_update(#{type:=Type}=Obj) ->
    case nkdomain_lib:type_apply(Type, object_udpate, [Obj]) of
        not_exported ->
            {ok, Obj};
        {ok, Obj2} ->
            {ok, Obj2};
        {error, Error} ->
            {error, Error}
    end.



%% @doc Called if an active object is detected on storage
%% If 'true' is returned, the object is ok
%% If 'false' is returned, it only means that the object has been processed
-spec object_check_active(type(), obj_id()) ->
    boolean().

object_check_active(Type, ObjId) ->
    case nkdomain_lib:type_apply(Type, object_check_active, [ObjId]) of
        not_exported ->
            true;
        true ->
            true;
        false ->
            false;
        force_load ->
            case nkdomain_lib:find_loaded(ObjId) of
                #obj_id_ext{} ->
                    true;
                _ ->
                    spawn_link(
                        fun() ->
                            case nkdomain_lib:load(ObjId) of
                                #obj_id_ext{} ->
                                    ok;
                                {error, Error} ->
                                    lager:notice("could not load stalle active object ~s (~s): ~p",
                                                 [ObjId, Type, Error])
                            end
                        end),
                    false
            end;
        delete_if_not_loaded ->
            case nkdomain_lib:find_loaded(ObjId) of
                #obj_id_ext{} ->
                    true;
                _ ->
                    spawn_link(
                        fun() ->
                            case ?CALL_NKROOT(object_db_delete, [ObjId]) of
                                {ok, _Meta} ->
                                    ?LLOG(notice, "removed stalle active object ~s (~s)", [ObjId, Type]);
                                {error, Error} ->
                                    ?LLOG(warning, "could not remove stalle active object ~s (~s): ~p",
                                          [ObjId, Type, Error])
                            end
                        end),
                    false
            end
    end.


%% @doc Called if an object is over its expired time
-spec object_do_expired(obj_id()) ->
    any().

object_do_expired(ObjId) ->
    lager:notice("NkDOMAIN: removing expired object ~s", [ObjId]),
    ok.










%% @doc
-spec object_send_push(nkdomain_user_obj:push_device_id(),
                       nkdomain_user_obj:push_device(), nkdomain_user_obj:push_msg()) ->
    ok | {error, term()}.

object_send_push(_PushDeviceId, _PushDevice, _PushMsg) ->
    lager:notice("NkDOMAIN unimplemented push: ~p", [_PushMsg]),
    {error, not_implemented}.



%% ===================================================================
%% Object-in-process related callbacks
%% ===================================================================


%% @doc Called when a new session starts
-spec object_init(state()) ->
    {ok, state()} | {stop, Reason::term()}.

object_init(State) ->
    call_module(object_init, [], State).


%% @doc Called when the session stops
-spec object_terminate(Reason::term(), state()) ->
    {ok, state()}.

object_terminate(Reason, State) ->
    call_module(object_terminate, [Reason], State).


%% @private
-spec object_stop(nkservice:error(), state()) ->
    {ok, state()} | continue().

object_stop(Reason, State) ->
    call_module(object_stop, [Reason], State).


%%  @doc Called to send an event
-spec object_event(nkdomain_obj:event(), state()) ->
    {ok, state()} | continue().

object_event(Event, State) ->
    % The object module can use this callback to detect core events or its own events
    {ok, State2} = call_module(object_event, [Event], State),
    % Use this callback to generate the right external Event
    case call_module(object_send_event, [Event], State2) of
        {ok, State3} ->
            EvSpec = nkdomain_obj_events:event(Event, State3),
            nkdomain_obj_util:send_event(EvSpec);
        EvSpec ->
            nkdomain_obj_util:send_event(EvSpec)
    end.


%% @doc Called when an event is sent, for each registered process to the session
%% The events are 'erlang' events (tuples usually)
-spec object_reg_event(nklib:link(), term(), nkdomain_obj:event(), state()) ->
    {ok, state()} | continue().

object_reg_event(Link, Data, Event, State) ->
    call_module(object_reg_event, [Link, Data, Event], State).


%% @doc
-spec object_sync_op(term(), {pid(), reference()}, state()) ->
    {reply, Reply::term(), session} | {reply_and_save, Reply::term(), session} |
    {noreply, state()} | {noreply_and_save, session} |
    {stop, Reason::term(), Reply::term(), state()} |
    {stop, Reason::term(), state()} |
    continue().

object_sync_op(Op, From, State) ->
    case call_module(object_sync_op, [Op, From], State) of
        {ok, State2} ->
            % It is probably not exported
            {continue, [Op, From, State2]};
        Other ->
            Other
    end.


%% @doc
-spec object_async_op(term(), state()) ->
    {noreply, state()} | {noreply_and_save, session} |
    {stop, Reason::term(), state()} |
    continue().

object_async_op(Op, State) ->
    case call_module(object_async_op, [Op], State) of
        {ok, State2} ->
            % It is probably not exported
            {continue, [Op, State2]};
        Other ->
            Other
    end.


%% @doc Called to save the object to disk
-spec object_save(state()) ->
    {ok, state(), Meta::map()} | {error, term(), state()}.

object_save(State) ->
    case call_module(object_save, [], State) of
        {ok, #obj_state{obj=Obj2}=State2} ->
            Obj3 = Obj2#{<<"vsn">> => <<"1">>},
            case ?CALL_NKROOT(object_db_save, [Obj3]) of
                {ok, Meta} ->
                    {ok, State2, Meta};
                {error, Error} ->
                    {error, Error, State2}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc Called to save the remove the object from disk
-spec object_delete(state()) ->
    {ok, state(), Meta::map()} | {error, term(), state()}.

object_delete(#obj_state{id=#obj_id_ext{obj_id=ObjId}}=State) ->
    case call_module(object_delete, [], State) of
        {ok, State2} ->
            case ?CALL_NKROOT(object_db_delete, [ObjId]) of
                {ok, Meta} ->
                    {ok, State2, Meta};
                {error, Error} ->
                    {error, Error, State2}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc Called when a linked process goes down
-spec object_link_down(event|{child, nkdomain:obj_id()}|{usage, nklib_links:link()}, state()) ->
    {ok, state()}.

object_link_down(Link, State) ->
    call_module(object_link_down, [Link], State).


%% @doc Called when an object is enabled/disabled
-spec object_enabled(boolean(), state()) ->
    {ok, state()}.

object_enabled(Enabled, State) ->
    call_module(object_enabled, [Enabled], State).


%% @doc Called when the timer in next_status_time is fired
-spec object_next_status_timer(state()) ->
    {ok, state()}.

object_next_status_timer(State) ->
    call_module(object_next_status_timer, [], State).


%% @doc
-spec object_handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()} |
    {stop, term(), term(), state()} | {stop, term(), state()} | continue.

object_handle_call(Msg, From, State) ->
    case call_module(object_handle_call, [Msg, From], State) of
        {ok, State2} ->
            lager:error("Module nkdomain_obj received unexpected call: ~p", [Msg]),
            {noreply, State2};
        Other ->
            Other
    end.


%% @doc
-spec object_handle_cast(term(), state()) ->
    {noreply, state()} | {stop, term(), state()} | continue.

object_handle_cast(Msg, State) ->
    case call_module(object_handle_cast, [Msg], State) of
        {ok, State2} ->
            lager:error("Module nkdomain_obj received unexpected cast: ~p", [Msg]),
            {noreply, State2};
        Other ->
            Other
    end.


%% @doc
-spec object_handle_info(term(), state()) ->
    {noreply, state()} | {stop, term(), state()} | continue.

object_handle_info(Msg, State) ->
    case call_module(object_handle_info, [Msg], State) of
        {ok, State2} ->
            lager:warning("Module nkdomain_obj received unexpected info: ~p", [Msg]),
            {noreply, State2};
        Other ->
            Other
    end.


%% @doc
-spec object_conflict_detected(nkdomain:type(), pid(), state()) ->
    {noreply, state()} | {stop, term(), state()} | continue.

object_conflict_detected(_Type, _Pid, _State) ->
    erlang:error(conflict_detected).




%% ===================================================================
%% DB Management
%%
%% Plugins are note expected to process management of database, but
%% delegate to nkroot
%% ===================================================================


%% @doc Called to initialize the database
-spec object_db_init(nkservice:service()) ->
    {ok, nkservice:service()}| {error, term()}.

object_db_init(State) ->
    ?CALL_NKROOT(object_db_init, [State]).


%% @doc Reads and parses object from database, using ObjId
-spec object_db_read(obj_id()) ->
    {ok, nkdomain:obj(), Meta::map()} | {error, term()}.

object_db_read(ObjId) ->
    ?CALL_NKROOT(object_db_read, [ObjId]).


%% @doc Saves an object to database
-spec object_db_save(nkdomain:obj()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_save(Obj) ->
    ?CALL_NKROOT(object_db_save, [Obj]).


%% @doc Deletes an object from database
-spec object_db_delete(nkdomain:obj_id()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_delete(ObjId) ->
    ?CALL_NKROOT(object_db_delete, [ObjId]).


%% @doc Finds an object from its ID or Path
-spec object_db_find_obj(nkdomain:id()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

object_db_find_obj(ObjId) ->
    ?CALL_NKROOT(object_db_find_obj, [ObjId]).


%% @doc
-spec object_db_search_types(obj_id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), integer()}]} | {error, term()}.

object_db_search_types(ObjId, Spec) ->
    ?CALL_NKROOT(object_db_search_types, [ObjId, Spec]).


%% @doc
-spec object_db_search_all_types(path(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{type(), integer()}]} | {error, term()}.

object_db_search_all_types(ObjId, Spec) ->
    ?CALL_NKROOT(object_db_search_all_types, [ObjId, Spec]).


%% @doc
-spec object_db_search_childs(obj_id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_childs(ObjId, Spec) ->
    ?CALL_NKROOT(object_db_search_childs, [ObjId, Spec]).


%% @doc
-spec object_db_search_all_childs(path(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_all_childs(Path, Spec) ->
    ?CALL_NKROOT(object_db_search_all_childs, [Path, Spec]).


%% @doc
-spec object_db_search_alias(nkdomain:alias()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_alias(Alias) ->
    ?CALL_NKROOT(object_db_search_alias, [Alias]).


%% @doc
-spec object_db_search(nkdomain:search_spec()) ->
    {ok, Total::integer(), Objs::[map()], map(), Meta::map()} |
    {error, term()}.

object_db_search(Spec) ->
    ?CALL_NKROOT(object_db_search, [Spec]).


%% @doc
-spec object_db_search_agg_field(nkdomain:id(), binary(),
                                 nkdomain:search_spec(), SubChilds::boolean()) ->
                                    {ok, Total::integer(), [{nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_agg_field(Id, Field, Spec, SubChilds) ->
    ?CALL_NKROOT(object_db_search_agg_field, [Id, Field, Spec, SubChilds]).


%% @doc Must stop loaded objects
-spec object_db_delete_all_childs(path(), nkdomain:search_spec()) ->
    {ok, Total::integer()} | {error, term()}.

object_db_delete_all_childs(Path, Spec) ->
    ?CALL_NKROOT(object_db_delete_all_childs, [Path, Spec]).


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_check_active/3 for each 'active' object found
-spec object_db_clean() ->
    ok | {error, term()}.

object_db_clean() ->
    ?CALL_NKROOT(object_db_clean, []).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
call_module(Fun, Args, #obj_state{module=Module}=State) ->
    case erlang:function_exported(Module, Fun, length(Args)+1) of
        true ->
            case apply(Module, Fun, Args++[State]) of
                continue ->
                    {ok, State};
                Other ->
                    Other
            end;
        false ->
            {ok, State}
    end.
