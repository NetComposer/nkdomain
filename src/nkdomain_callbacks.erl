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
-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).
-export([service_init/2, service_handle_cast/2, service_handle_info/2]).
-export([error/1, service_api_syntax/2, service_api_allow/2, service_api_cmd/2, api_server_http_auth/2,
         api_server_reg_down/3]).
-export([nkservice_rest_http/5]).
-export([admin_tree_categories/2, admin_tree_get_category/2, admin_event/3, admin_element_action/5, admin_get_data/3]).
-export([object_store_read_id/3, object_store_read_path/3, object_store_save/1]).

-export([object_mapping/0, object_syntax/1, object_parse/3, object_unparse/1]).
-export([object_load/2, object_get_session/1, object_save/1, object_delete/1, object_archive/1]).
-export([object_admin_info/1, object_get_counter/2]).
-export([object_check_active/3, object_do_expired/2]).
-export([object_init/1, object_terminate/2, object_start/1, object_stop/2,
         object_event/2, object_reg_event/3, object_sync_op/3, object_async_op/2,
         object_handle_call/3, object_handle_cast/2, object_handle_info/2]).
-export([object_store_reload_types/1, object_store_read_raw/2, object_store_save_raw/3,
         object_store_delete_raw/2,
         object_store_archive_find/2, object_store_archive_save_raw/3]).
-export([object_store_find_obj/2,
         %% object_store_find_obj_id/2, object_store_find_path/2,
         object_store_find_types/3, object_store_find_all_types/3,
         object_store_find_childs/3, object_store_find_all_childs/3,
         object_store_find_alias/2, object_store_delete_all_childs/3,
         object_store_find/2, object_store_clean/1]).
-export([nkmail_get_provider/2, nkfile_get_store/2]).


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.
-type obj_id() :: nkdomain:obj_id().
-type type() :: nkdomain:type().
-type path() :: nkdomain:path().



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error({body_too_large, Size, Max})      -> {"Body too large (size is ~p, max is ~p)", [Size, Max]};
error({could_not_load_parent, Id})      -> {"Object could not load parent '~s'", [Id]};
error(domain_unknown)                   -> "Unknown domain";
error(invalid_content_type)             -> "Invalid Content-Type";
error({invalid_name, N})                -> {"Invalid name '~s'", [N]};
error(invalid_object_id)                -> "Invalid object id";
error(invalid_object_type)              -> "Invalid object type";
error(invalid_object_path)              -> "Invalid object path";
error({invalid_object_path, P})         -> {"Invalid object path '~s'", [P]};
error(invalid_sessionn)                 -> "Invalid session";
error({invalid_type, T})                -> {"Invalid type '~s'", [T]};
error(invalid_token)                    -> "Invalid token";
error(invalid_token_ttl)                -> "Invalid token TTL";
error(member_already_present)           -> "Member is already present";
error(member_not_found)                 -> "Member not found";
error(missing_auth_header)              -> "Missing authentication header";
error({module_failed, Module})          -> {"Module '~s' failed", [Module]};
error(object_already_exists)            -> "Object already exists";
error(object_clean_process)             -> "Object cleaned (process stopped)";
error(object_clean_expire)              -> "Object cleaned (expired)";
error(object_deleted) 		            -> "Object removed";
error(object_expired) 		            -> "Object expired";
error(object_has_childs) 		        -> "Object has childs";
error({object_load_error, Error}) 		-> {"Object load error: '~p'", [Error]};
error(object_is_already_loaded)         -> "Object is already loaded";
error(object_is_disabled) 		        -> "Object is disabled";
error(object_is_stopped) 		        -> "Object is stopped";
error(object_not_found) 		        -> "Object not found";
error(object_not_started) 		        -> "Object is not started";
error(object_parent_conflict) 	        -> "Object has conflicting parent";
error(object_stopped) 		            -> "Object stopped";
error(parent_not_found) 		        -> "Parent not found";
error(parent_stopped) 		            -> "Parent stopped";
error(referred_not_found) 		        -> "Referred object not found";
error(session_already_present)          -> "Session is already present";
error(session_not_found)                -> "Session not found";
error(session_is_disabled)              -> "Session is disabled";
error(user_is_disabled) 		        -> "User is disabled";
error(user_unknown)                     -> "Unknown user";
error(_)   		                        -> continue.




%% ===================================================================
%% Admin
%% ===================================================================


%% @private
admin_tree_categories(Data, State) ->
    nkdomain_admin_tree:categories(Data, State).


%% @doc
admin_tree_get_category(Category, State) ->
    nkdomain_admin_tree:get_category(Category, State).


%% @doc
admin_event(#nkevent{class = ?DOMAIN_EVENT_CLASS}=Event, Updates, State) ->
    nkdomain_admin_tree:event(Event, Updates, State);

admin_event(_Event, _Updates, _State) ->
    continue.


%% @doc
admin_element_action(ElementId, Action, Value, Updates, State) ->
    nkdomain_admin_tree:element_action(ElementId, Action, Value, Updates, State).


%% @doc
admin_get_data(ElementId, Spec, State) ->
    nkdomain_admin_detail:get_data(ElementId, Spec, State).


%% ===================================================================
%% REST
%% ===================================================================


%% @doc
nkservice_rest_http(SrvId, get, [<<"_file">>, FileId], Req, State) ->
    case nkdomain_file_obj:http_get(SrvId, FileId, Req) of
        {ok, CT, Bin} ->
            {http, 200, [{<<"Content-Type">>, CT}], Bin, State};
        {error, Error} ->
            nkservice_rest_http:reply_json({error, Error}, Req, State)
    end;

nkservice_rest_http(SrvId, post, File, Req, State) ->
    case lists:reverse(File) of
        [<<"_file">>|Rest] ->
            Domain = nklib_util:bjoin(lists:reverse(Rest), <<"/">>),
            case nkdomain_file_obj:http_post(SrvId, Domain, Req) of
                {ok, Reply, _Pid} ->
                    nkservice_rest_http:reply_json({ok, Reply}, Req, State);
                {error, Error} ->
                    nkservice_rest_http:reply_json({error, Error}, Req, State)
            end;
        _ ->
            continue
    end;

nkservice_rest_http(_SrvId, _Method, _Path, _Req, _State) ->
    continue.



%% ===================================================================
%% Offered Object Callbacks
%% ===================================================================

-type session() :: nkdomain_obj:session().


%% @doc In-store base mapping
-spec object_mapping() -> map().

object_mapping() ->
    #{
        obj_id => #{type => keyword},
        type => #{type => keyword},
        path => #{type => keyword},
        parent_id => #{type => keyword},
        referred_id => #{type => keyword},
        subtype => #{type => keyword},
        created_by => #{type => keyword},
        created_time => #{type => date},
        updated_by => #{type => keyword},
        updated_time => #{type => date},
        enabled => #{type => boolean},
        active => #{type => boolean},
        expires_time => #{type => date},
        destroyed => #{type => boolean},
        destroyed_time => #{type => date},
        destroyed_code => #{type => keyword},
        destroyed_reason => #{type => keyword},
        name => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        description => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        tags => #{type => keyword},
        aliases => #{type => keyword},
        icon_id => #{type => keyword},
        icon_url => #{type => keyword},
        icon_content_type => #{type => keyword}

    }.


%% @doc Object syntax
-spec object_syntax(load|update) -> nklib_syntax:syntax().

object_syntax(load) ->
    #{
        obj_id => binary,
        type => binary,
        path => binary,
        parent_id => binary,
        referred_id => binary,
        subtype => {list, binary},
        created_by => binary,
        created_time => integer,
        updated_by => binary,
        updated_time => integer,
        parent_id => binary,
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
        icon_content_type => binary,
        '_store_vsn' => any,
        '__mandatory' => [type, obj_id, parent_id, path, created_time]
    };

object_syntax(update) ->
    #{
        type => ignore,             % Do not count as unknown is updates
        enabled => boolean,
        name => binary,
        description => binary,
        tags => {list, binary},
        aliases => {list, binary},
        icon_id => binary,
        icon_content_type => binary
    }.

%% @doc Reads an object from database, using its ObjId
%% Must return a parsed object
%% Opts: {field, [atom()|binary()]
object_store_read_id(_SrvId, _ObjId, _Meta) ->
    {error, not_implemented}.


%% @doc Reads an object from database, using its Path
%% Must return a parsed object
%% Opts: {field, [atom()|binary()]
object_store_read_path(_SrvId, _Path, _Meta) ->
    {error, not_implemented}.


%% @doc Saves an object to backend
%%

object_store_save(_Session) ->
    {error, not_implemented}.


%% @doc Called to get and parse an object
-spec object_load(nkservice:id(), obj_id()) ->
    {ok, nkdomain:obj()} | {error, term()}.

object_load(SrvId, ObjId) ->
    case SrvId:object_store_read_raw(SrvId, ObjId) of
        {ok, Map} ->
            case SrvId:object_parse(SrvId, load, Map) of
                {ok, Obj, UnknownFields} ->
                    {ok, Obj, UnknownFields};
                {error, Error} ->
                    ?LLOG(warning, "error parsing loaded object ~s: ~p\n~p", [ObjId, Error, Map]),
                    {error, parse_error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec object_get_session(session()) ->
    {ok, session()}.

object_get_session(Session) ->
    {ok, _Session2} = call_module(object_restore, [], Session).


%% @doc Called to save the object to disk
-spec object_save(session()) ->
    {ok, session()} | {error, term(), session()}.

object_save(#?NKOBJ{is_dirty=false}=Session) ->
    {ok, Session};

object_save(#?NKOBJ{srv_id=SrvId, obj_id=ObjId}=Session) ->
    {ok, Session2} = call_module(object_restore, [], Session),
    case call_module(object_save, [], Session2) of
        {ok, Session3} ->
            Map = SrvId:object_unparse(Session3),
            case nkdomain_store:save(SrvId, ObjId, Map) of
                {ok, _Vsn} ->
                    {ok, Session3#?NKOBJ{is_dirty=false}};
                {error, Error} ->
                    {error, Error, Session3}
            end;
        {error, Error} ->
            {error, Error, Session2}
    end.


%% @doc Called to save the remove the object from disk
-spec object_delete(session()) ->
    {ok, session()} | {error, term(), session()}.

object_delete(#?NKOBJ{srv_id=SrvId, obj_id=ObjId}=Session) ->
    case call_module(object_delete, [], Session) of
        {ok, Session2} ->
            case nkdomain_store:delete(SrvId, ObjId) of
                ok ->
                    {ok, Session2};
                {error, Error} ->
                    {error, Error, Session2}
            end;
        {error, Error} ->
            {error, Error, Session}
    end.



%% @doc Called to save the archived version to disk
-spec object_archive(session()) ->
    {ok, session()} | {error, term(), session()}.

object_archive(#?NKOBJ{srv_id=SrvId, obj_id=ObjId, obj=Obj}=Session) ->
    {ok, Session2} = call_module(object_restore, [], Session),
    case call_module(object_archive, [], Session2) of
        {ok, Session3} ->
            Map = SrvId:object_unparse(Session3#?NKOBJ{obj=Obj}),
            case nkdomain_store:archive(SrvId, ObjId, Map) of
                ok ->
                    {ok, Session3};
                {error, Error} ->
                    {error, Error, Session3}
            end;
        {error, Error} ->
            {error, Error, Session2}
    end.


%% @doc Called to parse an object's syntax
-spec object_parse(nkservice:id(), load|update, map()) ->
    {ok, nkdomain:obj(), Unknown::[binary()]} | {error, term()}.

object_parse(SrvId, Mode, Map) ->
    Type = case Map of
        #{<<"type">>:=Type0} -> Type0;
        #{type:=Type0} -> Type0;
        _ -> <<>>
    end,
    case nkdomain_all_types:get_module(Type) of
        undefined ->
            {error, {invalid_type, Type}};
        Module ->
            case Module:object_parse(SrvId, Mode, Map) of
                {ok, Obj2, UnknownFields} ->
                    {ok, Obj2, UnknownFields};
                {error, Error} ->
                    {error, Error};
                {type_obj, TypeObj, UnknownFields1} ->
                    BaseSyn = SrvId:object_syntax(Mode),
                    case nklib_syntax:parse(Map, BaseSyn#{Type=>ignore}) of
                        {ok, Obj, UnknownFields2} ->
                            {ok, Obj#{Type=>TypeObj}, UnknownFields1++UnknownFields2};
                        {error, Error} ->
                            {error, Error}
                    end;
                Syntax when Syntax==any; is_map(Syntax) ->
                    BaseSyn = SrvId:object_syntax(Mode),
                    nklib_syntax:parse(Map, BaseSyn#{Type=>Syntax})
            end
    end.



%% @doc Called to serialize an object to disk
-spec object_unparse(session()) ->
    map().

object_unparse(#?NKOBJ{srv_id=SrvId, type=Type, module=Module, obj=Obj}) ->
    BaseKeys = maps:keys(SrvId:object_mapping()),
    BaseMap1 = maps:with(BaseKeys, Obj),
    BaseMap2 = case BaseMap1 of
        #{pid:=Pid} ->
            BaseMap1#{pid:=base64:encode(term_to_binary(Pid))};
        _ ->
            BaseMap1
    end,
    ModData = maps:get(Type, Obj, #{}),
    case Module:object_mapping() of
        disabled ->
            BaseMap2#{Type => ModData};
        Map when is_map(Map) ->
            ModKeys = maps:keys(Map),
            ModMap = maps:with(ModKeys, ModData),
            BaseMap2#{Type => ModMap}
    end.


%% @doc Called if an active object is detected on storage
%% If 'true' is returned, the object is ok
%% If 'false' is returned, it only means that the object has been processed
-spec object_check_active(nkservice:id(), type(), obj_id()) ->
    boolean().

object_check_active(SrvId, Type, ObjId) ->
    case nkdomain_obj_util:call_type(object_check_active, [SrvId, ObjId], Type) of
        ok ->
            true;
        Other ->
            Other
    end.


%% @doc Called if an object is over its expired time
-spec object_do_expired(nkservice:id(), obj_id()) ->
    any().

object_do_expired(SrvId, ObjId) ->
    lager:notice("NkDOMAIN: removing expired object ~s", [ObjId]),
    nkdomain:archive(SrvId, ObjId, object_clean_expire).



%% @doc
-spec object_admin_info(nkdomain:type()) ->
    nkdomain_admin:object_admin_info().

object_admin_info(Type) ->
    Module = nkdomain_all_types:get_module(Type),
    case erlang:function_exported(Module, object_admin_info, 0) of
        true ->
            Module:object_admin_info();
        false ->
            #{}
    end.


%% @doc
-spec object_get_counter(nkdomain:type(), nkdomain:path()) ->
    {ok, integer()}.

object_get_counter(Type, DomainPath) ->
    Module = nkdomain_all_types:get_module(Type),
    nkdomain_type:get_global_counters(Module, DomainPath).


%% ===================================================================
%% Object-process related callbacks
%% ===================================================================


%% @doc Called when a new session starts
-spec object_init(session()) ->
    {ok, session()} | {stop, Reason::term()}.

object_init(Session) ->
    call_module(object_init, [], Session).


%% @doc Called when the session stops
-spec object_terminate(Reason::term(), session()) ->
    {ok, session()}.

object_terminate(Reason, Session) ->
    call_module(object_terminate, [Reason], Session).


%% @private
-spec object_start(session()) ->
    {ok, session()} | continue().

object_start(Session) ->
    call_module(object_start, [], Session).


%% @private
-spec object_stop(nkservice:error(), session()) ->
    {ok, session()} | continue().

object_stop(Reason, Session) ->
    call_module(object_stop, [Reason], Session).


%%  @doc Called when an event is sent
-spec object_event(nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_event(Event, Session) ->
    % The object module can use this callback to detect core events or its own events
    {ok, Session2} = call_module(object_event, [Event], Session),
    % Use this callback to generate the right external Event
    case call_module(object_send_event, [Event], Session) of
        {ok, Session2} ->
            case nkdomain_obj_events:event(Event, Session2) of
                {ok, Session3} ->
                    {ok, Session3};
                {event, Type, Body, Session3} ->
                    nkdomain_obj_lib:send_event(Type, Body, Session3);
                {event, Type, ObjId, Body, Session3} ->
                    nkdomain_obj_lib:send_event(Type, ObjId, Body, Session3);
                {event, Type, ObjId, Path, Body, Session3} ->
                    nkdomain_obj_lib:send_event(Type, ObjId, Path, Body, Session3)
            end;
        {event, Type, Body, Session2} ->
            nkdomain_obj_lib:send_event(Type, Body, Session2);
        {event, Type, ObjId, Body, Session3} ->
            nkdomain_obj_lib:send_event(Type, ObjId, Body, Session3);
        {event, Type, ObjId, Path, Body, Session2} ->
            nkdomain_obj_lib:send_event(Type, ObjId, Path, Body, Session2);
        {ignore, Session2} ->
            {ok, Session2}
    end.


%% @doc Called when an event is sent, for each registered process to the session
-spec object_reg_event(nklib:link(), nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_reg_event(Link, Event, Session) ->
    call_module(object_reg_event, [Link, Event], Session).


%% @doc
-spec object_sync_op(term(), {pid(), reference()}, session()) ->
    {reply, Reply::term(), session} | {reply_and_save, Reply::term(), session} |
    {noreply, session()} | {noreply_and_save, session} |
    {stop, Reason::term(), Reply::term(), session()} |
    {stop, Reason::term(), session()} |
    continue().

object_sync_op(Op, From, Session) ->
    case call_module(object_sync_op, [Op, From], Session) of
        {ok, _Session} ->
            continue;
        Other ->
            Other
    end.


%% @doc
-spec object_async_op(term(), session()) ->
    {noreply, session()} | {noreply_and_save, session} |
    {stop, Reason::term(), session()} |
    continue().

object_async_op(Op, Session) ->
    case call_module(object_async_op, [Op], Session) of
        {ok, _Session} ->
            continue;
        Other ->
            Other
    end.


%% @doc
-spec object_handle_call(term(), {pid(), term()}, session()) ->
    {reply, term(), session()} | {noreply, session()} |
    {stop, term(), term(), session()} | {stop, term(), session()} | continue.

object_handle_call(Msg, From, Session) ->
    case call_module(object_handle_call, [Msg, From], Session) of
        {ok, Session2} ->
            lager:error("Module nkdomain_obj received unexpected call: ~p", [Msg]),
            {noreply, Session2};
        Other ->
            Other
    end.


%% @doc
-spec object_handle_cast(term(), session()) ->
    {noreply, session()} | {stop, term(), session()} | continue.

object_handle_cast(Msg, Session) ->
    case call_module(object_handle_cast, [Msg], Session) of
        {ok, Session2} ->
            lager:error("Module nkdomain_obj received unexpected cast: ~p", [Msg]),
            {noreply, Session2};
        Other ->
            Other
    end.


%% @doc
-spec object_handle_info(term(), session()) ->
    {noreply, session()} | {stop, term(), session()} | continue.

object_handle_info(Msg, Session) ->
    case call_module(object_handle_info, [Msg], Session) of
        {ok, Session2} ->
            lager:warning("Module nkdomain_obj received unexpected info: ~p", [Msg]),
            {noreply, Session2};
        Other ->
            Other
    end.



%% ===================================================================
%% Offered Object Store Callbacks
%% ===================================================================


%% @doc
-spec object_store_reload_types(nkservice:id()) ->
    ok | {error, term()}.

object_store_reload_types(SrvId) ->
    call_parent_store(SrvId, object_store_reload_types, []).


%% @doc
-spec object_store_read_raw(nkservice:id(), obj_id()) ->
    {ok, [map()]} | {error, term()}.

object_store_read_raw(SrvId, ObjId) ->
    call_parent_store(SrvId, object_store_read_raw, [ObjId]).


%% @doc
-spec object_store_save_raw(nkservice:id(), obj_id(), map()) ->
    {ok, Vsn::term()} | {error, term()}.

object_store_save_raw(SrvId, ObjId, Map) ->
    call_parent_store(SrvId, object_store_save_raw, [ObjId, Map]).


%% @doc
-spec object_store_delete_raw(nkservice:id(), obj_id()) ->
    ok | {error, object_has_chids|term()}.

object_store_delete_raw(SrvId, ObjId) ->
    call_parent_store(SrvId, object_store_delete_raw, [ObjId]).


%% @doc
-spec object_store_find_obj(nkservice:id(), nkdomain:id()) ->
    {ok, type(), obj_id(), path()} | {error, term()}.

object_store_find_obj(SrvId, Id) ->
    call_parent_store(SrvId, object_store_find_obj, [Id]).


%% @doc
-spec object_store_find_types(nkservice:id(), obj_id(), nkdomain_store:search_spec()) ->
    {ok, Total::integer(), [{type(), integer()}]} | {error, term()}.

object_store_find_types(SrvId, ObjId, Spec) ->
    call_parent_store(SrvId, object_store_find_types, [ObjId, Spec]).


%% @doc
-spec object_store_find_all_types(nkservice:id(), path(), nkdomain_store:search_spec()) ->
    {ok, Total::integer(), [{type(), integer()}]} | {error, term()}.

object_store_find_all_types(SrvId, ObjId, Spec) ->
    call_parent_store(SrvId, object_store_find_all_types, [ObjId, Spec]).


%% @doc
-spec object_store_find_childs(nkservice:id(), obj_id(), nkdomain_store:search_spec()) ->
    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
    {error, term()}.

object_store_find_childs(SrvId, ObjId, Spec) ->
    call_parent_store(SrvId, object_store_find_childs, [ObjId, Spec]).


%% @doc
-spec object_store_find_all_childs(nkservice:id(), path(), nkdomain_store:search_spec()) ->
    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
    {error, term()}.

object_store_find_all_childs(SrvId, Path, Spec) ->
    call_parent_store(SrvId, object_store_find_all_childs, [Path, Spec]).


%% @doc
-spec object_store_find_alias(nkservice:id(), nkdomain:alias()) ->
    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
    {error, term()}.

object_store_find_alias(SrvId, Alias) ->
    call_parent_store(SrvId, object_store_find_alias, [Alias]).


%% @doc
-spec object_store_find(nkservice:id(), nkdomain_store:search_spec()) ->
    {ok, Total::integer(), [map()], map(), map()} |
    {error, term()}.

object_store_find(SrvId, Spec) ->
    call_parent_store(SrvId, object_store_find, [Spec]).


%% @doc
-spec object_store_archive_find(nkservice:id(), nkdomain_store:search_spec()) ->
    {ok, integer(), [map()]} | {error, term()}.

object_store_archive_find(SrvId, Spec) ->
    call_parent_store(SrvId, object_store_archive_find, [Spec]).


%% @doc
-spec object_store_archive_save_raw(nkservice:id(), obj_id(), map()) ->
    ok | {error, term()}.

object_store_archive_save_raw(SrvId, ObjId, Map) ->
    call_parent_store(SrvId, object_store_archive_save_raw, [ObjId, Map]).


%% @doc Must stop loaded objects
-spec object_store_delete_all_childs(nkservice:id(), path(), nkdomain_store:search_spec()) ->
    {ok, Total::integer()} | {error, term()}.

object_store_delete_all_childs(SrvId, Path, Spec) ->
    call_parent_store(SrvId, object_store_delete_all_childs, [Path, Spec]).


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_check_active/3 for each 'active' object found
-spec object_store_clean(nkservice:id()) ->
    ok.

object_store_clean(SrvId) ->
    call_parent_store(SrvId, object_store_clean, []).


%% ===================================================================
%% API Server
%% ===================================================================



%% @doc
service_api_syntax(Syntax, #nkreq{cmd = <<"objects/", Rest/binary>>}=Req) ->
    case binary:split(Rest, <<"/">>) of
        [] ->
            continue;
        [Type, Cmd] ->
            case nkdomain_all_types:get_module(Type) of
                undefined ->
                    continue;
                Module ->
                    Syntax2 = case erlang:function_exported(Module, object_api_syntax, 2) of
                        true ->
                            apply(Module, object_api_syntax, [Cmd, Syntax]);
                        false ->
                            nkdomain_obj_syntax:syntax(Cmd, Type, Syntax)
                    end,
                    {continue, [Syntax2, Req#nkreq{req_state={Type, Module, Cmd}}]}
            end
    end;

service_api_syntax(_Syntax, _Req) ->
    continue.


%% @doc
service_api_allow(#nkreq{cmd = <<"objects/user/login">>, user_id = <<>>}, State) ->
    {true, State};

service_api_allow(#nkreq{cmd = <<"objects/", _/binary>>, user_id = <<>>}, State) ->
    {false, State};

service_api_allow(#nkreq{cmd = <<"objects/", _/binary>>, req_state={_Type, Module, Cmd}}=Req, State) ->
    nklib_util:apply(Module, object_api_allow, [Cmd, Req, State]);

service_api_allow(#nkreq{cmd = <<"session", _/binary>>}, State) ->
    {true, State};

service_api_allow(#nkreq{cmd = <<"event", _/binary>>}, State) ->
    {true, State};

service_api_allow(#nkreq{cmd = <<"nkadmin", _/binary>>}, State) ->
    {true, State};

service_api_allow(_Req, _State) ->
    continue.


%% @doc
%%service_api_cmd(#nkreq{cmd = <<"objects/", _/binary>>, req_state={Type, Module, Cmd}}=Req, State) ->
%%    #nkreq{session_module=Mod, tid=TId} = Req,
%%    Self = self(),
%%    spawn_link(
%%        fun() ->
%%            Reply = case erlang:function_exported(Module, object_api_cmd, 2) of
%%                true ->
%%                    apply(Module, object_api_cmd, [Cmd, Req]);
%%                false ->
%%                    nkdomain_obj_api:api(Cmd, Type, Req)
%%            end,
%%            Mod:reply(Self, TId, Reply)
%%        end),
%%    {ack, State};

service_api_cmd(#nkreq{cmd = <<"objects/", _/binary>>, req_state={Type, Module, Cmd}}=Req, State) ->
    #nkreq{session_module=Mod, tid=TId} = Req,
    Reply = case erlang:function_exported(Module, object_api_cmd, 2) of
        true ->
            apply(Module, object_api_cmd, [Cmd, Req]);
        false ->
            nkdomain_obj_api:api(Cmd, Type, Req)
    end,
    case Reply of
        {login, R, U, M} ->
            {login, R, U, M, State};
        {ok, R} ->
            {ok, R, State};
        {ok, R, S} ->
            {ok, R, S, State};
        {error, E} ->
            {error, E, State}
    end;


service_api_cmd(_Req, _State) ->
    continue.


%% @private
api_server_reg_down({nkdomain_stop, Module, _Pid}, _Reason, State) ->
    {stop, {module_failed, Module}, State};

api_server_reg_down(_Link, _Reason, _State) ->
    continue.

%% @doc
api_server_http_auth(#nkreq{cmd = <<"objects/user/login">>}, _HttpReq) ->
    {true, <<>>, #{}, #{}};

api_server_http_auth(#nkreq{srv_id=SrvId}, HttpReq) ->
    Headers = nkapi_server_http:get_headers(HttpReq),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_util:get_http_auth(SrvId, Token) of
        {ok, UserId, Meta, State} ->
            {true, UserId, Meta, State};
        {error, Error} ->
            {error, Error}
    end.

%%%% @doc
%%api_server_handle_info({nkdist, {sent_link_down, Link}}, State) ->
%%    nkapi_server:stop(self(), {sent_link_down, Link}),
%%    {ok, State};
%%
%%api_server_handle_info(_Info, _State) ->
%%    continue.


%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [nkapi, nkadmin, nkmail, nkmail_smtp_client, nkfile_filesystem, nkfile_s3, nkservice_rest, nkservice_webserver].


%% @private
plugin_syntax() ->
    #{
        domain => binary,
        domain_default_store_id => binary
    }.


%% @private
plugin_config(#{domain:=_}=Config, _Service) ->
    {ok, Config};

plugin_config(_Config, _Service) ->
    {error, domain_unknown}.


%% @private
service_init(_Service, State) ->
    gen_server:cast(self(), nkdomain_load_domain),
    {ok, State}.


%% @private
service_handle_cast(nkdomain_load_domain, State) ->
    #{id:=SrvId} = State,
    #{domain:=Domain} = SrvId:config(),
    case nkdomain_obj_lib:load(SrvId, Domain, #{}) of
        #obj_id_ext{type = ?DOMAIN_DOMAIN, obj_id=ObjId, path=Path, pid=Pid} ->
            lager:info("Service loaded domain ~s (~s)", [Path, ObjId]),
            monitor(process, Pid),
            DomainData = #{
                domain_obj_id => ObjId,
                domain_path => Path,
                domain_pid => Pid
            },
            nkservice_srv:put(SrvId, nkdomain_data, DomainData),
            State2 = State#{nkdomain => DomainData},
            {noreply, State2};
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [Domain, Error]),
            {noreply, State}
    end;

service_handle_cast(_Msg, _State) ->
    continue.


%% @private
service_handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case State of
        #{nkdomain:=#{domain_pid:=Pid, domain_path:=Path}} ->
            lager:info("Service received domain '~s' down", [Path]),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
service_handle_info(_Msg, _State) ->
    continue.

%% ===================================================================
%% NkMAIL
%% ===================================================================

nkmail_get_provider(SrvId, Id) ->
    nkdomain_mail_provider_obj:get_provider(SrvId, Id).


%% ===================================================================
%% NkFILE
%% ===================================================================

nkfile_get_store(SrvId, Id) ->
    nkdomain_file_store_obj:get_store(SrvId, Id).


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
call_module(Fun, Args, #?NKOBJ{module=Module}=Session) ->
    case erlang:function_exported(Module, Fun, length(Args)+1) of
        true ->
            case apply(Module, Fun, Args++[Session]) of
                continue ->
                    {ok, Session};
                Other ->
                    Other
            end;
        false ->
            {ok, Session}
    end.


%% @private
call_parent_store(root, _Fun, _Args) ->
    {error, store_not_implemented};

call_parent_store(SrvId, Fun, Args) ->
    ?LLOG(warning, "calling root store", []),
    apply(root, Fun, [SrvId|Args]).

