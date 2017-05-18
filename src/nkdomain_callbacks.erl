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
-export([api_error/1, api_server_syntax/3, api_server_allow/2, api_server_cmd/2,
         api_server_reg_down/3]).
-export([admin_menu_fill_category/3]).
-export([object_mapping/0, object_syntax/1, object_parse/3, object_unparse/1]).
-export([object_load/2, object_get_session/1, object_save/1, object_delete/1, object_archive/1]).
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

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").


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
api_error({could_not_load_parent, Id})      -> {"Object could not load parent '~s'", [Id]};
api_error(domain_unknown)                   -> "Unknown domain";
api_error({invalid_name, N})                -> {"Invalid name '~s'", [N]};
api_error(invalid_object_id)                -> "Invalid object id";
api_error(invalid_object_type)              -> "Invalid object type";
api_error(invalid_object_path)              -> "Invalid object path";
api_error({invalid_object_path, P})         -> {"Invalid object path '~s'", [P]};
api_error({invalid_type, T})                -> {"Invalid type '~s'", [T]};
api_error(invalid_token_ttl)                -> "Invalid token TTL";
api_error(member_already_present)           -> "Member is already present";
api_error(member_not_found)                 -> "Member not found";
api_error(object_already_exists)            -> "Object already exists";
api_error(object_clean_process)             -> "Object cleaned (process stopped)";
api_error(object_clean_expire)              -> "Object cleaned (expired)";
api_error(object_deleted) 		            -> "Object removed";
api_error(object_expired) 		            -> "Object expired";
api_error(object_has_childs) 		        -> "Object has childs";
api_error(object_parent_conflict) 	        -> "Object has conflicting parent";
api_error(object_is_already_loaded)         -> "Object is already loaded";
api_error(object_is_disabled) 		        -> "Object is disabled";
api_error(object_is_stopped) 		        -> "Object is stopped";
api_error(object_not_found) 		        -> "Object not found";
api_error(object_not_started) 		        -> "Object is not started";
api_error(object_stopped) 		            -> "Object stopped";
api_error(parent_not_found) 		        -> "Parent not found";
api_error(parent_stopped) 		            -> "Parent stopped";
api_error(referred_not_found) 		        -> "Referred object not found";
api_error(session_already_present)          -> "Session is already present";
api_error(session_not_found)                -> "Session not found";
api_error(session_is_disabled)              -> "Session is disabled";
api_error(user_is_disabled) 		        -> "User is disabled";
api_error(user_unknown)                     -> "Unknown user";
api_error(_)   		                        -> continue.


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
        aliases => #{type => keyword},
        icon_id => #{type => keyword}
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
        subtype => binary,
        created_by => binary,
        created_time => integer,
        updated_by => binary,
        updated_time => integer,
        parent_id => binary,
        enabled => boolean,
        active => boolean,                    % Must be loaded
        expires_time => integer,
        destroyed_time => integer,
        destroyed_code => binary,
        destroyed_reason => binary,
        name => binary,
        description => binary,
        aliases => {list, binary},
        icon_id => binary,
        '_store_vsn' => any,
        '__mandatory' => [type, obj_id, parent_id, path, created_time]
    };

object_syntax(update) ->
    #{
        type => ignore,             % Do not count as unknown is updates
        enabled => boolean,
        name => binary,
        description => binary,
        aliases => {list, binary},
        icon_id => binary
    }.



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
                    {error, object_load_error}
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

object_save(#obj_session{is_dirty=false}=Session) ->
    {ok, Session};

object_save(#obj_session{srv_id=SrvId, obj_id=ObjId}=Session) ->
    {ok, Session2} = call_module(object_restore, [], Session),
    case call_module(object_save, [], Session2) of
        {ok, Session3} ->
            Map = SrvId:object_unparse(Session3),
            case nkdomain_store:save(SrvId, ObjId, Map) of
                {ok, _Vsn} ->
                    {ok, Session3#obj_session{is_dirty=false}};
                {error, Error} ->
                    {error, Error, Session3}
            end;
        {error, Error} ->
            {error, Error, Session2}
    end.


%% @doc Called to save the remove the object from disk
-spec object_delete(session()) ->
    {ok, session()} | {error, term(), session()}.

object_delete(#obj_session{srv_id=SrvId, obj_id=ObjId}=Session) ->
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

object_archive(#obj_session{srv_id=SrvId, obj_id=ObjId, obj=Obj}=Session) ->
    {ok, Session2} = call_module(object_restore, [], Session),
    case call_module(object_archive, [], Session2) of
        {ok, Session3} ->
            Map = SrvId:object_unparse(Session3#obj_session{obj=Obj}),
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
    case nkdomain_types:get_module(Type) of
        undefined ->
            {error, {invalid_type, Type}};
        Module ->
            case Module:object_parse(SrvId, Mode, Map) of
                {ok, Obj2, UnknownFields} ->
                    {ok, Obj2, UnknownFields};
                {error, Error} ->
                    {error, Error};
                {type_obj, TypeObj} ->
                    BaseSyn = SrvId:object_syntax(Mode),
                    case nklib_syntax:parse(Map, BaseSyn#{Type=>ignore}) of
                        {ok, Obj, UnknownFields} ->
                            {ok, Obj#{Type=>TypeObj}, UnknownFields};
                        {error, Error} ->
                            {error, Error}
                    end;
                Syntax when is_map(Syntax) ->
                    BaseSyn = SrvId:object_syntax(Mode),
                    nklib_syntax:parse(Map, BaseSyn#{Type=>Syntax})
            end
    end.



%% @doc Called to serialize an object to disk
-spec object_unparse(session()) ->
    map().

object_unparse(#obj_session{srv_id=SrvId, type=Type, module=Module, obj=Obj}) ->
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
    case call_type(object_check_active, [SrvId, ObjId], Type) of
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
api_server_syntax(Syntax, #nkapi_req{class=Type, subclass=Sub, cmd=Cmd}=Req, State) ->
    case nkdomain_types:get_module(Type) of
        undefined ->
            continue;
        Module ->
            case nklib_util:apply(Module, object_api_syntax, [Sub, Cmd, Syntax]) of
                Syntax2 when is_map(Syntax2) ->
                    {Syntax2, Req#nkapi_req{module=Module}, State};
                not_exported ->
                    continue;
                continue ->
                    continue
            end
    end.


%% @doc
api_server_allow(#nkapi_req{module=Module}=Req, State) when Module/=undefined ->
    #nkapi_req{subclass=Sub, cmd=Cmd, data=Data} = Req,
    nklib_util:apply(Module, object_api_allow, [Sub, Cmd, Data, State]);

api_server_allow(#nkapi_req{class=event}, State) ->
    {true, State};

api_server_allow(#nkapi_req{class=nkmail}, State) ->
    {true, State};

api_server_allow(#nkapi_req{class=nkadmin}, State) ->
    {true, State};

api_server_allow(_Req, _State) ->
    continue.


%% @doc
api_server_cmd(#nkapi_req{module=Module}=Req, State) when Module/=undefined ->
    #nkapi_req{subclass=Sub, cmd=Cmd, data=Data} = Req,
    #{srv_id:=SrvId} = State,
    Domain = case maps:find(domain, Data) of
        {ok, UserDomain} ->
            UserDomain;
        error ->
            nkdomain_util:get_service_domain(SrvId)
    end,
    case Domain of
        undefined ->
            {error, domain_unknown, State};
        _ ->
            State2 = ?ADD_TO_API_SESSION(domain, Domain, State),
            nklib_util:apply(Module, object_api_cmd, [Sub, Cmd, Req, State2])
    end;

api_server_cmd(_Req, _State) ->
    continue.


%% @private
api_server_reg_down({nkdomain_session_obj, _Pid}, _Reason, State) ->
    {stop, normal, State};

api_server_reg_down(_Link, _Reason, _State) ->
    continue.


%% ===================================================================
%% Admin
%% ===================================================================

admin_menu_fill_category(Category, Acc, State) ->
    #{types:=Types} = State,
    Acc2 = lists:foldl(
        fun({Type, Number}, FunAcc) ->
            case call_type(object_admin_tree, [Category, Number, State, FunAcc], Type) of
                ok -> FunAcc;
                FunAcc2 when is_map(FunAcc2) -> FunAcc2
            end
        end,
        Acc,
        maps:to_list(Types)),
    {continue, [Category, Acc2, State]}.



%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [nkelastic, nkadmin].


%% @private
plugin_syntax() ->
    #{
        domain => binary
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
%% Internal
%% ===================================================================


%% @private
call_type(Fun, Args, Type) ->
    case nkdomain_types:get_module(Type) of
        undefined ->
            ok;
        Module ->
            case erlang:function_exported(Module, Fun, length(Args)) of
                true ->
                    case apply(Module, Fun, Args) of
                        continue ->
                            ok;
                        Other ->
                            Other
                    end;
                false ->
                    ok
            end
    end.


%% @private
call_module(Fun, Args, #obj_session{module=Module}=Session) ->
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

