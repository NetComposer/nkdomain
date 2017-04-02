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
-export([plugin_deps/0, service_init/2, service_handle_cast/2, plugin_syntax/0, plugin_config/2]).
-export([api_error/1, api_server_syntax/3, api_server_allow/2, api_server_cmd/2]).
-export([object_mapping/0, object_syntax/1, object_parse/4, object_unparse/1]).
-export([object_load/2, object_save/1, object_delete/1, object_archive/2,
         object_updated/2, object_enabled/1, object_deleted/2,
         object_check_active/3,
         object_sync_op/3, object_async_op/2,
         object_status/2, object_all_links_down/1]).
-export([object_init/1, object_terminate/2, object_event/2, object_reg_event/3,
         object_reg_down/3, object_start/1, object_stop/2,
         object_handle_call/3, object_handle_cast/2, object_handle_info/2]).
-export([object_store_reload_types/1, object_store_read_raw/2, object_store_save_raw/3,
         object_store_delete_raw/2,
         object_store_archive_find/2, object_store_archive_save_raw/3]).
-export([object_store_find_obj_id/2, object_store_find_path/2,
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



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
api_error({could_not_load_parent, Id})      -> {"Object could not load parent '~s'", [Id]};
api_error(invalid_object_id)                -> "Invalid object id";
api_error(invalid_object_type)              -> "Invalid object type";
api_error(invalid_object_path)              -> "Invalid object path";
api_error({invalid_object_path, P})         -> {"Invalid object path '~s'", [P]};
api_error(object_already_exists)            -> "Object already exists";
api_error(object_clean_process)             -> "Object cleaned (process stopped)";
api_error(object_clean_expire)              -> "Object cleaned (expired)";
api_error(object_deleted) 		            -> "Object removed";
api_error(object_has_childs) 		        -> "Object has childs";
api_error(object_parent_conflict) 	        -> "Object has conflicting parent";
api_error(object_is_stopped) 		        -> "Object is stopped";
api_error(object_not_found) 		        -> "Object not found";
api_error(object_stopped) 		            -> "Object stopped";
api_error(parent_stopped) 		            -> "Parent stopped";
api_error(path_not_found) 		            -> "Path not found";
api_error(unknown_domain)                   -> "Domain is unknown";
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
        enabled => #{type => boolean},
        active => #{type => boolean},
        expires_time => #{type => date},
        destroyed_time => #{type => date},
        destroyed_code => #{type => keyword},
        destroyed_reason => #{type => keyword},
        description => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        aliases => #{type => keyword},
%%        pid => #{type => keyword},
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
        parent_id => binary,
        enabled => boolean,
        active => boolean,                    % Must be loaded
        expires_time => integer,
        destroyed_time => integer,
        destroyed_code => binary,
        destroyed_reason => binary,
        description => binary,
        aliases => {list, binary},
%%        pid => pid,
        icon_id => binary,
        '_store_vsn' => any,
        '__mandatory' => [type, obj_id, parent_id, path, created_time],
        id => ignore                                % Used in APIs
    };

object_syntax(update) ->
    #{
        enabled => boolean,
        description => binary,
        aliases => {list, binary},
        icon_id => binary,
        id => ignore                                % Used in APIs
    }.



%% @doc Called to get and parse an object
-spec object_load(nkservice:id(), nkdomain:obj_id()) ->
    {ok, nkdomain:obj()} | {error, term()}.

object_load(SrvId, ObjId) ->
    case SrvId:object_store_read_raw(SrvId, ObjId) of
        {ok, #{<<"type">>:=Type}=Map} ->
            SrvId:object_parse(SrvId, load, Type, Map);
        {ok, #{type:=Type}=Map} ->
            SrvId:object_parse(SrvId, load, Type, Map);
        {ok, _Map} ->
            {error, invalid_object_type};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Called to save the object to disk
-spec object_save(session()) ->
    {ok, session()} | {error, term(), session()}.

object_save(#obj_session{srv_id=SrvId, obj_id=ObjId}=Session) ->
    Map = SrvId:object_unparse(Session),
    case nkdomain_store:save(SrvId, ObjId, Map) of
        {ok, _Vsn} ->
            {ok, Session};
        {error, Error} ->
            {error, Error, Session}
    end.


%% @doc Called to save the remove the object from disk
-spec object_delete(session()) ->
    {ok, session()} | {error, term(), session()}.

object_delete(#obj_session{srv_id=SrvId, obj_id=ObjId}=Session) ->
    case nkdomain_store:delete(SrvId, ObjId) of
        ok ->
            {ok, Session};
        {error, Error} ->
            {error, Error, Session}
    end.


%% @doc Called to save the archived version to disk
-spec object_archive(nkdomain:obj(), session()) ->
    {ok, session()} | {error, term(), session()}.

object_archive(Obj, #obj_session{srv_id=SrvId, obj_id=ObjId}=Session) ->
    Map = SrvId:object_unparse(Session#obj_session{obj=Obj}),
    case nkdomain_store:archive(SrvId, ObjId, Map) of
        ok ->
            {ok, Session};
        {error, Error} ->
            {error, Error, Session}
    end.


%% @doc Called to parse an object's syntax
-spec object_parse(nkservice:id(), nkdomain:type(), load|update, map()) ->
    {ok, nkdomain:obj()} | {error, term()}.

object_parse(SrvId, Mode, Type, Map) ->
    case nkdomain_types:get_module(Type) of
        undefined ->
            {error, {invalid_type, Type}};
        Module ->
            BaseSyn = SrvId:object_syntax(Mode),
            BaseMand = maps:get('__mandatory', BaseSyn, []),
            ModSyn = Module:object_syntax(Mode),
            ModMand = [
                <<Type/binary, $., (nklib_util:to_binary(F))/binary>>
                || F <- maps:get('__mandatory', ModSyn, [])
            ],
            Mandatory = BaseMand ++ ModMand,
            TypeAtom = binary_to_atom(Type, utf8),
            Syntax = BaseSyn#{TypeAtom => ModSyn, '__mandatory'=>Mandatory},
            Opts = #{
                meta => #{module=>Module},
                {binary_key, Type} => true
            },
            case nklib_syntax:parse(Map, Syntax, Opts) of
                {ok, Obj2, _Exp, []} ->
                    {ok, Obj2};
                {ok, Obj2, _Exp, Missing} ->
                    ?LLOG(notice, "Object of type ~s has unknown fields: ~p", [Type, Missing]),
                    {ok, Obj2};
                {error, Error} ->
                    {error, Error}
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
    ModKeys = maps:keys(Module:object_mapping()),
    ModMap = maps:with(ModKeys, ModData),
    BaseMap2#{Type => ModMap}.





%% ===================================================================
%% Object-related callbacks
%% ===================================================================


%% @doc Called if an active object is detected on storage
%% If 'true' is returned, the object is ok
%% If 'false' is returned, it only means that the object has been processed
-spec object_check_active(nkservice:id(), nkdomain:type(), nkdomain:obj_id()) ->
    boolean().

object_check_active(SrvId, Type, ObjId) ->
    case call_type(object_check_active, [SrvId, ObjId], Type) of
        ok ->
            true;
        Other ->
            Other
    end.


%% @doc Called when an object is modified
-spec object_updated(map(), session()) ->
    {ok, session()}.

object_updated(Update, Session) ->
    call_module(object_updated, [Update], Session).


%% @doc Called when an object is enabled or disabled
-spec object_enabled(session()) ->
    {ok, session()}.

object_enabled(Session) ->
    call_module(object_enabled, [], Session).


%% @doc Called when an object is removed
-spec object_deleted(term(), session()) ->
    {ok, session()}.

object_deleted(Reason, Session) ->
    call_module(object_deleted, [Reason], Session).


%% @doc
-spec object_sync_op(term(), {pid(), reference()}, session()) ->
    {reply, Reply::term(), session} |
    {noreply, session()} |
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
    {noreply, session()} |
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
-spec object_status(term(), session()) ->
    {ok, session()}.

object_status(Status, Session) ->
    call_module(object_status, [Status], Session).


%% @doc
-spec object_all_links_down(session()) ->
    {keepalive, session()} | {stop, nkservice:error(), session()}.

object_all_links_down(Session) ->
    case call_module(object_all_links_down, [], Session) of
        {ok, Session2} ->
            {stop, no_usages, Session2};
        Other ->
            Other
    end.


%% ===================================================================
%% Low level object
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
    call_module(object_event, [Event], Session).


%% @doc Called when an event is sent, for each registered process to the session
-spec object_reg_event(nklib:link(), nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_reg_event(Link, Event, Session) ->
    call_module(object_reg_event, [Link, Event], Session).


%% @doc Called when a registered process fails
-spec object_reg_down(nklib:link(), term(), session()) ->
    {ok, session()} | {stop, Reason::term(), session()} | continue().

object_reg_down(Link, Reason, Session) ->
    call_module(object_reg_down, [Link, Reason], Session).


%% @doc
-spec object_handle_call(term(), {pid(), term()}, session()) ->
    {reply, term(), session()} | {noreply, session()} | continue().

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
    {noreply, session()} | continue().

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
    {noreply, session()} | continue().

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

object_store_reload_types(_SrvId) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_read_raw(nkservice:id(), nkdomain:obj_id()) ->
    {ok, [map()]} | {error, term()}.

object_store_read_raw(_SrvId, _ObjId) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_save_raw(nkservice:id(), nkdomain:obj_id(), map()) ->
    {ok, Vsn::term()} | {error, term()}.

object_store_save_raw(_SrvId, _ObjId, _Map) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_delete_raw(nkservice:id(), nkdomain:obj_id()) ->
    ok | {error, object_has_chids|term()}.

object_store_delete_raw(_SrvId, _ObjId) ->
    {error, store_not_implemented}.


%%%% @doc
%%-spec object_store_find_obj(nkservice:id(), nkdomain:id()) ->
%%    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, term()}.
%%
%%object_store_find_obj(_SrvId, _Id) ->
%%    {error, store_not_implemented}.


%% @doc
-spec object_store_find_obj_id(nkservice:id(), nkdomain:obj_id()) ->
    {ok, nkdomain:type(), nkdomain:path()} | {error, term()}.

object_store_find_obj_id(_SrvId, _ObjId) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_path(nkservice:id(), nkdomain:path()) ->
    {ok, nkdomain:type(), nkdomain:obj_id()} | {error, term()}.

object_store_find_path(_SrvId, _Path) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_types(nkservice:id(), nkdomain:obj_id(), map()) ->
    {ok, Total::integer(), [{nkdomain:type(), integer()}]} | {error, term()}.

object_store_find_types(_SrvId, _ObjId, _Spec) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_all_types(nkservice:id(), nkdomain:path(), map()) ->
    {ok, Total::integer(), [{nkdomain:type(), integer()}]} | {error, term()}.

object_store_find_all_types(_SrvId, _ObjId, _Spec) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_childs(nkservice:id(), nkdomain:obj_id(), Spec::map()) ->
    {ok, Total::integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]} |
    {error, term()}.

object_store_find_childs(_SrvId, _ObjId, _Spec) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_all_childs(nkservice:id(), nkdomain:path(), Spec::map()) ->
    {ok, Total::integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]} |
    {error, term()}.

object_store_find_all_childs(_SrvId, _Path, _Spec) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_alias(nkservice:id(), nkdomain:alias()) ->
    {ok, Total::integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}]} |
    {error, term()}.

object_store_find_alias(_SrvId, _Alias) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find(nkservice:id(), map()) ->
    {ok, Total::integer(), [map()], map(), map()} |
    {error, term()}.

object_store_find(_SrvId, _Spec) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_archive_find(nkservice:id(), Spec::map()) ->
    {ok, integer(), [map()]} | {error, term()}.

object_store_archive_find(_SrvId, _Spec) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_archive_save_raw(nkservice:id(), nkdomain:obj_id(), map()) ->
    ok | {error, term()}.

object_store_archive_save_raw(_SrvId, _ObjId, _Map) ->
    {error, store_not_implemented}.


%% @doc Must stop loaded objects
-spec object_store_delete_all_childs(nkservice:id(), nkdomain:path(), Spec::map()) ->
    {ok, Total::integer()} | {error, term()}.

object_store_delete_all_childs(_SrvId, _Path, _Spec) ->
    {error, store_not_implemented}.


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_check_active/3 for each 'active' object found
-spec object_store_clean(nkservice:id()) ->
    ok.

object_store_clean(_SrvId) ->
    {error, store_not_implemented}.


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
            {error, unknown_domain, State};
        _ ->
            State2 = ?ADD_TO_API_SESSION(domain, Domain, State),
            nklib_util:apply(Module, object_api_cmd, [Sub, Cmd, Data, State2])
    end;

api_server_cmd(_Req, _State) ->
    continue.


%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [nkelastic].


%% @private
plugin_syntax() ->
    #{
        domain => binary
    }.


%% @private
plugin_config(#{domain:=_}=Config, _Service) ->
    {ok, Config};

plugin_config(_Config, _Service) ->
    {error, unknown_domain}.


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
                    apply(Module, Fun, Args);
                false ->
                    ok
            end
    end.


%% @private
call_module(Fun, Args, #obj_session{module=Module}=Session) ->
    case erlang:function_exported(Module, Fun, length(Args)+1) of
        true ->
            apply(Module, Fun, Args++[Session]);
        false ->
            {ok, Session}
    end.





