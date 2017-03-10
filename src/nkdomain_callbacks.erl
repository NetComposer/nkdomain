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
-export([plugin_deps/0, service_init/2, service_handle_cast/2]).
-export([object_base_mapping/0, object_base_syntax/0]).
-export([object_load/2, object_save/1, object_remove/2, object_parse/3, object_store/1,
         object_updated/2, object_enabled/1, object_sync_op/3, object_async_op/2]).
-export([object_init/1, object_terminate/2, object_event/2, object_reg_event/3,
         object_reg_down/3, object_start/1, object_stop/2,
         object_handle_call/3, object_handle_cast/2, object_handle_info/2]).
-export([object_store_reload_types/1, object_store_read_raw/2, object_store_save_raw/3,
         object_store_remove_raw/2, object_store_find_path/2, object_store_find_childs/3]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.


%% ===================================================================
%% Offered Object Callbacks
%% ===================================================================

-type session() :: nkdomain_obj:session().


%% @doc In-store base mapping
-spec object_base_mapping() -> map().

object_base_mapping() ->
    #{
        obj_id => #{type => keyword},
        module => #{type => keyword},
        type => #{type => keyword},
        path => #{type => keyword},
        parent_id => #{type => keyword},
        subtype => #{type => keyword},
        description => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        created_by => #{type => keyword},
        created_time => #{type => date},
        enabled => #{type => boolean},
        expires_time => #{type => date},
        destroyed_time => #{type => date},
        destroyed_code => #{type => keyword},
        destroyed_reason => #{type => keyword},
        aliases => #{type => keyword},
        icon_id => #{type => keyword}
    }.


%% @doc Base syntax
-spec object_base_syntax() -> nklib_syntax:syntax().

object_base_syntax() ->
    #{
        obj_id => binary,
        module => atom,
        type => binary,
        path => binary,
        parent_id => binary,
        subtype => binary,
        description => binary,
        created_by => binary,
        created_time => integer,
        parent_id => binary,
        enabled => boolean,
        expires_time => integer,
        destroyed_time => integer,
        destroyed_code => binary,
        destroyed_reason => binary,
        aliases => {list, binary},
        icon_id => binary,
        '_store_vsn' => any,
        '__mandatory' => [module, type, obj_id, path, parent_id, created_time]
    }.


%% @doc Called to parse an object's syntax
-spec object_load(nkservice:id(), nkdomain:obj_id()) ->
    {ok, nkdomain:obj()} | {error, term()}.

object_load(SrvId, ObjId) ->
    case SrvId:object_store_read_raw(SrvId, ObjId) of
        {ok, Module, Map} ->
            SrvId:object_parse(SrvId, Module, Map);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Called to save the object to disk
-spec object_save(session()) ->
    {ok, session()} | {error, term(), session()}.

object_save(#obj_session{srv_id=SrvId, obj_id=ObjId, module=Module, obj=Obj}=Session) ->
    BaseKeys = maps:keys(SrvId:object_base_mapping()),
    Store1 = maps:with(BaseKeys, Obj),
    Store2 = SrvId:object_store(Session),
    Store3 = Store1#{Module=>Store2},
    case SrvId:object_store_save_raw(SrvId, ObjId, Store3) of
        {ok, _Vsn} ->
            {ok, Session};
        {error, Error} ->
            {error, Error, Session}
    end.


%% @doc Called to save the remove the object from disk
-spec object_remove(Reason::term(), session()) ->
    {ok, session()} | {error, term(), session()}.

object_remove(_Reason, #obj_session{srv_id=SrvId, obj_id=ObjId}=Session) ->
    case SrvId:object_store_remove_raw(SrvId, ObjId) of
        ok ->
            {ok, Session};
        {error, Error} ->
            {error, Error, Session}
    end.


%% @doc Called to parse an object's syntax
-spec object_parse(nkservice:id(), module(), map()) ->
    {ok, nkdomain:obj()} | {error, term()}.

object_parse(SrvId, Module, Map) ->
    Base = SrvId:object_base_syntax(),
    Syntax = Module:object_add_syntax(Base),
    case nklib_syntax:parse(Map, Syntax, #{meta=>#{module=>Module}}) of
        {ok, #{module:=Module}=Obj2, _Exp, []} ->
            {ok, Obj2};
        {ok, #{module:=Module, obj_id:=ObjId}=Obj2, _Exp, Missing} ->
            ?LLOG(notice, "Object ~s (~s) has unknown fields: ~p", [Module, ObjId, Missing]),
            {ok, Obj2};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Called to get a "storable" version of the object
-spec object_store(session()) -> map().

object_store(#obj_session{module=Module, obj=Obj}) ->
    Module:object_store(Obj).


%% @doc Called when an object is modified
-spec object_updated(map(), session()) ->
    {ok, session()}.

object_updated(_Update, Session) ->
    {ok, Session}.


%% @doc Called when an object is enabled or disabled
-spec object_enabled(session()) ->
    {ok, session()}.

object_enabled(Session) ->
    {ok, Session}.


%% @doc
-spec object_sync_op(term(), {pid(), reference()}, session()) ->
    {reply, Reply::term(), session} |
    {noreply, session()} |
    {stop, Reason::term(), Reply::term(), session()} |
    {stop, Reason::term(), session()} |
    continue().

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @doc
-spec object_async_op(term(), session()) ->
    {noreply, session()} |
    {stop, Reason::term(), session()} |
    continue().

object_async_op(_Op, _Session) ->
    continue.




%% ===================================================================
%% Low level object
%% ===================================================================


%% @doc Called when a new session starts
-spec object_init(session()) ->
    {ok, session()} | {stop, Reason::term()}.

object_init(Session) ->
    {ok, Session}.


%% @doc Called when the session stops
-spec object_terminate(Reason::term(), session()) ->
    {ok, session()}.

object_terminate(_Reason, Session) ->
    {ok, Session}.


%% @private
-spec object_start(session()) ->
    {ok, session()} | continue().

object_start(Session) ->
    {ok, Session}.


%% @private
-spec object_stop(nkservice:error(), session()) ->
    {ok, session()} | continue().

object_stop(_Reason, Session) ->
    {ok, Session}.


%%  @doc Called when an event is sent
-spec object_event(nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_event(_Event, Session) ->
    {ok, Session}.


%% @doc Called when an event is sent, for each registered process to the session
-spec object_reg_event(nklib:link(), nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_reg_event(_Link, _Event, Session) ->
    {ok, Session}.


%% @doc Called when a registered process fails
-spec object_reg_down(nklib:link(), term(), session()) ->
    {ok, session()} | {stop, Reason::term(), session()} | continue().

object_reg_down(_Link, _Reason, Session) ->
    {stop, registered_down, Session}.


%% @doc
-spec object_handle_call(term(), {pid(), term()}, session()) ->
    {reply, term(), session()} | {noreply, session()} | continue().

object_handle_call(Msg, _From, Session) ->
    lager:error("Module nkdomain_obj received unexpected call: ~p", [Msg]),
    {noreply, Session}.


%% @doc
-spec object_handle_cast(term(), session()) ->
    {noreply, session()} | continue().

object_handle_cast(Msg, Session) ->
    lager:error("Module nkdomain_obj received unexpected cast: ~p", [Msg]),
    {noreply, Session}.


%% @doc
-spec object_handle_info(term(), session()) ->
    {noreply, session()} | continue().

object_handle_info(Msg, Session) ->
    lager:warning("Module nkdomain_obj received unexpected info: ~p", [Msg]),
    {noreply, Session}.



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
    {ok, module(), map()} | {error, term()}.

object_store_read_raw(_SrvId, _ObjId) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_save_raw(nkservice:id(), nkdomain:obj_id(), map()) ->
    {ok, Vsn::term()} | {error, term()}.

object_store_save_raw(_SrvId, _ObjId, _Map) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_remove_raw(nkservice:id(), nkdomain:obj_id()) ->
    {ok, Vsn::term()} | {error, term()}.

object_store_remove_raw(_SrvId, _ObjId) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_path(nkservice:id(), nkdomain:path()) ->
    {ok, module(), nkdomain:obj_id()} | {error, term()}.

object_store_find_path(_SrvId, _Path) ->
    {error, store_not_implemented}.


%% @doc
-spec object_store_find_childs(nkservice:id(), nkdomain:path(), Spec::map()) ->
    {ok, Total::integer(), [{module(), nkdomain:obj_id()}]} |
    {error, term()}.

object_store_find_childs(_SrvId, _Path, _Spec) ->
    {error, store_not_implemented}.




%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [nkelastic].


%% @private
service_init(_Service, State) ->
    gen_server:cast(self(), nkdomain_load_domain),
    {ok, State}.


%% @private
service_handle_cast(nkdomain_load_domain, State) ->
    {noreply, State};

service_handle_cast(_Msg, _State) ->
    continue.

