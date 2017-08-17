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


%% @doc Basic Obj behaviour
%% One of this objects is started for each object, distributed in the cluster
%%
%% Unload policy
%%
%% - Permanent mode
%%      if object has permanent => true in object_info(), it is permanent
%% - Expires mode
%%      if object has expires property, the object expires after that time no matter what
%% - TTL mode
%%      otherwise, property default_ttl in object_info() is used for ttl, default is ?DEFAULT_TTL
%%      once expired, the object is unloaded if no childs or usages
%%      some functions restart de count calling do_refresh()
%%
%% Enabled policy
%%
%% - For an object to be loaded, both domain and parent must be able to be loaded
%% - It will start enabled only if both parent and domain are enabled and the enabled
%%   property of the object is not false
%% - Object should not be created if parent or domain are disabled
%% - If parent or domain is disabled, object is disabled until both are enabled
%% - It is up to any sync_op or async_op object type implementation to work or not in disabled objects

-module(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([sync_op/3, sync_op/4, async_op/3]).
-export([start/4, new_type_master/1, object_deleted/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3,  handle_cast/2, handle_info/2]).
-export([links_add/3, links_remove/3, links_iter/4]).
-export([get_all/0, unload_all/0, get_state/2, get_time/2]).
-export([do_update_name/2, do_stop/2]).
-export_type([event/0, status/0]).


-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(DEFAULT_TTL, 10000).
-define(RELOAD_PARENT_TIME, 1000).
-define(MOVE_WAIT_TIME, 30000).
-define(DEF_SYNC_CALL, 5000).


-compile({no_auto_import, [register/2]}).


%% ===================================================================
%% Callbacks definitions
%% ===================================================================

-type state() :: #?STATE{}.


-callback object_info() ->
    object_info().



%% ===================================================================
%% Types
%% ===================================================================


-type object_info() ::
    #{
        type => nkdomain:type(),                        %% Mandatory!
        permanent => boolean(),
        default_ttl => integer(),                    %% msecs
        stop_after_disabled => boolean(),               %% Default false
        remove_after_stop => boolean(),
        dont_create_childs_on_disabled => boolean(),    %% Default false
        dont_update_on_disabled => boolean(),           %% Default false
        dont_delete_on_disabled => boolean(),           %% Default false
        default_token_ttl => integer(),
        max_token_ttl => integer()
    }.

-type info() :: atom().

-type event() ::
    created |
    loaded |
    {status, status()} |
    saved |
    {updated, map()} |
    deleted |
    {enabled, boolean()} |
    {child_loaded, nkdomain:type(), nkdomain:obj_id(), pid()} |
    {child_unloaded, nkdomain:type(), nkdomain:obj_id()} |
    {info, info(), map()} |
    {stopped, Code::binary(), Txt::binary()} |
    {unloaded, nkservice:error()}.

-type status() ::
    loaded |
    {unloaded, nkservice:error()} |
    term().


%% If an object has a session_link and session_events, events will be sent directly to
%% that the session (using object_session_event/3 in nkdomain_obj_util:send_session_event/2).
%% session_link is also used by some session objects to link to api server
%% (see link_to_session_server and unlink_from_session_server in nkdomain_obj_util)
-type start_opts() ::
    #{
        enabled => boolean(),
        session_events => [binary()],       % See bellow
        session_link => nklib_links:link(), %
        meta => map()
    }.


-type sync_op() ::
    get_obj |
    get_obj_info |
    get_obj_name |
    get_state |
    get_childs|
    get_domain_id |
    get_time |
    is_enabled |
    {enable, boolean()} |
    {update_name, binary()} |
    {update, map()} |
    delete |
    {register, usage|link, nklib:link()} |
    save |
    term().

-type async_op() ::
    refresh_timer |
    {unregister, usage|link, nklib:link()} |
    {send_info, atom()|binary(), map()} |
    {send_event, event()} |
    save |
    {unload, Reason::nkservice:error()} |
    term().


%% ===================================================================
%% Public
%% ===================================================================


%% @private
-spec start(nkservice:id(), nkdomain:obj(), loaded|created, start_opts()) ->
    {ok, pid()} | {error, term()}.

start(SrvId, #{obj_id:=ObjId}=Obj, Op, Meta) ->
    case nkdomain_lib:get_node(ObjId) of
        {ok, Node} ->
            case rpc:call(Node, gen_server, start, [?MODULE, {Op, SrvId, Obj, Meta}, []]) of
                {ok, Pid} -> {ok, Pid};
                {error, {already_registered, Pid}} -> {ok, Pid};
                {error, Error} -> {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec sync_op(nkservice:id(), nkdomain:id()|pid(), sync_op()) ->
    term() | {error, timeout|process_not_found|object_not_found|term()}.

sync_op(SrvId, IdOrPid, Op) ->
    sync_op(SrvId, IdOrPid, Op, ?DEF_SYNC_CALL).


%% @doc
-spec sync_op(nkservice:id(), pid()|nkservice:id(), sync_op(), timeout()) ->
    term() | {error, timeout|process_not_found|object_not_found|term()}.

sync_op(_SrvId, Pid, Op, Timeout) when is_pid(Pid) ->
    nkservice_util:call(Pid, {nkdomain_sync_op, Op}, Timeout);

sync_op(SrvId, Id, Op, Timeout) ->
    sync_op(SrvId, Id, Op, Timeout, 5).


%% @private
sync_op(_SrvId, _Id, _Op, _Timeout, 0) ->
    {error, process_not_found};

sync_op(SrvId, Id, Op, Timeout, Tries) ->
    case nkdomain_lib:load(SrvId, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            case sync_op(SrvId, Pid, Op, Timeout) of
                {error, process_not_found} ->
                    lager:notice("NkDOMAIN SynOP failed, retrying..."),
                    timer:sleep(250),
                    sync_op(SrvId, Id, Op, Timeout, Tries-1);
                Other ->
                    Other
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec async_op(nkservice:id(), nkdomain:id()|pid(), async_op()) ->
    ok | {error, process_not_found|object_not_found|term()}.

async_op(_SrvId, Pid, Op) when is_pid(Pid) ->
    gen_server:cast(Pid, {nkdomain_async_op, Op});

async_op(SrvId, Id, Op) ->
    case nkdomain_lib:load(SrvId, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            async_op(SrvId, Pid, Op);
        {error, Error} ->
            {error, Error}
    end.


%% @private Called from nkdomain_type
new_type_master(Pid) ->
    gen_server:cast(Pid, nkdomain_new_type_master).


%% @private Called when the object has been deleted on database
object_deleted(Pid) ->
    gen_server:cast(Pid, nkdomain_obj_deleted).


%% @private
get_state(SrvId, Id) ->
    sync_op(SrvId, Id, get_state).


%% @private
get_time(SrvId, Id) ->
    sync_op(SrvId, Id, get_time).


%% @doc
-spec get_all() ->
    [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path(), pid()}].

get_all() ->
    [
        {Type, ObjId, Path, Pid} ||
        {{Type, ObjId, Path}, Pid} <- nklib_proc:values(?MODULE)
    ].

%% @private
unload_all() ->
    lists:foreach(
        fun({_Module, _ObjId, _Path, Pid}) -> async_op(none, Pid, {unload, normal}) end,
        get_all()).


% ===================================================================
%% gen_server behaviour
%% ===================================================================


%% @private
-spec init(term()) ->
    {ok, state()} | {error, term()}.

init({Op, SrvId, Obj, Meta}) when Op==loaded; Op==created ->
    #{
        type := Type,
        obj_id := ObjId,
        path := Path,
        obj_name := ObjName,
        domain_id := DomainId,
        parent_id := ParentId
    } = Obj,
    Module = nkdomain_all_types:get_module(Type),
    false = Module==undefined,
    Enabled = case maps:find(enabled, Meta) of
        {ok, false} ->
            false;
        _ ->
            maps:get(enabled, Obj, true)
    end,
    Info = Module:object_info(),
    ObjIdExt = #obj_id_ext{srv_id=SrvId, obj_id=ObjId, type=Type, path=Path, pid=self()},
    State1 = #?STATE{
        srv_id = SrvId,
        id = ObjIdExt,
        module = Module,
        domain_id = DomainId,
        parent_id = ParentId,
        obj_name = ObjName,
        object_info = Info,
        obj = Obj,
        is_dirty = (Op == created),
        is_enabled = Enabled,
        started = nkdomain_util:timestamp(),
        childs = #{},
        usage_links = nklib_links:new(),
        event_links = nklib_links:new(),
        status = init,
        session_events = maps:get(session_events, Meta, []),
        session_link = maps:get(session_link, Meta, undefined),
        meta = maps:get(meta, Meta, #{}),
        unload_policy = set_unload_policy(Obj, Info),
        domain_enabled = true,
        parent_enabled = true,
        session = #{}
    },
    ?DEBUG("started (~p)", [self()], State1),
    State2 = do_init_common(State1),
    % Register obj_id, domain, parent and type
    case do_register(State2) of
        {ok, State3} ->
            case handle(object_init, [], State3) of
                {ok, State4} ->
                    State5 = case Op of
                        created ->
                            ?DEBUG("created (~p)", [self()], State4),
                            do_event(created, State4);
                        loaded ->
                            State4
                    end,
                    ?DEBUG("loaded (~p)", [self()], State5),
                    State6 = do_event(loaded, State5),
                    % Save if created or init sets is_dirty = true
                    case do_save(creation, State6) of
                        {ok, State7} ->
                            {ok, do_refresh(State7)};
                        {{error, Error}, _State7} ->
                            {stop, Error}
                    end;
                {error, Error} ->
                    {stop, Error}
            end;
        {error, Error} ->
            {stop, Error}
    end;

init({moved, State, OldPid}) ->
    State2 = do_init_common(State),
    case do_register(OldPid, State2) of
        {ok, State3} ->
            %% TODO: Add 'moved' callback
            %% TODO: Re-register childs
            ?DEBUG("moved (~p)", [self()], State3),
            {ok, do_refresh(State3)};
        {error, Error} ->
            {stop, Error}
    end.


%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {noreply, state()} | {reply, term(), state()} |
    {stop, Reason::term(), state()} | {stop, Reason::term(), Reply::term(), state()}.

handle_call(Msg, _From, #?STATE{moved_to=Pid}=State) when is_pid(Pid) ->
    ?DEBUG("forwarding call to new process", [], State),
    Reply = gen_server:call(Pid, Msg, infinity),
    reply(Reply, State);

handle_call({nkdomain_sync_op, Op}, From, State) ->
    case handle(object_sync_op, [Op, From], State) of
        {reply, Reply, #?STATE{}=State2} ->
            reply(Reply, do_refresh(State2));
        {reply_and_save, Reply, #?STATE{}=State2} ->
            {_, State3} = do_save(user_op, State2),
            reply(Reply, do_refresh(State3));
        {noreply, #?STATE{}=State2} ->
            noreply(do_refresh(State2));
        {noreply_and_save, #?STATE{}=State2} ->
            {_, State3} = do_save(user_op, State2),
            noreply(do_refresh(State3));
        {stop, Reason, Reply, #?STATE{}=State2} ->
            gen_server:reply(From, Reply),
            do_stop(Reason, State2);
        {stop, Reason, #?STATE{}=State2} ->
            do_stop(Reason, State2);
        continue ->
            do_sync_op(Op, From, State);
        {continue, [Op2, _From2, #?STATE{}=State2]} ->
            do_sync_op(Op2, From, State2)
    end;

handle_call(Msg, From, State) ->
    handle(object_handle_call, [Msg, From], State).


%% @private
-spec handle_cast(term(), state()) ->
    {noreply, state()} | {stop, term(), state()}.

handle_cast(Msg, #?STATE{moved_to=Pid}=State) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg),
    noreply(State);

handle_cast({nkdomain_async_op, Op}, State) ->
    case handle(object_async_op, [Op], State) of
        {noreply, #?STATE{}=State2} ->
            noreply(do_refresh(State2));
        {noreply_and_save, #?STATE{}=State2} ->
            {_, State3} = do_save(user_op, State2),
            noreply(do_refresh(State3));
        {stop, Reason, #?STATE{}=State2} ->
            do_stop(Reason, State2);
        continue ->
            do_async_op(Op, State);
        {continue, [Op2, #?STATE{}=State2]} ->
            do_async_op(Op2, State2)
    end;

handle_cast({nkdomain_parent_enabled, Enabled}, State) ->
    noreply(do_enabled(State#?STATE{parent_enabled=Enabled}));

handle_cast({nkdomain_domain_enabled, Enabled}, State) ->
    noreply(do_enabled(State#?STATE{domain_enabled=Enabled}));

handle_cast(nkdomain_service_stopped, State) ->
    ?LLOG(warning, "service stopped", [], State),
    do_stop(service_down, State);

handle_cast(nkdomain_new_type_master, State) ->
    {noreply, register_type(State)};

handle_cast(nkdomain_obj_deleted, State) ->
    do_stop(object_deleted, State#?STATE{is_dirty=false});

handle_cast(Msg, State) ->
    handle(object_handle_cast, [Msg], State).


%% @private
-spec handle_info(term(), state()) ->
    {noreply, state()} | {stop, term(), state()}.

handle_info(nkdomain_move_completed, State) ->
    ?DEBUG("move completed", [], State),
    {stop, normal, State};

handle_info({nkservice_updated, _SrvId}, #?STATE{id=#obj_id_ext{srv_id=SrvId, type=Type}}=State) ->
    set_log(SrvId, Type),
    noreply(State);

handle_info(nkdomain_check_expire, State) ->
    case do_check_expired(State) of
        true ->
            do_stop(object_expired, State);
        {false, State2} ->
            noreply(State2)
    end;

handle_info(nkdomain_timeout, State) ->
    do_check_timeout(State);

handle_info(nkdomain_destroy, State) ->
    {stop, normal, State};

handle_info(nkdomain_find_parent, #?STATE{parent_pid=undefined}=State) ->
    case register_parent(State) of
        {ok, State2} ->
            ?DEBUG("parent reloaded", [], State),
            noreply(State2);
        {error, Error} ->
            ?LLOG(notice, "object could not reload parent: ~p", [Error], State),
            erlang:send_after(?RELOAD_PARENT_TIME, self(), nkdomain_find_parent),
            noreply(State)
    end;

handle_info(nkdomain_find_parent, State) ->
    {noreply, State};

handle_info(nkdomain_find_domain, #?STATE{domain_pid=undefined}=State) ->
    case register_domain(State) of
        {ok, State2} ->
            ?DEBUG("domain reloaded", [], State),
            noreply(State2);
        {error, Error} ->
            ?LLOG(notice, "object could not reload domain: ~p", [Error], State),
            erlang:send_after(?RELOAD_PARENT_TIME, self(), nkdomain_find_domain),
            noreply(State)
    end;

handle_info(nkdomain_find_domain, State) ->
    {noreply, State};

handle_info({nkdist, Msg}, State) ->
    do_nkdist(Msg, State);

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #?STATE{parent_pid=Pid}=State) ->
    ?LLOG(notice, "parent has failed", [], State),
    self() ! nkdomain_find_parent,
    State2 = do_enabled(State#?STATE{parent_pid=undefined, parent_enabled=false}),
    noreply(State2);

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #?STATE{domain_pid=Pid}=State) ->
    ?LLOG(notice, "domain has failed", [], State),
    self() ! nkdomain_find_domain,
    State2 = do_enabled(State#?STATE{domain_pid=undefined, domain_enabled=false}),
    noreply(State2);

handle_info({'DOWN', Ref, process, _Pid, _Reason}, #?STATE{type_monitor=Ref}=State) ->
    Self = self(),
    spawn_link(
        fun() ->
            timer:sleep(5000),
            new_type_master(Self)
        end),
    {noreply, State};

handle_info({'DOWN', Ref, process, _Pid, _Reason}=Info, State) ->
    case links_down(Ref, State) of
        {event, State2} ->
            {ok, State3} = handle(object_link_down, [event], State2),
            noreply(State3);
        {usage, {child, ObjId, _Pid}, State2} ->
            State3 = do_rm_child(ObjId, State2),
            {ok, State4} = handle(object_link_down, [{child, ObjId}], State3),
            do_check_timeout(State4);
        {usage, Link, State2} ->
            {ok, State3} = handle(object_link_down, [{usage, Link}], State2),
            do_check_timeout(State3);
        not_found ->
            handle(object_handle_info, [Info], State)
    end;

handle_info(Msg, State) ->
    handle(object_handle_info, [Msg], State).


%% @private
-spec code_change(term(), state(), term()) ->
    {ok, state()}.

code_change(OldVsn, #?STATE{srv_id=SrvId}=State, Extra) ->
    SrvId:object_code_change(OldVsn, State, Extra).


%% @private
-spec terminate(term(), state()) ->
    ok.

terminate(_Reason, #?STATE{moved_to=Pid}) when is_pid(Pid) ->
    ok;

terminate(Reason, State) ->
    State2 = do_stop2({terminate, Reason}, State),
    {ok, _State3} = handle(object_terminate, [Reason], State2),
    ok.


%% ===================================================================
%% Operations
%% ===================================================================

%% @private
do_sync_op(get_obj_info, _From, State) ->
    Reply = nkdomain_obj_util:get_obj_info(State),
    reply({ok, Reply}, do_refresh(State));

do_sync_op(get_obj_name, _From, State) ->
    Reply = nkdomain_obj_util:get_obj_name(State),
    reply({ok, Reply}, do_refresh(State));

do_sync_op(get_obj, _From, State) ->
    #?STATE{
        module = Module,
        status = Status,
        started = Started,
        is_enabled = Enabled,
        obj = Obj
    } = State,
    Obj2 = Obj#{
        '_module' => Module,
        '_status' => Status,
        '_started' => Started,
        '_is_enabled' => Enabled
    },
    reply({ok, Obj2}, State);

do_sync_op(get_state, _From, State) ->
    reply({ok, State}, State);

do_sync_op(get_childs, _From, #?STATE{childs=Childs}=State) ->
    reply({ok, Childs}, State);

do_sync_op(get_time, _From, State) ->
    reply({ok, get_timer(State)}, State);

do_sync_op(get_domain_id, _From, #?STATE{domain_id=DomainId}=State) ->
    reply({ok, DomainId}, State);

do_sync_op(get_path, _From, #?STATE{id=#obj_id_ext{path=Path}}=State) ->
    reply({ok, Path}, State);

do_sync_op(save, _From, State) ->
    {Reply, State2} = do_save(user_order, State),
    reply(Reply, State2);

do_sync_op(delete, From, #?STATE{is_enabled=IsEnabled, object_info=Info}=State) ->
    case {IsEnabled, Info} of
        {false, #{dont_delete_on_disabled:=true}} ->
            reply({error, object_is_disabled}, State);
        _ ->
            {Reply, State2} = do_delete(State),
            gen_server:reply(From, Reply),
            case Reply of
                ok ->
                    do_stop(object_deleted, State2);
                _ ->
                    noreply(State)
            end
    end;

do_sync_op({enable, Enable}, From, #?STATE{obj=Obj}=State) ->
    case maps:get(enabled, Obj, true) of
        Enable ->
            reply(ok, State);
        _ ->
            case do_update(#{enabled=>Enable}, State) of
                {ok, [], State2} ->
                    gen_server:reply(From, ok),
                    noreply(do_enabled(do_refresh(State2)));
                {error, Error, State2} ->
                    reply({error, Error}, State2)
            end
    end;

do_sync_op(is_enabled, _From, #?STATE{is_enabled=IsEnabled}=State) ->
    reply({ok, IsEnabled}, State);

do_sync_op({register, Type, Link}, _From, State) ->
    {reply, ok, links_add(Type, Link, State)};

do_sync_op({update, Map}, _From, #?STATE{is_enabled=IsEnabled, object_info=Info}=State) ->
    case {IsEnabled, Info} of
        {false, #{dont_update_on_disabled:=true}} ->
            reply({error, object_is_disabled}, State);
        _ ->
            case do_update(Map, State) of
                {ok, UnknownFields, State2} ->
                    reply({ok, UnknownFields}, do_refresh(State2));
                {error, Error, State2} ->
                    reply({error, Error}, State2)
            end
    end;

do_sync_op({update_name, _ObjName}, _From, #?STATE{id=#obj_id_ext{type=?DOMAIN_DOMAIN}}=State) ->
    reply({error, domains_name_cannot_change}, State);

do_sync_op({update_name, ObjName}, _From, #?STATE{is_enabled=IsEnabled, object_info=Info}=State) ->
    case {IsEnabled, Info} of
        {false, #{dont_update_on_disabled:=true}} ->
            reply({error, object_is_disabled}, State);
        _ ->
            ObjName2 = nkdomain_util:name(ObjName),
            case do_update_name(ObjName2, State) of
                {ok, State2} ->
                    State3 = do_event({updated, #{obj_name=>ObjName2}}, State2),
                    reply({ok, ObjName2}, do_refresh(State3));
                {error, Error} ->
                    reply({error, Error}, State)
            end
    end;

do_sync_op({nkdomain_reg_child, ObjIdExt}, _From, State) ->
    #obj_id_ext{obj_id=ObjId, type=Type, path=Path, pid=Pid} = ObjIdExt,
    #?STATE{is_enabled=IsEnabled, object_info=Info} = State,
    case {IsEnabled, Info} of
        {false, #{dont_create_childs_on_disabled:=true}} ->
            reply({error, parent_is_disabled}, State);
        _ ->
            ?DEBUG("registering child ~s", [Path], State),
            %% Check do_check_child(ObjIdExt, State)
            State2 = do_rm_child(ObjId, State),
            State3 = do_add_child(ObjId, Type, Pid, State2),
            State4 = do_event({child_loaded, Type, ObjId, Pid}, State3),
            {reply, {ok, IsEnabled, self()}, State4}
    end;

do_sync_op(Op, _From, State) ->
    ?LLOG(notice, "unknown sync op: ~p", [Op], State),
    reply({error, unknown_op}, State).


%% @private
do_async_op({unregister, Type, Link}, State) ->
    {ok, links_remove(Type, Link, State)};

do_async_op({send_info, Info, Meta}, State) ->
    noreply(do_event({info, Info, Meta}, do_refresh(State)));

do_async_op({send_event, Event}, State) ->
    noreply(do_event(Event, do_refresh(State)));

do_async_op(save, State) ->
    {_Reply, State2} = do_save(user_order, State),
    noreply(State2);

do_async_op({unload, Reason}, State) ->
    ?DEBUG("received unload: ~p", [Reason], State),
    do_stop(Reason, State);

do_async_op(Op, State) ->
    ?LLOG(notice, "unknown async op: ~p", [Op], State),
    noreply(State).



%% ===================================================================
%% Internals
%% ===================================================================

%% @private
do_init_common(State) ->
    #?STATE{
        id = #obj_id_ext{srv_id=SrvId, type=Type},
        unload_policy = Policy
    }= State,
    % If expired, do proper delete
    case Policy of
        {expires, _} ->
            self() ! nkdomain_check_expire;
        _ ->
            ok
    end,
    set_log(SrvId, Type),
    nkservice_util:register_for_changes(SrvId),
    State.


%% @private
set_log(SrvId, Type) ->
    Debug =
        case nkservice_util:get_debug_info(SrvId, ?MODULE) of
            {true, all} -> true;
            {true, #{types:=Types}} -> lists:member(Type, Types);
            {true, _} -> true;
            _ -> false
        end,
    % lager:notice("DEBUG: ~p", [Debug]),
    put(object_debug, Debug).


%% @private
do_register(State) ->
    do_register(none, State).


%% @private
do_register(OldPid, State) ->
    case register_obj_id(OldPid, State) of
        ok ->
            case register_domain(State) of
                {ok, State2} ->
                    case register_parent(State2) of
                        {ok, State3} ->
                            State4 = register_type(State3),
                            {ok, State4};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, {pid_conflict, Pid}} ->
            ?LLOG(warning, "object is already registered", [], State),
            {error, {already_registered, Pid}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
register_obj_id(OldPid, #?STATE{id=Id}) ->
    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path} = Id,
    OptsPid = case is_pid(OldPid) of
        true ->
            #{replace_pid => OldPid};
        false ->
            #{}
    end,
    Opts1 = OptsPid#{sync=>true, meta=>Type},
    case nkdist_reg:register(proc, {nkdomain, SrvId}, ObjId, Opts1) of
        ok ->
            nklib_proc:put(?MODULE, {Type, ObjId, Path}),
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @private
register_domain(#?STATE{id=#obj_id_ext{obj_id = <<"root">>, type = ?DOMAIN_DOMAIN}}=State) ->
    {ok, State};

register_domain(#?STATE{srv_id=SrvId, id=ObjIdExt, domain_id=DomainId}=State) ->
    case sync_op(SrvId, DomainId, {nkdomain_reg_obj, ObjIdExt}) of
        {ok, Enabled, Pid} ->
            ?DEBUG("registered with domain (enabled:~p)", [Enabled], State),
            monitor(process, Pid),
            State2 = do_enabled(State#?STATE{domain_pid=Pid, domain_enabled=Enabled}),
            {ok, State2};
        {error, object_not_found} ->
            {error, {could_not_load_domain, DomainId}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
register_parent(#?STATE{id=#obj_id_ext{obj_id = <<"root">>, type = ?DOMAIN_DOMAIN}}=State) ->
    {ok, State};

register_parent(#?STATE{srv_id=SrvId, id=ObjIdExt, parent_id=ParentId}=State) ->
    case sync_op(SrvId, ParentId, {nkdomain_reg_child, ObjIdExt}) of
        {ok, Enabled, Pid} ->
            ?DEBUG("registered with parent (enabled:~p)", [Enabled], State),
            monitor(process, Pid),
            State2 = do_enabled(State#?STATE{parent_pid=Pid, parent_enabled=Enabled}),
            {ok, State2};
        {error, object_not_found} ->
            {error, {could_not_load_parent, ParentId}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
register_type(#?STATE{id=ObjIdExt, module=Module, type_monitor=OldMon}=State) ->
    nklib_util:demonitor(OldMon),
    {ok, Pid} = nkdomain_type:register(Module, ObjIdExt),
    State#?STATE{type_monitor=monitor(process, Pid)}.


%% @private
set_unload_policy(Obj, Info) ->
    case maps:get(permanent, Info, false) of
        true ->
            permanent;
        false ->
            case maps:get(expires_time, Obj, 0) of
                0 ->
                    {ttl, maps:get(default_ttl, Info, ?DEFAULT_TTL)};
                Expires ->
                    {expires, Expires}
            end
    end.


%% @private
do_save(_Reason, #?STATE{is_dirty=false}=State) ->
    {ok, State};

do_save(Reason, State) ->
    ?DEBUG("save object (~p)", [Reason], State),
    case handle(object_save, [], State) of
        {ok, State2, _Meta} ->
            {ok, do_event(saved, State2#?STATE{is_dirty=false})};
        {error, Error, State2} ->
            ?LLOG(warning, "object save error: ~p", [Error], State),
            {{error, Error}, State2}
    end.


%% @private
do_delete(#?STATE{childs=Childs}=State) when map_size(Childs)==0 ->
    {_, State2} = do_save(pre_delete, State),
    case handle(object_delete, [], State2) of
        {ok, State3, _Meta} ->
            ?DEBUG("object deleted", [], State3),
            {ok, do_event(deleted, State3)};
        {error, object_has_childs, State3} ->
            {{error, object_has_childs}, State3};
        {error, Error, State3} ->
            ?LLOG(warning, "object could not be deleted: ~p", [Error], State3),
            {{error, Error}, State3}
    end;

do_delete(State) ->
    {{error, object_has_childs}, State}.


%% @private
do_refresh(#?STATE{unload_policy={ttl, Time}, timer=Timer}=State) when is_integer(Time) ->
    nklib_util:cancel_timer(Timer),
    Ref = erlang:send_after(Time, self(), nkdomain_timeout),
    State#?STATE{timer=Ref};

do_refresh(State) ->
    State.


%% @private
do_check_expired(#?STATE{unload_policy={expires, Expires}, timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    case nkdomain_util:timestamp() of
        Now when Now >= Expires ->
            true;
        Now ->
            Remind = min(3600000, Expires - Now),
            Ref = erlang:send_after(Remind, self(), nkdomain_check_expire),
            {false, State#?STATE{timer=Ref}}
    end;

do_check_expired(State) ->
    {false, State}.


%% @private
do_stop(Reason, State) ->
    {stop, normal, do_stop2(Reason, State)}.


%% @private
do_stop2(Reason, #?STATE{srv_id=SrvId, stop_reason=false, object_info=Info}=State) ->
    {ok, State2} = handle(object_stop, [Reason], State#?STATE{stop_reason=Reason}),
    {Code, Txt} = nkservice_util:error(SrvId, Reason),
    State3 = do_event({stopped, Code, Txt}, State2),
    {_, State4} = do_save(object_stopped, State3),
    State5 = do_event({unloaded, Reason}, State4),
    case Info of
        #{remove_after_stop:=true} ->
            {_, State6} = do_delete(State5),
            % do_archive(Reason, DeleteState);
            State6;
        _ ->
            State5
    end;

do_stop2(_Reason, State) ->
    State.


%% @private
do_add_child(ObjId, Type, Pid, #?STATE{childs=Childs}=State) ->
    Childs2 = Childs#{ObjId => {Type, Pid}},
    State2 = State#?STATE{childs=Childs2},
    links_add(usage, {child, ObjId, Pid}, State2).


%% @private
do_rm_child(ObjId, #?STATE{childs=Childs}=State) ->
    case maps:find(ObjId, Childs) of
        {ok, {Type, Pid}} ->
            Childs2 = maps:remove(ObjId, Childs),
            State2 = State#?STATE{childs=Childs2},
            State3 = do_event({child_unloaded, Type, ObjId}, State2),
            links_remove(usage, {child, ObjId, Pid}, State3);
        error ->
            State
    end.


%% @private
do_update_name(ObjName, #?STATE{srv_id=SrvId, id=Id, obj=Obj}=State) ->
    #obj_id_ext{type=Type, path=Path1} = Id,
    case nkdomain_util:get_parts(Type, Path1) of
        {ok, _Domain, ObjName} ->
            {ok, State};
        {ok, Domain, _} ->
            Path2 = nkdomain_util:make_path(Domain, Type, ObjName),
            case nkdomain_lib:find(SrvId, Path2) of
                {error, object_not_found} ->
                    Id2 = Id#obj_id_ext{path=Path2},
                    Update = #{
                        obj_name => ObjName,
                        path => Path2,
                        updated_time => nkdomain_util:timestamp(),
                        updated_by => <<>>
                    },
                    Obj2 = ?ADD_TO_OBJ(Update, Obj),
                    State2 = State#?STATE{id=Id2, obj=Obj2, obj_name=ObjName, is_dirty=true},
                    case register_domain(State2) of
                        {ok, State3} ->
                            lager:error("NKLOG UNREGISTER ~p", [Path1]),
                            case do_save(object_updated, State3) of
                                {ok, State4} ->
                                    {ok, State4};
                                {{error, Error}, _} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    ?LLOG(notice, "cannot update name, ~s already exists", [Path2], State),
                    {error, object_already_exists}
            end
    end.


%% @private
do_update(Update, #?STATE{id=Id, obj=Obj}=State) ->
    #obj_id_ext{srv_id=SrvId, type=Type} = Id,
    Update2 = Update#{type=>Type},
    case SrvId:object_parse(SrvId, update, Update2) of
        {ok, Update3, UnknownFields} ->
            case ?ADD_TO_OBJ_DEEP(Update3, Obj) of
                Obj ->
                    {ok, UnknownFields, State};
                Obj3 ->
                    case SrvId:object_parse(SrvId, load, Obj3) of
                        {ok, Obj4, _} ->
                            Time = nkdomain_util:timestamp(),
                            Obj5 = ?ADD_TO_OBJ(updated_time, Time, Obj4),
                            Obj6 = maps:merge(#{updated_by => <<>>}, Obj5),
                            State2 = State#?STATE{obj=Obj6, is_dirty=true},
                            case do_save(object_updated, State2) of
                                {ok, State3} ->
                                    {ok, UnknownFields, do_event({updated, Update3}, State3)};
                                {{error, Error}, State3} ->
                                    {error, Error, State3}
                            end;
                        {error, Error} ->
                            {error, Error, State}
                    end
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @private Sets an enabled state at the object
do_enabled(#?STATE{parent_enabled=Parent, domain_enabled=Domain, obj=Obj}=State) ->
    Status = case Parent andalso Domain of
        true ->
            maps:get(enabled, Obj, true);
        false ->
            false
    end,
    do_enabled(Status, State).


%% @private
do_enabled(Enabled, #?STATE{is_enabled=Enabled}=State) ->
    State;

do_enabled(Enabled, #?STATE{object_info=Info}=State) ->
    case Enabled==false andalso Info of
        #{stop_after_disabled:=true} ->
            async_op(any, self(), {unload, object_is_disabled});
        _ ->
            ok
    end,
    State2 = State#?STATE{is_enabled=Enabled},
    send_childs({nkdomain_parent_enabled, Enabled}, State2),
    {ok, State3} = handle(object_enabled, [Enabled], State2),
    do_event({enabled, Enabled}, State3).


%% @private
do_event(Event, State) ->
    ?DEBUG("sending 'event': ~p", [Event], State),
    nkdomain_obj_util:event(Event, State).


%% @private
do_check_timeout(#?STATE{unload_policy=permanent}=State) ->
    noreply(State);

do_check_timeout(#?STATE{unload_policy={expires, _}}=State) ->
    noreply(State);

do_check_timeout(#?STATE{usage_links=[]}=State) ->
    do_stop(no_usages, State);

do_check_timeout(State) ->
    noreply(do_refresh(State)).

do_nkdist({must_move, Node}, #?STATE{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    case rpc:call(Node, gen_server, start, [?MODULE, {moved, State, self()}, []]) of
        {ok, NewPid} ->
            ?LLOG(info, "starting move to ~p (~p -> ~p)", [Node, self(), NewPid], State),
            erlang:send_after(?MOVE_WAIT_TIME, self(), nkdomain_move_completed),
            noreply(State#?STATE{moved_to=NewPid, obj=#{}});
        {error, Error} ->
            ?LLOG(warning, "could not move process to ~p: ~p", [Node, Error], State),
            noreply(State)
    end;

do_nkdist({vnode_pid, _Pid}, State) ->
    noreply(State);

do_nkdist({updated_reg_pid, _Class, ObjId, Pid}, State) ->
    lager:error("NKLOG UPDATED REG ~s, ~p (I am ~p)", [ObjId, Pid, self()]),
    noreply(State);

do_nkdist(Msg, State) ->
    ?LLOG(notice, "unexpected NkDIST event: ~p", [Msg], State),
    noreply(State).


%% ===================================================================
%% Util
%% ===================================================================


% @private
reply(Reply, #?STATE{}=State) ->
    {reply, Reply, State}.


%% @private
noreply(#?STATE{}=State) ->
    {noreply, State}.


%% @private
handle(Fun, Args, #?STATE{srv_id=SrvId}=State) ->
    apply(SrvId, Fun, Args++[State]).


%% @private
send_childs(Msg, #?STATE{childs=Childs}=State) ->
    lists:foreach(
        fun({_ObjId, {Type, Pid}}) ->
            ?LLOG(notice, "sending ~p to child ~s", [Msg, Type], State),
            gen_server:cast(Pid, Msg)
        end,
        maps:to_list(Childs)).


%% @private
links_add(usage, Link, #?STATE{usage_links=Links}=State) ->
    State#?STATE{usage_links=nklib_links:add(Link, Links)};

links_add(event, Link, #?STATE{event_links=Links}=State) ->
    State#?STATE{event_links=nklib_links:add(Link, Links)}.


%% @private
links_remove(usage, Link, #?STATE{usage_links=Links}=State) ->
    State#?STATE{usage_links=nklib_links:remove(Link, Links)};

links_remove(event, Link, #?STATE{event_links=Links}=State) ->
    State#?STATE{event_links=nklib_links:remove(Link, Links)}.


%% @private
links_down(Mon, #?STATE{usage_links=Usage, event_links=Event}=State) ->
    case nklib_links:down(Mon, Usage) of
        {ok, Link, _Data, Links2} ->
            {usage, Link, State#?STATE{usage_links=Links2}};
        not_found ->
            case nklib_links:down(Mon, Event) of
                {ok, _Link, _Data, Links2} ->
                    {event, State#?STATE{event_links=Links2}};
                not_found ->
                    not_found
            end
    end.


%% @private
links_iter(usage, Fun, Acc, #?STATE{usage_links=Links}) ->
    nklib_links:fold(Fun, Acc, Links);

links_iter(event, Fun, Acc, #?STATE{event_links=Links}) ->
    nklib_links:fold(Fun, Acc, Links).


%% @private
get_timer(#?STATE{timer=Timer}) when is_reference(Timer) ->
    erlang:read_timer(Timer);
get_timer(_) ->
    permanent.
