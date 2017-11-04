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

-export([sync_op/2, sync_op/3, async_op/2]).
-export([start/3, object_deleted/1, conflict_detected/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,  handle_cast/2, handle_info/2]).
-export([links_add/3, links_remove/3, links_iter/4]).
-export([get_all/0, unload_all/0, get_state/1, get_time/1]).
-export([do_update_name/2, do_stop/2]).
-export_type([event/0, status/0]).


-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(DEFAULT_TTL, 10000).
-define(RELOAD_PARENT_TIME, 5000).
-define(MOVE_WAIT_TIME, 30000).
-define(DEF_SYNC_CALL, 5000).
-define(DEFAULT_SAVE_TIME, 5000).

-compile({no_auto_import, [register/2]}).


%% ===================================================================
%% Callbacks definitions
%% ===================================================================

-type state() :: #obj_state{}.


-callback object_info() ->
    object_info().



%% ===================================================================
%% Types
%% ===================================================================


-type object_info() ::
    #{
        type => nkdomain:type(),                        %% Mandatory!
        schema_type => nkdomain_graphql:schema_type(),
        permanent => boolean(),
        save_time => integer(),                         %% secs
        default_ttl => integer(),                       %% msecs
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
%% that the session (using nkdomain_obj_util:send_session_event/2).
%% session_link is also used by some session objects to link to api server
%% (see link_to_session_server and unlink_from_session_server in nkdomain_obj_util)
-type start_opts() ::
    #{
        enabled => boolean(),
        session_events => [binary()],       % See bellow
        session_link => nklib_links:link(), %
        % callback_srv_id => nkservice:id(),
        parent_pid => pid(),
        allow_no_service => boolean(),
        meta => map()
    }.


-type sync_op() ::
    get_obj |
    get_obj_info |
    get_obj_name |
    get_state |
    get_childs|
    get_domain_id |
    get_parent_id |
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
    {nkdomain_reg_child, #obj_id_ext{}} |
    term().


%% ===================================================================
%% Public
%% ===================================================================


%% @private
-spec start(nkdomain:obj(), loaded|created, start_opts()) ->
    {ok, pid()} | {error, term()}.

start(Obj, Op, StartOpts) ->
    case gen_server:start(?MODULE, {Op, Obj, StartOpts}, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_registered, Pid}} -> {ok, Pid};
        {error, Error} -> {error, Error}
    end.


%% @doc
-spec sync_op(nkdomain:id()|pid(), sync_op()) ->
    term() | {error, timeout|process_not_found|object_not_found|term()}.

sync_op(IdOrPid, Op) ->
    sync_op(IdOrPid, Op, ?DEF_SYNC_CALL).


%% @doc
-spec sync_op(nkomain:id()|pid(), sync_op(), timeout()) ->
    term() | {error, timeout|process_not_found|object_not_found|term()}.

sync_op(Pid, Op, Timeout) when is_pid(Pid) ->
    nkservice_util:call(Pid, {nkdomain_sync_op, Op}, Timeout);

sync_op(Id, Op, Timeout) ->
    sync_op(Id, Op, Timeout, 5).


%% @private
sync_op(_Id, _Op, _Timeout, 0) ->
    {error, process_not_found};

sync_op(Id, Op, Timeout, Tries) ->
    case nkdomain_lib:load(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            case sync_op(Pid, Op, Timeout) of
                {error, process_not_found} ->
                    lager:notice("NkDOMAIN SynOP failed, retrying..."),
                    timer:sleep(250),
                    sync_op(Id, Op, Timeout, Tries-1);
                Other ->
                    Other
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec async_op(nkdomain:id()|pid(), async_op()) ->
    ok | {error, process_not_found|object_not_found|term()}.

async_op(Pid, Op) when is_pid(Pid) ->
    gen_server:cast(Pid, {nkdomain_async_op, Op});

async_op(Id, Op) ->
    case nkdomain_lib:load(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            async_op(Pid, Op);
        {error, Error} ->
            {error, Error}
    end.


%% @private Called when the object has been deleted on database
object_deleted(Pid) ->
    gen_server:cast(Pid, nkdomain_obj_deleted).


%% @private
get_state(Id) ->
    sync_op(Id, get_state).


%% @private
get_time(Id) ->
    sync_op(Id, get_time).


%% @private
%% Called from nkdomain_domain_obj after detecting OldPid is in conflict (and will win)
conflict_detected(Pid, WinnerPid) ->
    gen_server:cast(Pid, {nkdomain_obj_conflict_detected, WinnerPid}).


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
        fun({_Type, _ObjId, _Path, Pid}) -> async_op(Pid, {unload, normal}) end,
        get_all()).


% ===================================================================
%% gen_server behaviour
%% ===================================================================


%% @private
-spec init(term()) ->
    {ok, state()} | {error, term()}.

init({Op, Obj, StartOpts}) when Op==loaded; Op==created ->
    case do_init(Op, Obj, StartOpts) of
        {ok, State1} ->
            ?DEBUG("started (~p)", [self()], State1),
            % Register obj_id, domain, parent and type
            case do_register(StartOpts, State1) of
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
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {noreply, state()} | {reply, term(), state()} |
    {stop, Reason::term(), state()} | {stop, Reason::term(), Reply::term(), state()}.

handle_call({nkdomain_sync_op, Op}, From, State) ->
    case handle(object_sync_op, [Op, From], State) of
        {reply, Reply, #obj_state{}=State2} ->
            reply(Reply, do_save_timer(do_refresh(State2)));
        {reply_and_save, Reply, #obj_state{}=State2} ->
            {_, State3} = do_save(user_op, State2#obj_state{is_dirty=true}),
            reply(Reply, do_refresh(State3));
        {noreply, #obj_state{}=State2} ->
            noreply(do_save_timer(do_refresh(State2)));
        {noreply_and_save, #obj_state{}=State2} ->
            {_, State3} = do_save(user_op, State2#obj_state{is_dirty=true}),
            noreply(do_refresh(State3));
        {stop, Reason, Reply, #obj_state{}=State2} ->
            gen_server:reply(From, Reply),
            do_stop(Reason, State2);
        {stop, Reason, #obj_state{}=State2} ->
            do_stop(Reason, State2);
        continue ->
            do_sync_op(Op, From, State);
        {continue, [Op2, _From2, #obj_state{}=State2]} ->
            do_sync_op(Op2, From, State2);
        Other ->
            ?LLOG(error, "invalid response for sync op ~p: ~p", [Op, Other], State),
            error(invalid_sync_response)
    end;

handle_call(Msg, From, State) ->
    safe_handle(object_handle_call, [Msg, From], State).


%% @private
-spec handle_cast(term(), state()) ->
    {noreply, state()} | {stop, term(), state()}.

%%handle_cast(Msg, #obj_state{moved_to=Pid}=State) when is_pid(Pid) ->
%%    gen_server:cast(Pid, Msg),
%%    noreply(State);

handle_cast({nkdomain_async_op, Op}, State) ->
    case handle(object_async_op, [Op], State) of
        {noreply, #obj_state{}=State2} ->
            noreply(do_save_timer(do_refresh(State2)));
        {noreply_and_save, #obj_state{}=State2} ->
            {_, State3} = do_save(user_op, State2#obj_state{is_dirty=true}),
            noreply(do_refresh(State3));
        {stop, Reason, #obj_state{}=State2} ->
            do_stop(Reason, State2);
        continue ->
            do_async_op(Op, State);
        {continue, [Op2, #obj_state{}=State2]} ->
            do_async_op(Op2, State2);
        Other ->
            ?LLOG(error, "invalid response for async op ~p: ~p", [Op, Other], State),
            error(invalid_async_response)
    end;

handle_cast({nkdomain_parent_enabled, Enabled}, State) ->
    noreply(do_enabled(State#obj_state{parent_enabled=Enabled}));

handle_cast({nkdomain_domain_enabled, Enabled}, State) ->
    noreply(do_enabled(State#obj_state{domain_enabled=Enabled}));

handle_cast(nkdomain_service_stopped, State) ->
    ?LLOG(warning, "service stopped", [], State),
    do_stop(service_down, State);

handle_cast(nkdomain_obj_deleted, State) ->
    do_stop(object_deleted, State#obj_state{is_dirty=false});

handle_cast({nkdomain_obj_conflict_detected, WinnerPid}, State) ->
    #obj_state{id=#obj_id_ext{type=Type}} = State,
    handle(object_conflict_detected, [Type, WinnerPid], State);

handle_cast(Msg, State) ->
    safe_handle(object_handle_cast, [Msg], State).


%% @private
-spec handle_info(term(), state()) ->
    {noreply, state()} | {stop, term(), state()}.

handle_info({nkservice_updated, _SrvId}, #obj_state{id=#obj_id_ext{type=Type}}=State) ->
    set_log(Type),
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

handle_info(nkdomain_find_parent, #obj_state{parent_pid=undefined}=State) ->
    case register_parent(State, #{}) of
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

handle_info(nkdomain_find_domain, #obj_state{domain_pid=undefined}=State) ->
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

handle_info(nkdomain_obj_next_status_timer, State) ->
    State2 = State#obj_state{next_status_timer=undefined},
    {ok, State3} = handle(object_next_status_timer, [], State2),
    {noreply, State3};

handle_info(nkdomain_obj_save_timer, #obj_state{is_dirty=true}=State) ->
    ?LLOG(info, "timer save", [], State),
    {_, State2} = do_save(timer, State),
    {noreply, State2};

handle_info(nkdomain_obj_save_timer, State) ->
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #obj_state{parent_pid=Pid}=State) ->
    ?LLOG(notice, "parent has failed", [], State),
    self() ! nkdomain_find_parent,
    State2 = do_save(parent_down, State),
    State3 = do_enabled(State2#obj_state{parent_pid=undefined, parent_enabled=false}),
    noreply(State3);

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #obj_state{domain_pid=Pid}=State) ->
    ?LLOG(notice, "domain has failed", [], State),
    self() ! nkdomain_find_domain,
    State2 = do_save(domain_down, State),
    State3 = do_enabled(State2#obj_state{domain_pid=undefined, domain_enabled=false}),
    noreply(State3);

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
            safe_handle(object_handle_info, [Info], State)
    end;

handle_info(Msg, State) ->
    safe_handle(object_handle_info, [Msg], State).


%% @private
-spec code_change(term(), state(), term()) ->
    {ok, state()}.

code_change(OldVsn, State, Extra) ->
    nkdomain_obj_util:obj_apply(object_code_change, [OldVsn, State, Extra], State).


%% @private
-spec terminate(term(), state()) ->
    ok.

%%terminate(_Reason, #obj_state{moved_to=Pid}) when is_pid(Pid) ->
%%    ok;

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
    #obj_state{
        module = Module,
        started = Started,
        is_enabled = Enabled,
        obj = Obj
    } = State,
    Obj2 = Obj#{
        '_module' => Module,
        '_started' => Started,
        '_is_enabled' => Enabled
    },
    reply({ok, Obj2}, State);

do_sync_op(get_state, _From, State) ->
    reply({ok, State}, State);

do_sync_op(get_childs, _From, #obj_state{childs=Childs}=State) ->
    reply({ok, Childs}, State);

do_sync_op(get_domain_id, _From, #obj_state{domain_id=DomainId}=State) ->
    reply({ok, DomainId}, State);

do_sync_op(get_parent_id, _From, #obj_state{parent_id=ParentId}=State) ->
    reply({ok, ParentId}, State);

do_sync_op(get_path, _From, #obj_state{id=#obj_id_ext{path=Path}}=State) ->
    reply({ok, Path}, State);

do_sync_op(save, _From, State) ->
    {Reply, State2} = do_save(user_order, State),
    reply(Reply, State2);

do_sync_op(delete, From, #obj_state{is_enabled=IsEnabled, object_info=Info}=State) ->
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

do_sync_op({enable, Enable}, From, #obj_state{obj=Obj}=State) ->
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

do_sync_op(is_enabled, _From, #obj_state{is_enabled=IsEnabled}=State) ->
    reply({ok, IsEnabled}, State);

do_sync_op({register, Type, Link}, _From, State) ->
    {reply, ok, links_add(Type, Link, State)};

do_sync_op({update, Map}, _From, #obj_state{is_enabled=IsEnabled, object_info=Info}=State) ->
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

do_sync_op({update_name, _ObjName}, _From, #obj_state{id=#obj_id_ext{type=?DOMAIN_DOMAIN}}=State) ->
    reply({error, domains_name_cannot_change}, State);

do_sync_op({update_name, ObjName}, _From, #obj_state{obj=#{obj_name:=ObjName}}=State) ->
    lager:error("NKLOG NO CHANGE"),
    reply({ok, ObjName}, do_refresh(State));

do_sync_op({update_name, ObjName}, _From, #obj_state{is_enabled=IsEnabled, object_info=Info}=State) ->
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

do_sync_op(Op, _From, State) ->
    ?LLOG(notice, "unknown sync op: ~p", [Op], State),
    reply({error, unknown_op}, State).


%% @private
do_async_op({nkdomain_reg_child, ObjIdExt}, State) ->
    #obj_id_ext{obj_id=ObjId, type=Type, path=Path, pid=Pid} = ObjIdExt,
    #obj_state{is_enabled=IsEnabled} = State,
    ?DEBUG("registering child ~s", [Path], State),
    State2 = do_rm_child(ObjId, State),
    State3 = do_add_child(ObjId, Type, Pid, State2),
    State4 = do_event({child_loaded, Type, ObjId, Pid}, State3),
    case IsEnabled of
        false ->
            gen_server:cast(Pid, {nkdomain_parent_enabled, false});
        true ->
            ok
    end,
    noreply(State4);

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

do_init(Op, Obj, StartOpts) ->
    #{
        type := Type,
        obj_id := ObjId,
        path := Path,
        obj_name := ObjName,
        domain_id := DomainId,
        parent_id := ParentId
    } = Obj,
    Module = nkdomain_lib:get_module(Type),
    false = Module==undefined,
    Enabled = case maps:find(enabled, StartOpts) of
        {ok, false} ->
            false;
        _ ->
            maps:get(enabled, Obj, true)
    end,
    Info = Module:object_info(),
    ObjIdExt = #obj_id_ext{obj_id=ObjId, type=Type, path=Path, obj_name=ObjName, pid=self()},
    Now = nkdomain_util:timestamp(),
    NextStatusTimer = case Obj of
        #{next_status_time:=NextStatusTime} ->
            case NextStatusTime-Now of
                Step when Step=<0 ->
                    self() ! nkdomain_obj_next_status_timer,
                    undefined;
                Step ->
                    erlang:send_after(Step, self(), nkdomain_obj_next_status_timer)
            end;
        _ ->
            undefined
    end,
    UnloadPolicy = set_unload_policy(Obj, Info),
    % If expired, do proper delete
    case UnloadPolicy of
        {expires, _} ->
            self() ! nkdomain_check_expire;
        _ ->
            ok
    end,
    set_log(Type),
    nkservice_util:register_for_changes(?NKROOT),
    State = #obj_state{
        id = ObjIdExt,
        module = Module,
        domain_id = DomainId,
        parent_id = ParentId,
        object_info = Info,
        obj = Obj,
        is_dirty = (Op==created),
        is_enabled = Enabled,
        started = Now,
        childs = #{},
        usage_links = nklib_links:new(),
        event_links = nklib_links:new(),
        session_events = maps:get(session_events, StartOpts, []),
        session_link = maps:get(session_link, StartOpts, undefined),
        meta = maps:get(meta, StartOpts, #{}),
        unload_policy = UnloadPolicy,
        domain_enabled = true,
        parent_enabled = true,
        session = #{},
        next_status_timer=NextStatusTimer
    },
    load_srv_id(Obj, StartOpts, State).


%% @private
set_log(Type) ->
    Debug =
        case nkservice_util:get_debug_info(?NKROOT, ?MODULE) of
            {true, #{types:=all}} -> true;
            {true, #{types:=Types}} -> lists:member(Type, Types);
            _ -> false
        end,
    % lager:notice("DEBUG: ~p", [Debug]),
    put(object_debug, Debug).


%% @private
load_srv_id(#{srv_id:=SrvId}, Opts, State) ->
    case SrvId of
        <<>> ->
            {ok, State#obj_state{effective_srv_id=?NKROOT}};
        ?NKROOT ->
            {ok, State#obj_state{effective_srv_id=?NKROOT}};
        _ ->
            Allow = maps:get(allow_not_service, Opts, false),
            case catch nklib_util:to_existing_atom(SrvId) of
                {'EXIT', _} when Allow ->
                    ?LLOG(notice, "loading object with unknown service '~s'", [SrvId], State),
                    {ok, State#obj_state{effective_srv_id=?NKROOT}};
                {'EXIT', _} ->
                    {error, {srv_id_invalid, SrvId}};
                SrvId2 ->
                    case whereis(SrvId2) of
                        Pid when is_pid(Pid) ->
                            {ok, State#obj_state{effective_srv_id=SrvId2}};
                        undefined when Allow ->
                            ?LLOG(notice, "loading object with unknown service '~s'", [SrvId], State),
                            {ok, State#obj_state{effective_srv_id=?NKROOT}};
                        undefined ->
                            {error, {srv_id_invalid, SrvId}}
                    end
            end
    end;

load_srv_id(_Obj, _Opts, State) ->
    {ok, State#obj_state{effective_srv_id=?NKROOT}}.


%% @private
do_register(Opts, State) ->
    case register_domain(State) of
        {ok, State2} ->
            case register_parent(State2, Opts) of
                {ok, State3} ->
                    #obj_state{id=#obj_id_ext{type=Type, obj_id=ObjId, path=Path}} = State,
                    nklib_proc:put(?MODULE, {Type, ObjId, Path}),
                    {ok, State3};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
register_domain(#obj_state{id=#obj_id_ext{obj_id = <<"root">>, type = ?DOMAIN_DOMAIN}}=State) ->
    {ok, State};

register_domain(#obj_state{id=ObjIdExt, domain_id=DomainId, effective_srv_id=SrvId}=State) ->
    % Sync operation, domain can deny our loading
    case sync_op(DomainId, {nkdomain_reg_obj, ObjIdExt}) of
        {ok, DomSrvId, Enabled, Pid} ->
            ?DEBUG("registered with domain (enabled:~p)", [Enabled], State),
            monitor(process, Pid),
            % If the object has a srv_id that is not ?NKROOT, it is honored
            % If not, we will take domain's srv_id
            SrvId2 = case SrvId /= ?NKROOT of
                true ->
                    SrvId;
                false ->
                    DomSrvId
             end,
            State2 = do_enabled(State#obj_state{effective_srv_id=SrvId2, domain_pid=Pid, domain_enabled=Enabled}),
            {ok, State2};
        {error, object_not_found} ->
            ?LLOG(warning, "cannot load domain ~s: not found", [DomainId], State),
            {error, {could_not_load_domain, DomainId}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
register_parent(#obj_state{id=#obj_id_ext{obj_id = <<"root">>, type = ?DOMAIN_DOMAIN}}=State, _Opts) ->
    {ok, State};

register_parent(#obj_state{id=ObjIdExt, parent_id=ParentId}=State, #{parent_pid:=ParentPid}) ->
    async_op(ParentPid, {nkdomain_reg_child, ObjIdExt}),
    ?DEBUG("registered with parent ~s", [ParentId], State),
    monitor(process, ParentPid),
    % We will receive the parent enabled event
    State2 = State#obj_state{parent_pid=ParentPid, parent_enabled=true},
    {ok, State2};

register_parent(#obj_state{parent_id=ParentId}=State, Opts) ->
    case nkdomain_lib:load(ParentId) of
        #obj_id_ext{pid=ParentPid} ->
            register_parent(State, Opts#{parent_pid=>ParentPid});
        {error, object_not_found} ->
            {error, {could_not_load_parent, ParentId}};
        {error, Error} ->
            {error, Error}
    end.


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
do_save(_Reason, #obj_state{is_dirty=false}=State) ->
    {ok, State};

do_save(Reason, State) ->
    ?DEBUG("save object (~p)", [Reason], State),
    case handle(object_save, [], State) of
        {ok, State2, _Meta} ->
            #obj_state{save_timer=Timer} = State,
            nklib_util:cancel_timer(Timer),
            {ok, do_event(saved, State2#obj_state{is_dirty=false, save_timer=undefined})};
        {error, Error, State2} ->
            ?LLOG(warning, "object save error: ~p", [Error], State),
            {{error, Error}, State2}
    end.


%% @private
do_delete(#obj_state{childs=Childs}=State) when map_size(Childs)==0 ->
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
do_refresh(#obj_state{unload_policy={ttl, Time}, timer=Timer}=State) when is_integer(Time) ->
    nklib_util:cancel_timer(Timer),
    Ref = erlang:send_after(Time, self(), nkdomain_timeout),
    State#obj_state{timer=Ref};

do_refresh(State) ->
    State.


%% @private
do_save_timer(#obj_state{is_dirty=true, object_info=Info, save_timer=undefined}=State) ->
    Time = 1000 * maps:get(save_time, Info, ?DEFAULT_SAVE_TIME),
    Ref = erlang:send_after(Time, self(), nkdomain_obj_save_timer),
    State#obj_state{timer=Ref};

do_save_timer(State) ->
    State.


%% @private
do_check_expired(#obj_state{unload_policy={expires, Expires}, timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    case nkdomain_util:timestamp() of
        Now when Now >= Expires ->
            true;
        Now ->
            Remind = min(3600000, Expires - Now),
            Ref = erlang:send_after(Remind, self(), nkdomain_check_expire),
            {false, State#obj_state{timer=Ref}}
    end;

do_check_expired(State) ->
    {false, State}.


%% @private
do_stop(Reason, State) ->
    {stop, normal, do_stop2(Reason, State)}.


%% @private
do_stop2(Reason, #obj_state{stop_reason=false, object_info=Info}=State) ->
    {ok, State2} = handle(object_stop, [Reason], State#obj_state{stop_reason=Reason}),
    {Code, Txt} = nkdomain_obj_util:obj_error(Reason, State),
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
do_add_child(ObjId, Type, Pid, #obj_state{childs=Childs}=State) ->
    Childs2 = Childs#{ObjId => {Type, Pid}},
    State2 = State#obj_state{childs=Childs2},
    links_add(usage, {child, ObjId, Pid}, State2).


%% @private
do_rm_child(ObjId, #obj_state{childs=Childs}=State) ->
    case maps:find(ObjId, Childs) of
        {ok, {Type, Pid}} ->
            Childs2 = maps:remove(ObjId, Childs),
            State2 = State#obj_state{childs=Childs2},
            State3 = do_event({child_unloaded, Type, ObjId}, State2),
            links_remove(usage, {child, ObjId, Pid}, State3);
        error ->
            State
    end.


%% @private
do_update_name(ObjName, #obj_state{id=Id, obj=Obj}=State) ->
    #obj_id_ext{type=Type, path=Path1} = Id,
    case nkdomain_util:get_parts(Type, Path1) of
        {ok, _Domain, ObjName} ->
            {ok, State};
        {ok, Domain, _} ->
            Path2 = nkdomain_util:make_path(Domain, Type, ObjName),
            case nkdomain_lib:find(Path2) of
                {error, object_not_found} ->
                    Id2 = Id#obj_id_ext{path=Path2, obj_name=ObjName},
                    Update = #{
                        obj_name => ObjName,
                        path => Path2,
                        updated_time => nkdomain_util:timestamp(),
                        updated_by => <<>>
                    },
                    Obj2 = ?ADD_TO_OBJ(Update, Obj),
                    State2 = State#obj_state{id=Id2, obj=Obj2, is_dirty=true},
                    case register_domain(State2) of
                        {ok, State3} ->
                            case do_save(object_updated, State3) of
                                {ok, State4} ->
                                    {ok, State4};
                                {{error, Error}, _} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                #obj_id_ext{} ->
                    ?LLOG(notice, "cannot update name, ~s already exists", [Path2], State),
                    {error, object_already_exists};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private
do_update(Update, #obj_state{id=Id, obj=Obj}=State) ->
    #obj_id_ext{type=Type} = Id,
    Update2 = Update#{type=>Type},
    case ?CALL_NKROOT(object_parse, [update, Update2]) of
        {ok, Update3, UnknownFields} ->
            case ?ADD_TO_OBJ_DEEP(Update3, Obj) of
                Obj ->
                    {ok, UnknownFields, State};
                Obj3 ->
                    case ?CALL_NKROOT(object_parse, [load, Obj3]) of
                        {ok, Obj4, _} ->
                            case ?CALL_NKROOT(object_update, [Obj4]) of
                                {ok, Obj5} ->
                                    Time = nkdomain_util:timestamp(),
                                    Obj6 = ?ADD_TO_OBJ(updated_time, Time, Obj5),
                                    Obj7 = maps:merge(#{updated_by => <<>>}, Obj6),
                                    State2 = State#obj_state{obj=Obj7, is_dirty=true},
                                    case do_save(object_updated, State2) of
                                        {ok, State3} ->
                                            {ok, UnknownFields, do_event({updated, Update3}, State3)};
                                        {{error, Error}, State3} ->
                                            {error, Error, State3}
                                    end;
                                {error, Error} ->
                                    {error, Error, State}
                            end;
                        {error, Error} ->
                            {error, Error, State}
                    end
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @private Sets an enabled state at the object
do_enabled(#obj_state{parent_enabled=Parent, domain_enabled=Domain, obj=Obj}=State) ->
    Status = case Parent andalso Domain of
        true ->
            maps:get(enabled, Obj, true);
        false ->
            false
    end,
    do_enabled(Status, State).


%% @private
do_enabled(Enabled, #obj_state{is_enabled=Enabled}=State) ->
    State;

do_enabled(Enabled, #obj_state{object_info=Info}=State) ->
    case Enabled==false andalso Info of
        #{stop_after_disabled:=true} ->
            async_op(self(), {unload, object_is_disabled});
        _ ->
            ok
    end,
    State2 = State#obj_state{is_enabled=Enabled},
    send_childs({nkdomain_parent_enabled, Enabled}, State2),
    {ok, State3} = handle(object_enabled, [Enabled], State2),
    do_event({enabled, Enabled}, State3).


%% @private
do_event(Event, State) ->
    ?DEBUG("sending 'event': ~p", [Event], State),
    nkdomain_obj_util:event(Event, State).


%% @private
do_check_timeout(#obj_state{unload_policy=permanent}=State) ->
    noreply(State);

do_check_timeout(#obj_state{unload_policy={expires, _}}=State) ->
    noreply(State);

do_check_timeout(#obj_state{usage_links=[], next_status_timer=undefined}=State) ->
    do_stop(no_usages, State);

do_check_timeout(State) ->
    noreply(do_refresh(State)).


%% ===================================================================
%% Util
%% ===================================================================


% @private
reply(Reply, #obj_state{}=State) ->
    {reply, Reply, State}.


%% @private
noreply(#obj_state{}=State) ->
    {noreply, State}.


%% @private
%% Will call the service's functions
handle(Fun, Args, State) ->
    nkdomain_obj_util:obj_apply(Fun, Args++[State], State).


%% @private
safe_handle(Fun, Args, State) ->
    Reply = handle(Fun, Args, State),
    case Reply of
        {reply, _, #obj_state{}} ->
            Reply;
        {reply, _, #obj_state{}, _} ->
            Reply;
        {noreply, #obj_state{}} ->
            Reply;
        {noreply, #obj_state{}, _} ->
            Reply;
        {stop, _, _, #obj_state{}} ->
            Reply;
        {stop, _, #obj_state{}} ->
            Reply;
        Other ->
            ?LLOG(error, "invalid response for ~p(~p): ~p", [Fun, Args, Other], State),
            error(invalid_handle_response)
    end.


%% @private
send_childs(Msg, #obj_state{childs=Childs}=State) ->
    lists:foreach(
        fun({_ObjId, {Type, Pid}}) ->
            ?DEBUG("sending ~p to child ~s", [Msg, Type], State),
            gen_server:cast(Pid, Msg)
        end,
        maps:to_list(Childs)).


%% @private
links_add(usage, Link, #obj_state{usage_links=Links}=State) ->
    State#obj_state{usage_links=nklib_links:add(Link, Links)};

links_add(event, Link, #obj_state{event_links=Links}=State) ->
    State#obj_state{event_links=nklib_links:add(Link, Links)}.


%% @private
links_remove(usage, Link, #obj_state{usage_links=Links}=State) ->
    State#obj_state{usage_links=nklib_links:remove(Link, Links)};

links_remove(event, Link, #obj_state{event_links=Links}=State) ->
    State#obj_state{event_links=nklib_links:remove(Link, Links)}.


%% @private
links_down(Mon, #obj_state{usage_links=Usage, event_links=Event}=State) ->
    case nklib_links:down(Mon, Usage) of
        {ok, Link, _Data, Links2} ->
            {usage, Link, State#obj_state{usage_links=Links2}};
        not_found ->
            case nklib_links:down(Mon, Event) of
                {ok, _Link, _Data, Links2} ->
                    {event, State#obj_state{event_links=Links2}};
                not_found ->
                    not_found
            end
    end.


%% @private
links_iter(usage, Fun, Acc, #obj_state{usage_links=Links}) ->
    nklib_links:fold(Fun, Acc, Links);

links_iter(event, Fun, Acc, #obj_state{event_links=Links}) ->
    nklib_links:fold(Fun, Acc, Links).
