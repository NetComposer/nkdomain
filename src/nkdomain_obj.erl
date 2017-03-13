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
%% Object stop:
%% - parent stops
%% - all of my childs and usages stop


-module(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([find/2, load/3, create/3, get_session/1, save/1, stop/2, sync_op/2, async_op/2]).
-export([set_enabled/2, remove/2, register/2, unregister/2]).
-export([do_find/1, do_call/2, do_call/3, do_cast/2, do_info/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([get_all/0, stop_all/0]).
-export_type([event/0, status/0]).


-define(DEBUG(Txt, Args, State),
    case erlang:get(object_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, State),
    lager:Type(
        [
            {obj_id, State#state.obj_id},
            {module, (State#state.session)#obj_session.module},
            {path, (State#state.session)#obj_session.path}
        ],
        "NkDOMAIN Obj (~s:~s, ~s) "++Txt,
        [
            (State#state.session)#obj_session.module,
            State#state.obj_id,
            (State#state.session)#obj_session.path
            | Args]
        )).

-define(MIN_STARTED_TIME, 2000).
-define(DEF_SYNC_CALL, 5000).
-define(SAVE_RETRY, 5000).
-define(MAX_SAVE_RETRIES, 10).

-include("nkdomain.hrl").
-compile({no_auto_import, [register/2]}).

%% ===================================================================
%% Callbacks definitions
%% ===================================================================

-callback object_get_desc() ->
    type_desc().


-callback object_get_mapping() ->
    map().


-callback object_add_syntax(nklib_syntax:syntax()) ->
    nklib_syntax:syntax().


-callback object_store(nkdomain:object()) ->
    map().


%% ===================================================================
%% Types
%% ===================================================================


-type id() ::
    nkdomain:obj_id() | {nkservice:id(), nkdomain:path()} | pid().

-type type_desc() ::
    map().

-type create_opts() ::
    #{
        register => nklib:link(),
        user_id => nkdomain:obj_id(),
        user_session => nkservice:user_session(),
        events => [nkservice_events:type()],
        enabled => boolean()                        % Start disabled
    }.

-type load_opts() ::
    #{
        register => nklib:link()
    }.

-type session() :: #obj_session{}.

-type info() :: atom().

-type event() ::
    created |
    {updated, map()} |
    {enabled, boolean()} |
    {stopped, nkservice:error()} |
    {info, info(), map()} |
    destroyed.

-type apply_fun() ::
    fun((session()) -> Reply::term() | {Reply::term(), session()}).

-type op() ::
    {set_enabled, boolean()} |
    {apply, apply_fun()} |
    {remove, Reason::nkservice:error()}.

-type status() ::
    init |
    {stopped, Reason::term()} |
    term().


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, module(), domain:obj_id(), nkdomain:path(), pid()|undefined} |
    {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkdomain_util:is_path(IdOrPath) of
        {true, Path} ->
            case nkservice_srv:get_srv_id(Srv) of
                {ok, SrvId} ->
                    case SrvId:object_store_find_path(SrvId, Path) of
                        {ok, Module, ObjId} when is_binary(ObjId) ->
                            case do_find(ObjId) of
                                {ok, Module, Path, Pid} ->
                                    {ok, Module, ObjId, Path, Pid};
                                not_found ->
                                    {ok, Module, ObjId, Path, undefined}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                not_found ->
                    {error, service_not_found}
            end;
        false ->
            case do_find(IdOrPath) of
                {ok, Module, Path, Pid} ->
                    {ok, Module, IdOrPath, Path, Pid};
                not_found ->
                    {error, object_not_found}
            end
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path(), load_opts()) ->
    {ok, module(), nkdomain:obj_id(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, Id, Meta) ->
    case find(Srv, Id) of
        {ok, Module, ObjId, _Path, Pid} when is_pid(Pid) ->
            case Meta of
                #{register:=Link} ->
                    register(Pid, Link);
                _ ->
                    ok
            end,
            {ok, Module, ObjId, Pid};
        {ok, _Module, ObjId, _Path, undefined} ->
            do_load1(Srv, ObjId, Meta);
        {error, object_not_found} ->
            do_load1(Srv, Id, Meta)
    end.


%% @private
do_load1(Srv, ObjId, Meta) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            Meta2 = Meta#{
                srv_id => SrvId,
                is_dirty => false
            },
            case SrvId:object_load(SrvId, ObjId) of
                {ok, #{module:=Module}=Obj} ->
                    case Obj of
                        #{expires_time:=Expires} ->
                            case nklib_util:m_timestamp() of
                                Now when Now >= Expires ->
                                    SrvId:object_store_remove_raw(SrvId, ObjId),
                                    {error, object_not_found};
                                _ ->
                                    do_load2(Module, ObjId, Obj, Meta2)
                            end;
                        _ ->
                            do_load2(Module, ObjId, Obj, Meta2)
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private
do_load2(Module, ObjId, Obj, Meta2) ->
    {ok, ObjPid} = gen_server:start(?MODULE, {Obj, Meta2}, []),
    {ok, Module, ObjId, ObjPid}.


%% @doc Creates a new object
-spec create(nkservice:id(), nkdomain:obj(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()}.

create(Srv, #{module:=Module, obj_id:=ObjId}=Obj, Meta) ->
    case load(Srv, ObjId, #{}) of
        {error, object_not_found} ->
            case catch Module:object_get_desc() of
                Desc when is_map(Desc) ->
                    case nkservice_srv:get_srv_id(Srv) of
                        {ok, SrvId} ->
                            do_create(SrvId, Obj, Meta);
                        not_found ->
                            {error, service_not_found}
                    end;
                _ ->
                    {error, invalid_module}
            end;
        _ ->
            {error, object_already_exists}
    end.


%% @private
do_create(SrvId, #{module:=Module, obj_id:=ObjId}=Obj, Meta) ->
    {ObjId, Obj2} = nkmedia_util:add_id(obj_id, Obj, Module),
    Obj3 = Obj2#{
        created_time => nklib_util:m_timestamp()
    },
    case SrvId:object_parse(SrvId, Module, Obj3) of
        {ok, Obj4} ->
            case do_create_check_parent(SrvId, Obj4) of
                {ok, ParentMeta} ->
                    Meta2 = Meta#{
                        srv_id => SrvId,
                        is_dirty => true
                    },
                    Meta3 = maps:merge(Meta2, ParentMeta),
                    {ok, ObjPid} = gen_server:start(?MODULE, {Obj4, Meta3}, []),
                    {ok, ObjId, ObjPid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_create_check_parent(_SrvId, #{parent_id:=<<>>, obj_id:=<<"root">>}) ->
    {ok, #{}};

do_create_check_parent(SrvId, #{parent_id:=ParentId, path:=Path}) ->
    case load(SrvId, ParentId, #{}) of
        {ok, Module, _ObjId, Pid} ->
            case do_call(Pid, {nkdomain_check_child, Module, Path}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Error} ->
                    {error, Error}
            end;
        {error, _Error} ->
            {error, parent_error}
    end.


%% @doc
-spec get_session(id()) ->
    {ok, session()} | {error, term()}.

get_session(Id) ->
    do_call(Id, nkdomain_get_session).


%% @doc
-spec save(id()) ->
    ok | {error, term()}.

save(Id) ->
    do_cast(Id, nkdomain_save).


%% @doc
-spec remove(id(), nkservice:error()) ->
    ok | {error, term()}.

remove(Id, Reason) ->
    do_cast(Id, {nkdomain_remove, Reason}).


%% @doc
-spec stop(id(), nkservice:error()) ->
    ok | {error, term()}.

stop(Id, Reason) ->
    do_cast(Id, {nkdomain_stop, Reason}).


%% @doc
-spec set_enabled(id(), boolean()) ->
    ok | {error, term()}.

set_enabled(Id, Enabled) when is_boolean(Enabled)->
    async_op(Id, {set_enabled, Enabled}).


%% @doc
-spec sync_op(id(), op()) ->
    {ok, term()} | {error, term()}.

sync_op(Id, Op) ->
    do_call(Id, {nkdomain_sync_op, Op}).


%% @doc
-spec async_op(id(), op()) ->
    ok | {error, term()}.

async_op(Id, Op) ->
    do_cast(Id, {nkdomain_async_op, Op}).


% @doc
-spec register(id(), nklib:link()) ->
    ok | {error, term()}.

register(Id, Link) ->
    do_cast(Id, {nkdomain_register, Link}).


% @doc
-spec unregister(id(), nklib:link()) ->
    ok | {error, term()}.

unregister(Id, Link) ->
    do_cast(Id, {nkdomain_unregister, Link}).



%% @doc
-spec get_all() ->
    [{module(), nkdomain:obj_id(), nkdomain:path(), pid()}].

get_all() ->
    [
        {Module, ObjId, Path, Pid} ||
        {{ObjId, Module, Path}, Pid} <- nklib_proc:values(?MODULE)
    ].

%% @private
stop_all() ->
    lists:foreach(fun({_Module, _ObjId, _Path, Pid}) -> stop(Pid, normal) end, get_all()).


% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(child, {
    module :: module(),
    obj_id :: nkdomain:obj_id(),
    name :: binary(),
    pid :: pid()
}).

-record(state, {
    obj_id :: nkdomain:obj_id(),
    parent_pid :: pid(),
    srv_id :: nkservice:id(),
    stop_reason = false :: false | nkservice:error(),
    links :: nklib_links:links(),
    session :: session(),
    started :: nklib_util:m_timestamp(),
    timer :: reference(),
    childs = [] :: [#child{}],
    save_op = save  :: save | remove | archive,
    save_tries = 0 :: integer(),
    timelog = [] :: [map()]
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init({Obj, Meta}) ->
    #{obj_id:=ObjId, module:=Module, path:=Path, parent_id:=Parent} = Obj,
    true = nklib_proc:reg({?MODULE, ObjId}, {Module, Path}),
    nklib_proc:put(?MODULE, {ObjId, Module, Path}),
    #{srv_id:=SrvId, is_dirty:=IsDirty} = Meta,
    Enabled = case maps:find(enabled, Meta) of
        {ok, true} ->
            case maps:get(enabled, Obj, true) of
                true -> true;
                false -> false
            end;
        {ok, false} ->
            false;
        error ->
            maps:get(enabled, Obj, true)
    end,
    Session = #obj_session{
        obj_id = ObjId,
        module = Module,
        path = Path,
        type = maps:get(type, Obj),
        parent_id = Parent,
        obj = Obj,
        srv_id = SrvId,
        status = init,
        meta = maps:without([srv_id, is_dirty], Meta),
        is_dirty = IsDirty,
        enabled = Enabled
    },
    State1 = #state{
        obj_id = ObjId,
        srv_id = SrvId,
        links = nklib_links:new(),
        session = Session,
        started = nklib_util:m_timestamp()
    },
    State2 = case Meta of
         #{register:=Link} ->
             links_add(Link, State1);
         _ ->
             State1
    end,
    set_log(State2),
    nkservice_util:register_for_changes(SrvId),
    ?LLOG(info, "loaded (~p)", [self()], State2),
    gen_server:cast(self(), nkdomain_do_start),
    {ok, State3} = handle(object_init, [], State2),
    {ok, do_event(created, State3)}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(nkdomain_get_session, _From, #state{session=Session}=State) ->
    reply({ok, Session}, State);

handle_call(nkdomain_get_timelog, _From, #state{timelog=Log}=State) ->
    reply({ok, Log}, State);

handle_call(nkdomain_get_state, _From, State) ->
    reply(State, State);

handle_call({nkdomain_sync_op, Op}, From, State) ->
    case handle(object_sync_op, [Op, From], State) of
        {reply, Reply, State2} ->
            reply(Reply, State2);
        {noreply, State2} ->
            noreply(State2);
        {stop, Reason, Reply, State2} ->
            gen_server:reply(From, Reply),
            do_stop(Reason, State2);
        {stop, Reason, State2} ->
            do_stop(Reason, State2);
        {continue, State2} ->
            do_sync_op(Op, From, State2)
    end;

handle_call({nkdomain_check_child, ObjModule, ObjPath}, _From, State) ->
    case do_check_child(ObjModule, ObjPath, State) of
        {ok, _Name, Data} ->
            reply({ok, Data}, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({nkdomain_set_child, _ObjModule, _ObjId, _ObjPath, _Pid}, _From,
             #state{session=#obj_session{status={stopped, _}}}=State) ->
    reply({error, object_is_stopped}, State);

handle_call({nkdomain_set_child, ObjModule, ObjId, ObjPath, Pid}, _From, State) ->
    #state{childs=Childs} = State,
    case do_check_child(ObjModule, ObjPath, State) of
        {ok, ObjName, Data} ->
            Child = #child{
                module = ObjModule,
                obj_id = ObjId,
                name = ObjName,
                pid = Pid
            },
            monitor(process, Pid),
            Childs2 = [Child|Childs],
            reply({ok, Data}, State#state{childs=Childs2});
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call(Msg, From, State) ->
    handle(object_handle_call, [Msg, From], State).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(nkdomain_do_start, #state{obj_id = <<"root">>}=State) ->
    #state{session=#obj_session{parent_id = <<>>}} = State,
    ?LLOG(notice, "domain ROOT loaded", [], State),
    {ok, State2} = handle(object_start, [], State),
    State3 = do_save(State2),
    noreply(State3);

handle_cast(nkdomain_do_start, State) ->
    #state{srv_id=SrvId, obj_id=ObjId, session=Session} = State,
    #obj_session{parent_id=ParentId, path=Path, module=Module} = Session,
    ?DEBUG("loading parent ~s", [ParentId], State),
    case load(SrvId, ParentId, #{}) of
        {ok, _ParentModule, ParentId, Pid} ->
            case do_call(Pid, {nkdomain_set_child, Module, ObjId, Path, self()}) of
                {ok, #{enabled:=Enabled}} ->
                    State2 = do_enabled(Enabled, State),
                    monitor(process, Pid),
                    State3 = State2#state{parent_pid=Pid},
                    {ok, State4} = handle(object_start, [], State3),
                    State5 = do_save(State4),
                    case do_check_expire(State5) of
                        false ->
                            noreply(State5);
                        true ->
                            do_stop(object_expired, State5)
                    end;
                {error, Error} ->
                    do_stop(Error, State)
            end;
        {error, Error} ->
            ?LLOG(warning, "could not load parent: ~p", [Error], State),
            do_stop(parent_load_error, State)
    end;

handle_cast({nkdomain_add_timelog, Data}, State) ->
    noreply(do_add_timelog(Data, State));

handle_cast(nkdomain_save, #state{save_op=Op}=State) when Op==archive; Op==remove ->
    ?LLOG(info, "received save command in ~p save state", [Op], State),
    noreply(State);

handle_cast(nkdomain_save, State) ->
    State2 = do_save(State),
    noreply(State2);

handle_cast({nkdomain_remove, Reason}, State) ->
    State2 = do_remove(Reason, State),
    do_stop(object_removed, State2);

handle_cast({nkdomain_async_op, Op}, State) ->
    case handle(object_async_op, [Op], State) of
        {noreply, State2} ->
            noreply(State2);
        {stop, Reason, State2} ->
            do_stop(Reason, State2);
        {continue, State2} ->
            do_async_op(Op, State2)
    end;

handle_cast({nkdomain_father_enabled, Enabled}, State) ->
    noreply(do_enabled(Enabled, State));

handle_cast({nkdomain_restart_timer, Time}, #state{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    NewTimer = erlang:start_timer(Time, self(), nkdomain_session_timeout),
    State#state{timer=NewTimer};

handle_cast({nkdomain_send_info, Info, Meta}, State) ->
    noreply(do_event({info, Info, Meta}, State));

handle_cast({nkdomain_register, Link}, State) ->
    ?DEBUG("registered link (~p)", [Link], State),
    noreply(links_add(Link, State));

handle_cast({nkdomain_unregister, Link}, State) ->
    ?DEBUG("proc unregistered (~p)", [Link], State),
    State2 = links_remove(Link, State),
    do_check_links_down(State2);

handle_cast({nkdomain_stop, Error}, State) ->
    ?DEBUG("received stop: ~p", [Error], State),
    do_stop(Error, State);

handle_cast(Msg, State) ->
    handle(object_handle_cast, [Msg], State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info({nkservice_updated, _SrvId}, State) ->
    noreply(set_log(State));

handle_info(nkdomain_check_expire, State) ->
    case do_check_expire(State) of
        false ->
            noreply(State);
        true ->
            do_stop(object_expired, State)
    end;

handle_info(nkdomain_save, State) ->
    handle_cast(nkdomain_save, State);

handle_info(nkdomain_destroy, State) ->
    do_destroy(State);

handle_info({timeout, Ref, nkdomain_session_timeout}, #state{timer=Ref}=State) ->
    ?DEBUG("session timeout", [], State),
    do_stop(nkdomain_session_timeout, State);

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{parent_pid=Pid}=State) ->
    ?DEBUG("parent stopped", [], State),
    do_stop(parent_stopped, State);

handle_info({'DOWN', Ref, process, Pid, Reason}=Msg, State) ->
    #state{stop_reason=Stop, childs=Childs} = State,
    case links_down(Ref, State) of
        {ok, Link, State2} when Stop==false ->
            case handle(object_reg_down, [Link, Reason], State2) of
                {ok, State3} ->
                    do_check_links_down(State3);
                {stop, normal, State3} ->
                    ?DEBUG("reg '~p' down (~p)", [Link, Reason], State3),
                    do_stop(normal, State3);
                {stop, Error, State3} ->
                    ?LLOG(info, "reg '~p' down (~p)", [Link, Reason], State3),
                    do_stop(Error, State3)
            end;
        {ok, _, State2} ->
            noreply(State2);
        not_found ->
            case lists:keytake(Pid, #child.pid, Childs) of
                {value, #child{module=Module, obj_id=ObjId}, Rest} ->
                    ?DEBUG("child ~s:~s stopped", [Module, ObjId], State),
                    noreply(State#state{childs=Rest});
                false ->
                    handle(object_handle_info, [Msg], State)
            end
    end;

handle_info(Msg, State) ->
    handle(object_handle_info, [Msg], State).


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(OldVsn, State, Extra) ->
    nklib_gen_server:code_change(object_code_change,
        OldVsn, State, Extra,
        #state.srv_id, #state.session).


%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(Reason, #state{stop_reason=Stop, timelog=Log}=State) ->
    State2 = case Stop of
        false ->
            {noreply, StopState} = do_stop({terminate, Reason}, State),
            StopState;
        _ ->
            State
    end,
    State3 = do_event({record, lists:reverse(Log)}, State2),
    State4 = do_event(destroyed, State3),
    {ok, _State5} = handle(object_terminate, [Reason], State4),
    ok.




%% ===================================================================
%% OPs
%% ===================================================================

%% @private
do_sync_op({apply, Fun}, _From, #state{session=Session}=State) ->
    {Reply2, State2} = try Fun(Session) of
        {Reply, #obj_session{}=Session2} ->
            {Reply, State#state{session=Session2}};
        Reply ->
            {Reply, State}
    catch
        error:Error ->
            ?LLOG(warning, "error calling apply fun: ~p", [Error], State),
            {{error, internal_error}, State}
    end,
    reply(Reply2, State2);

do_sync_op(Op, _From, State) ->
    ?LLOG(notice, "unknown sync op: ~p", [Op], State),
    reply({error, unknown_op}, State).


%% @private
do_async_op({set_enabled, Enabled}, #state{session=#obj_session{obj=Obj}}=State) ->
    State2 = case maps:get(enabled, Obj, true) of
        Enabled ->
            State;
        _ ->
            ?DEBUG("setting enabled to: ~p", [Enabled], State),
            do_update(#{enabled=>Enabled}, State)
    end,
    noreply(do_enabled(Enabled, State2));

do_async_op({apply, Fun}, #state{session=Session}=State) ->
    State2 = try Fun(Session) of
        {_Reply, #obj_session{}=Session2} ->
            State#state{session=Session2};
        _Reply ->
            State
    catch
        error:Error ->
            ?LLOG(warning, "error calling apply fun: ~p", [Error], State),
            State
    end,
    noreply(State2);

do_async_op(Op, State) ->
    ?LLOG(notice, "unknown async op: ~p", [Op], State),
    noreply(State).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
set_log(#state{srv_id=SrvId, session=#obj_session{module=Module}}=State) ->
    Debug =
        case nkservice_util:get_debug_info(SrvId, ?MODULE) of
            {true, all} -> true;
            {true, #{modules:=Modules}} -> lists:member(Module, Modules);
            {true, _} -> true;
            _ -> false
        end,
    put(object_debug, Debug),
    State.


%% @private
do_check_expire(#state{session=#obj_session{obj=Obj}}) ->
    case maps:get(expires_time, Obj, 0) of
        0 ->
            false;
        Expires ->
            case nklib_util:m_timestamp() of
                Now when Now >= Expires ->
                    true;
                Now ->
                    Remind = min(3600000, Expires - Now),
                    erlang:send_after(Remind, self(), nkdomain_check_expire),
                    false
            end
    end.


%% @private
do_save(#state{session=#obj_session{is_dirty=false}}=State) ->
    State;

do_save(#state{save_op=Op}=State) ->
    ?DEBUG("~p object", [Op], State),
    case Op of
        save ->
            case handle(object_save, [], State) of
                {ok, State2} ->
                    do_save_ok(State2);
                {error, Error, State2} ->
                    do_save_error(Error, State2)
            end;
        archive ->
            case handle(object_archive, [], State) of
                {ok, State2} ->
                    do_save(State2#state{save_op=remove});
                {error, Error, State2} ->
                    do_save_error(Error, State2)
            end;
        remove ->
            case handle(object_remove, [], State) of
                {ok, State2} ->
                    do_save_ok(State2);
                {error, Error, State2} ->
                    do_save_error(Error, State2)
            end
    end.


%% @private
do_save_ok(#state{session=Session}=State) ->
    Session2 = Session#obj_session{is_dirty=false},
    State#state{save_op=none, save_tries=0, session=Session2}.


%% @private
do_save_error(Error, #state{save_op=Op, save_tries=Tries}=State) ->
    ?LLOG(warning, "could not ~p object: ~p", [Op, Error], State),
    erlang:send_after(?SAVE_RETRY, self(), nkdomain_save),
    State#state{save_tries=Tries+1}.


%% @private
do_remove(Reason, #state{srv_id=SrvId, session=#obj_session{obj=Obj}=Session}=State) ->
    {Code, Txt} = nkdomain_util:error_code(SrvId, Reason),
    Obj2 = ?ADD_TO_OBJ(
        #{
            destroyed_time => nklib_util:m_timestamp(),
            destroyed_code => Code,
            destroyed_reason => Txt
        }, Obj),
    State2 = State#state{session=Session#obj_session{obj=Obj2, is_dirty=true}},
    State3 = case handle(object_removed, [Reason], State2) of
        {ok, RemState} -> RemState#state{save_op=remove};
        {archive, RemState} -> RemState#state{save_op=archive}
    end,
    do_save(State3).


%% @private
do_stop(_Reason, #state{session=#obj_session{status={stopped, _Reason}}}=State) ->
    %% Stop already sent, timer is running
    noreply(State);

do_stop(Reason, #state{srv_id=SrvId, started=Started}=State) ->
    ?DEBUG("stopped: ~p", [Reason], State),
    {ok, State2} = handle(object_stop, [Reason], State#state{stop_reason=Reason}),
    {Code, Txt} = nkdomain_util:error_code(SrvId, Reason),
    State3 = do_add_timelog(#{msg=>stopped, code=>Code, reason=>Txt}, State2),
    % Give time for possible registrations to success and capture stop event
    State4 = do_status({stopped, Reason}, State3),
    timer:sleep(100),
    State5 = do_event({stopped, Reason}, State4),
    Now = nklib_util:m_timestamp(),
    case (Started + ?MIN_STARTED_TIME) - Now of
        Time when Time > 0 ->
            % Save a minimum running time
            erlang:send_after(Time, self(), nkdomain_destroy);
        _ ->
            % Process any remaining message
            self() ! nkdomain_destroy
    end,
    noreply(State5).


%% @private
do_destroy(#state{session=#obj_session{is_dirty=false}}=State) ->
    {stop, normal, State};

do_destroy(#state{save_tries=Tries}=State) ->
    case Tries > ?MAX_SAVE_RETRIES of
        true ->
            ?LLOG(warning, "could not save object, giving up", [], State),
            {stop, normal, State};
        false ->
            ?LLOG(notice, "cannot save object, retrying", [], State),
            erlang:send_after(?SAVE_RETRY, self(), nkdomain_destroy),
            noreply(State)
    end.


%% @private
do_check_child(ObjModule, ObjPath, State) ->
    #state{session=Session, childs=Childs} = State,
    #obj_session{path=Path} = Session,
    case nkdomain_util:get_parts(ObjModule, ObjPath) of
        {ok, Path, ObjName} ->
            case lists:keymember(ObjName, #child.name, Childs) of
                true ->
                    {error, invalid_name};
                false ->
                    Data = #{enabled=>Session#obj_session.enabled},
                    {ok, ObjName, Data}
            end;
        {ok, _, _} ->
            ?LLOG(notice, "error trying to check invalid child path (~s)", [ObjPath], State),
            {error, invalid_path};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_update(_Update, #state{save_op=Op}=State) when Op==remove; Op==archive ->
    ?LLOG(notice, "received update command in ~p save state", [Op], State),
    State;

do_update(Update, #state{session=#obj_session{obj=Obj}=Session}=State) ->
    Obj2 = ?ADD_TO_OBJ(Update, Obj),
    Session2 = Session#obj_session{obj=Obj2, is_dirty=true},
    {ok, State2} = handle(object_updated, [Update], State#state{session=Session2}),
    State3 = do_save(State2),
    do_event({updated, Update}, State3).


%% @private
do_enabled(Enabled, #state{session=#obj_session{enabled=Enabled}}=State) ->
    State;

do_enabled(false, State) ->
    do_set_enabled(false, State);

do_enabled(true, #state{session=#obj_session{obj=Obj}}=State) ->
    case maps:get(enabled, Obj, true) of
        true ->
            do_set_enabled(true, State);
        false ->
            State
    end.


%% @private
do_set_enabled(Enabled, #state{session=Session, childs=Childs}=State) ->
    Session2 = Session#obj_session{enabled=Enabled},
    {ok, State2} = handle(object_enabled, [], State#state{session=Session2}),
    lists:foreach(
        fun(#child{pid=Pid}) -> gen_server:cast(Pid, {nkdomain_father_enabled, Enabled}) end,
        Childs),
    do_event({enabled, Enabled}, State2).


%% @private
do_event(Event, State) ->
    ?DEBUG("sending 'event': ~p", [Event], State),
    State2 = links_fold(
        fun(Link, AccState) ->
            {ok, AccState2} =
                handle(object_reg_event, [Link, Event], AccState),
            AccState2
        end,
        State,
        State),
    {ok, State3} = handle(object_event, [Event], State2),
    State3.


%% @private
do_status(Status, #state{session=#obj_session{status=Status}}=State) ->
    noreply(State);

do_status(Status, #state{session=#obj_session{status=OldStatus}=Session}=State) ->
    ?DEBUG("status ~p -> ~p", [OldStatus, Status], State),
    State2 = State#state{session=Session#obj_session{status=Status}},
    {ok, State3} = handle(object_status, [Status], State2),
    do_event({status, Status}, State3).


%% @private
do_check_links_down(#state{session=#obj_session{module=nkdomain_domain}}=State) ->
    noreply(State);

do_check_links_down(#state{childs=Childs}=State) when Childs /= [] ->
    noreply(State);

do_check_links_down(State) ->
    case links_is_empty(State) of
        true ->
            case handle(object_all_links_down, [], State) of
                {ok, State2} ->
                    noreply(State2);
                {stop, Reason, State2} ->
                    do_stop(Reason, State2)
                end;
        false ->
            noreply(State)
    end.


%% ===================================================================
%% Util
%% ===================================================================

%% @private
reply(Reply, #state{}=State) ->
    {reply, Reply, State}.


%% @private
noreply(#state{}=State) ->
    {noreply, State}.


%% @private
handle(Fun, Args, State) ->
    nklib_gen_server:handle_any(Fun, Args, State, #state.srv_id, #state.session).

%% @private
do_find({Srv, Path}) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:object_store_find_path(SrvId, Path) of
                {ok, _Module, ObjId} when is_binary(ObjId) ->
                    do_find(ObjId);
                _ ->
                    not_found
            end;
        not_found ->
            not_found
    end;

do_find(ObjId) when is_binary(ObjId) ->
    case nklib_proc:values({?MODULE, ObjId}) of
        [{{Module, Path}, Pid}] ->
            {ok, Module, Path, Pid};
        [] ->
            not_found
    end;

do_find(ObjId) ->
    do_find(nklib_util:to_binary(ObjId)).


%% @private
do_call(Id, Msg) ->
    do_call(Id, Msg, ?DEF_SYNC_CALL).


%% @private
do_call(Pid, Msg, Timeout) when is_pid(Pid) ->
    nkservice_util:call(Pid, Msg, Timeout);

do_call(Id, Msg, Timeout) ->
    case do_find(Id) of
        {ok, _Module, _Path, Pid} when is_pid(Pid) ->
            do_call(Pid, Msg, Timeout);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);

do_cast(Id, Msg) ->
    case do_find(Id) of
        {ok, _Module, _Path, Pid} when is_pid(Pid) ->
            do_cast(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_info(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;

do_info(Id, Msg) ->
    case do_find(Id) of
        {ok, _Module, _Path, Pid} when is_pid(Pid) ->
            do_info(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_add_timelog(Msg, State) when is_atom(Msg); is_binary(Msg) ->
    do_add_timelog(#{msg=>Msg}, State);

do_add_timelog(#{msg:=_}=Data, #state{started=Started, timelog=Log}=State) ->
    Time = nklib_util:m_timestamp() - Started,
    State#state{timelog=[Data#{time=>Time}|Log]}.



%% @private
links_add(Link, #state{links=Links}=State) ->
    State#state{links=nklib_links:add(Link, Links)}.


%%%% @private
%%links_add(Link, Pid, #state{links=Links}=State) ->
%%    State#state{links=nklib_links:add(Link, none, Pid, Links)}.


%% @private
links_remove(Link, #state{links=Links}=State) ->
    State#state{links=nklib_links:remove(Link, Links)}.


%% @private
links_is_empty(#state{links=Links}) ->
    nklib_links:is_empty(Links).


%% @private
links_down(Mon, #state{links=Links}=State) ->
    case nklib_links:down(Mon, Links) of
        {ok, Link, _Data, Links2} ->
            {ok, Link, State#state{links=Links2}};
        not_found ->
            not_found
    end.


%% @private
links_fold(Fun, Acc, #state{links=Links}) ->
    nklib_links:fold(Fun, Acc, Links).

