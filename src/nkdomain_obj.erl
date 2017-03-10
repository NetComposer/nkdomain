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
-export([set_enabled/2, remove/2]).
-export([do_find/1, do_call/2, do_call/3, do_cast/2, do_info/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([get_all/0, stop_all/0]).
-export_type([event/0]).


-define(DEBUG(Txt, Args, State),
    case erlang:get(object_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, State),
    lager:Type(
        [
            {obj_id, State#state.obj_id},
            {module, State#state.module},
            {path, State#state.path}
        ],
        "NkDOMAIN Obj (~s:~s, ~s) "++Txt,
        [State#state.module, State#state.obj_id, State#state.path | Args])).

-define(MIN_STARTED_TIME, 2000).
-define(DEF_SYNC_CALL, 5000).
-define(SAVE_RETRY, 5000).

-include("nkdomain.hrl").

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
        events => [nkservice_events:type()]
    }.

-type load_opts() ::
    #{

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





%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, module(), domain:obj_id(), nkdomain:path(), pid()|undefined} |
    {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkdomain_types:is_path(IdOrPath) of
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
        {ok, _Module, _ObjId, Pid} ->
            case do_call(Pid, {check_child, Path}) of
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
    do_call(Id, get_session).


%% @doc
-spec save(id()) ->
    ok | {error, term()}.

save(Id) ->
    do_cast(Id, do_save).


%% @doc
-spec stop(id(), nkservice:error()) ->
    ok | {error, term()}.

stop(Id, Reason) ->
    do_cast(Id, {stop, Reason}).


%% @doc
-spec sync_op(id(), op()) ->
    {ok, term()} | {error, term()}.

sync_op(Id, Op) ->
    do_call(Id, {sync_op, Op}).


%% @doc
-spec async_op(id(), op()) ->
    ok | {error, term()}.

async_op(Id, Op) ->
    do_cast(Id, {async_op, Op}).


%% @doc
-spec remove(id(), nkservice:error()) ->
    ok | {error, term()}.

remove(Id, Reason) ->
    async_op(Id, {remove, Reason}).


%% @doc
-spec set_enabled(id(), boolean()) ->
    ok | {error, term()}.

set_enabled(Id, Enabled) when is_boolean(Enabled)->
    async_op(Id, {set_enabled, Enabled}).


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
    module :: module(),
    path :: nkdomain:domain(),
    parent_id :: nkdomain:obj_id(),
    parent_pid :: pid(),
    srv_id :: nkservice:id(),
    stop_reason = false :: false | nkservice:error(),
    links :: nklib_links:links(),
    session :: session(),
    started :: nklib_util:m_timestamp(),
    timer :: reference(),
    childs = [] :: [#child{}],
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
    Session = #obj_session{
        obj_id = ObjId,
        module = Module,
        type = maps:get(type, Obj),
        obj = Obj,
        srv_id = SrvId,
        meta = maps:without([srv_id, is_dirty], Meta),
        is_dirty = IsDirty,
        enabled = maps:get(enabled, Obj, true)
    },
    State1 = #state{
        module = Module,
        obj_id = ObjId,
        path = Path,
        parent_id = Parent,
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
    gen_server:cast(self(), do_start),
    {ok, State3} = handle(object_init, [], State2),
    {ok, do_event(created, State3)}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(get_session, _From, #state{session=Session}=State) ->
    reply({ok, Session}, State);

handle_call(get_timelog, _From, #state{timelog=Log}=State) ->
    reply({ok, Log}, State);

handle_call(get_state, _From, State) ->
    reply(State, State);

handle_call(Msg, _From, #state{stop_reason=Reason}=State) when Reason /= false ->
    ?LLOG(info, "received gen_server call (~p) while stopping", [Msg], State),
    reply({error, process_is_stopping}, State);

handle_call({sync_op, Op}, From, State) ->
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

handle_call({check_child, ObjPath}, _From, State) ->
    case do_check_child(ObjPath, State) of
        {ok, _Name, Data} ->
            reply({ok, Data}, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({set_child, ObjModule, ObjId, ObjPath, Pid}, _From, State) ->
    #state{childs=Childs} = State,
    case do_check_child(ObjPath, State) of
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

handle_cast(do_start, #state{obj_id = <<"root">>, parent_id = <<>>}=State) ->
    ?LLOG(notice, "domain ROOT loaded", [], State),
    {ok, State2} = handle(object_start, [], State),
    gen_server:cast(self(), do_save),
    noreply(State2);

handle_cast(do_start, State) ->
    #state{srv_id=SrvId, module=Module, obj_id=ObjId, parent_id=ParentId, path=Path} = State,
    ?DEBUG("loading parent ~s", [ParentId], State),
    case load(SrvId, ParentId, #{}) of
        {ok, _ParentModule, ParentId, Pid} ->
            case do_call(Pid, {set_child, Module, ObjId, Path, self()}) of
                {ok, #{enabled:=Enabled}} ->
                    State2 = do_enabled(Enabled, State),
                    monitor(process, Pid),
                    State3 = State2#state{parent_pid=Pid},
                    gen_server:cast(self(), do_save),
                    {ok, State4} = handle(object_start, [], State3),
                    noreply(State4);
                {error, Error} ->
                    do_stop(Error, State)
            end;
        {error, Error} ->
            ?LLOG(warning, "could not load parent: ~p", [Error], State),
            do_stop(parent_error, State)
    end;

handle_cast({add_timelog, Data}, State) ->
    noreply(do_add_timelog(Data, State));

handle_cast(Msg, #state{stop_reason=Reason}=State) when Reason /= false ->
    ?LLOG(info, "received gen_server cast (~p) while stopping", [Msg], State),
    noreply(State);

handle_cast(do_save, State) ->
    {_Res, State2} = do_save(State),
    noreply(State2);

handle_cast({async_op, Op}, State) ->
    case handle(object_async_op, [Op], State) of
        {noreply, State2} ->
            noreply(State2);
        {stop, Reason, State2} ->
            do_stop(Reason, State2);
        {continue, State2} ->
            do_async_op(Op, State2)
    end;

handle_cast({father_enabled, Enabled}, State) ->
    noreply(do_enabled(Enabled, State));

handle_cast({restart_timer, Time}, #state{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    NewTimer = erlang:start_timer(Time, self(), session_timeout),
    State#state{timer=NewTimer};

handle_cast({send_info, Info, Meta}, State) ->
    noreply(do_event({info, Info, Meta}, State));

handle_cast({register, Link}, State) ->
    ?DEBUG("registered link (~p)", [Link], State),
    noreply(links_add(Link, State));

handle_cast({unregister, Link}, State) ->
    ?DEBUG("proc unregistered (~p)", [Link], State),
    noreply(links_remove(Link, State));

handle_cast({stop, Error}, State) ->
    ?DEBUG("received stop: ~p", [Error], State),
    do_stop(Error, State);

handle_cast(Msg, State) ->
    handle(object_handle_cast, [Msg], State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info({timeout, Ref, session_timeout}, #state{timer=Ref}=State) ->
    ?DEBUG("session timeout", [], State),
    do_stop(session_timeout, State);

handle_info({nkservice_updated, _SrvId}, State) ->
    noreply(set_log(State));

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{parent_pid=Pid}=State) ->
    ?DEBUG("parent stopped", [], State),
    do_stop(parent_stopped, State);

handle_info({'DOWN', Ref, process, Pid, Reason}=Msg, State) ->
    #state{stop_reason=Stop, childs=Childs} = State,
    case links_down(Ref, State) of
        {ok, Link, State2} when Stop==false ->
            case handle(object_reg_down, [Link, Reason], State2) of
                {ok, State3} ->
                    noreply(State3);
                {stop, normal, State3} ->
                    do_stop(normal, State3);
                {stop, Error, State3} ->
                    case Reason of
                        normal ->
                            ?DEBUG("reg '~p' down (~p)", [Link, Reason], State3);
                        _ ->
                            ?LLOG(info, "reg '~p' down (~p)", [Link, Reason], State3)
                    end,
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

handle_info(destroy, State) ->
    {stop, normal, State};

handle_info(check_expire, #state{session=#obj_session{obj=Obj}}=State) ->
    case maps:get(expires_time, Obj, 0) of
        0 ->
            ok;
        Expires ->
            case nklib_util:m_timestamp() of
                Now when Now >= Expires ->
                    remove(self(), expired);
                Now ->
                    Remind = min(3600000, Expires - Now),
                    erlang:send_after(Remind, self(), check_expire)
            end
    end,
    noreply(State);

handle_info(Msg, #state{stop_reason=Reason}=State) when Reason /= false ->
    ?LLOG(info, "received gen_server info (~p) while stopping", [Msg], State),
    noreply(State);

handle_info(do_save, State) ->
    {_Res, State2} = do_save(State),
    noreply(State2);

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
            case do_stop({terminate, Reason}, State) of
                {noreply, StopState}  -> StopState;
                {stop, normal, StopState} -> StopState
            end;
        _ ->
            State
    end,
    State3 = do_event({record, lists:reverse(Log)}, State2),
    State4 = do_event(destroyed, State3),
    {ok, _State5} = handle(object_terminate, [Reason], State4),
    ok.




%% ===================================================================
%% Internal
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

do_async_op({remove, Reason}, #state{srv_id=SrvId, session=#obj_session{obj=Obj}=Session}=State) ->
    ?DEBUG("received REMOVE: ~p", [Reason], State),
    {Code, Txt} = nkdomain_util:error_code(SrvId, Reason),
    Obj2 = ?ADD_TO_OBJ(
        #{
            destroyed_time => nklib_util:m_timestamp(),
            destroyed_code => Code,
            destroyed_reason => Txt
        }, Obj),
    Session2 = Session#obj_session{obj=Obj2, is_dirty=false},
    {_Res, State2} = do_save(State#state{session=Session2}),
    State3 = case handle(object_remove, [Reason], State2) of
        {ok, RemState} ->
            RemState;
        {error, Error, RemState} ->
            ?LLOG(warning, "could not remote object: ~p", [Error], RemState),
            RemState
    end,
    do_stop(object_removed, State3);

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

%% @private
set_log(#state{srv_id=SrvId, module=Module}=State) ->
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
do_save(#state{session=#obj_session{is_dirty=false}}=State) ->
    {ok, State};

do_save(#state{session=Session}=State) ->
    ?DEBUG("saving object", [], State),
    case handle(object_save, [], State) of
        {ok, State2} ->
            Session2 = Session#obj_session{is_dirty=false},
            {ok, State2#state{session=Session2}};
        {error, Error, State2} ->
            ?LLOG(warning, "could not save object: ~p", [Error], State2),
            erlang:send_after(?SAVE_RETRY, self(), do_save),
            {error, State2}
    end.


%% @private
do_stop(Reason, #state{srv_id=SrvId, stop_reason=false, started=Started, session=Session}=State) ->
    ?DEBUG("stopped: ~p", [Reason], State),
    {ok, State2} = handle(object_stop, [Reason], State#state{stop_reason=Reason}),
    {Code, Txt} = nkdomain_util:error_code(SrvId, Reason),
    State3 = do_add_timelog(#{msg=>stopped, code=>Code, reason=>Txt}, State2),
    % Give time for possible registrations to success and capture stop event
    timer:sleep(100),
    State4 = do_event({stopped, Reason}, State3),
    Now = nklib_util:m_timestamp(),
    case (Started + ?MIN_STARTED_TIME) - Now of
        Time when Time > 0 ->
            % Save a minimum started time
            erlang:send_after(Time, self(), destroy),
            noreply(State4);
        _ ->
            case Session of
                #obj_session{is_dirty=true} ->
                    ?LLOG(warning, "stopping dirty object", [], State4);
                _ -> ok
            end,
            {stop, normal, State4}
    end;

%% Stop already sent
do_stop(_Reason, State) ->
    noreply(State).


%% @private
do_check_child(ObjPath, State) ->
    #state{path=Path, session=Session, childs=Childs} = State,
    [ObjName|Parts] = lists:reverse(binary:split(ObjPath, <<"/">>, [global])),
    Base = case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
        <<>> -> <<"/">>;
        Base0 -> Base0
    end,
    case Base of
        Path ->
            case ObjName of
                <<>> ->
                    {error, invalid_name};
                _ ->
                    case lists:keymember(ObjName, #child.name, Childs) of
                        true ->
                            {error, invalid_name};
                        false ->
                            Data = #{enabled=>Session#obj_session.enabled},
                            {ok, ObjName, Data}
                    end
            end;
        _Other ->
            ?LLOG(notice, "trying to check invalid child path (~s)", [ObjPath], State),
            {error, invalid_path}
    end.


%% @private
do_update(Update, #state{session=#obj_session{obj=Obj}=Session}=State) ->
    Obj2 = ?ADD_TO_OBJ(Update, Obj),
    Session2 = Session#obj_session{obj=Obj2, is_dirty=true},
    {ok, State2} = handle(object_updated, [Update], State#state{session=Session2}),
    {_Res, State3} = do_save(State2),
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
        fun(#child{pid=Pid}) -> gen_server:cast(Pid, {father_enabled, Enabled}) end,
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

