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

-export([find/2, load/2, load/3, create/3, get_session/1, save/1, unload/2]).
-export([update/2, set_enabled/2, delete/2, sync_op/2, async_op/2]).
-export([register/2, unregister/2, get_childs/1]).
-export([start/2, init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([get_all/0, unload_all/0]).
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
            {type, (State#state.session)#obj_session.type},
            {path, (State#state.session)#obj_session.path}
        ],
        "NkDOMAIN Obj ~s (~s) "++Txt,
        [
            (State#state.session)#obj_session.path,
            State#state.obj_id
            | Args]
        )).

-define(MIN_STARTED_TIME, 10000).

-include("nkdomain.hrl").
-compile({no_auto_import, [register/2]}).

%% ===================================================================
%% Callbacks definitions
%% ===================================================================

-callback object_get_info() ->
    object_info().


-callback object_mapping() ->
    map().


-callback object_syntax(load|update) ->
    nklib_syntax:syntax().


-callback object_api_syntax(nkapi:subclass(), nkapi:cmd(), nklib_syntax:stntax()) ->
    nklib_syntax:syntax() | continue.


-callback object_api_allow(nkapi:subclass(), nkapi:cmd(), nkapi:data(), nkapi:state()) ->
    {boolean, nkapi:state()}.


-callback object_api_cmd(nkapi:subclass(), nkapi:cmd(), nkapi:data(), nkapi:state()) ->
    {ok, map(), nkapi:state()} | {ack, nkapi:state()} |
    {login, Reply::term(), User::binary(), Meta::map(), nkapi:state()} |
    {error, nkapi:error(), nkapi:state()}.




%% ===================================================================
%% Types
%% ===================================================================


-type id() ::
    nkdomain:obj_id() | {nkservice:id(), nkdomain:path()} | pid().

-type object_info() ::
    #{
        type => nkdomain:type()
    }.

-type create_opts() ::
    #{
        obj_id => nkdomain:obj_id(),
        register => nklib:link(),
%%        user_id => nkdomain:obj_id(),
%%        user_session => nkservice:user_session(),
        events => [nkservice_events:type()],
        enabled => boolean(),                        % Start disabled
        update_pid => boolean(),
        remove_after_stop => boolean()
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
    {ok, nkdomain:type(), domain:obj_id(), nkdomain:path(), pid()|undefined} |
    {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    nkdomain_obj_lib:find(Srv, IdOrPath).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, IdOrPath) ->
    load(Srv, IdOrPath, #{}).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path(), load_opts()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, IdOrPath, Meta) ->
    nkdomain_obj_lib:load(Srv, IdOrPath, Meta).


%% @doc Creates a new object
-spec create(nkservice:id(), map(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()}.

create(Srv, Obj, Meta) ->
    nkdomain_obj_lib:create(Srv, Obj, Meta).


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
-spec delete(id(), nkservice:error()) ->
    ok | {error, term()}.

delete(Id, Reason) ->
    do_call(Id, {nkdomain_delete, Reason}).


%% @doc
-spec unload(id(), nkservice:error()) ->
    ok | {error, term()}.

unload(Id, Reason) ->
    do_cast(Id, {nkdomain_unload, Reason}).


%% @doc
-spec set_enabled(id(), boolean()) ->
    ok | {error, term()}.

set_enabled(Id, Enabled) when is_boolean(Enabled)->
    async_op(Id, {set_enabled, Enabled}).


%% @doc
-spec update(id(), map()) ->
    ok | {error, term()}.

update(Id, Map) ->
    do_call(Id, {nkdomain_update, Map}).


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


% @doc
-spec get_childs(id()) ->
    ok | {error, term()}.

get_childs(Id) ->
    do_call(Id, nkdomain_get_childs).


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
    lists:foreach(fun({_Module, _ObjId, _Path, Pid}) -> unload(Pid, normal) end, get_all()).


% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start(Obj, Meta) ->
    gen_server:start(?MODULE, {Obj, Meta}, []).



-record(child, {
    obj_id :: nkdomain:obj_id(),
    type_name :: {Type::nkdomain:type(), Name::binary()},
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
    is_deleted = false :: boolean(),
    childs = [] :: [#child{}],
    timelog = [] :: [map()]
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init({Obj, Meta}) ->
    #{obj_id:=ObjId, type:=Type, path:=Path, parent_id:=Parent} = Obj,
    Module = nkdomain_types:get_module(Type),
    false = Module==undefined,
    true = nklib_proc:reg({?MODULE, ObjId}, {Type, Path}),
    nklib_proc:put(?MODULE, {Type, ObjId, Path}),
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
        type = Type,
        parent_id = Parent,
        obj = Obj,
        srv_id = SrvId,
        status = init,
        meta = maps:without([srv_id, is_dirty, obj_id, update_pid], Meta),
        is_dirty = IsDirty,
        enabled = Enabled
    },
    Session2 = case Meta of
        #{update_pid:=true} ->
            Session#obj_session{
                obj = Obj#{pid=>self()},
                is_dirty = true
            };
        _ ->
            Session
    end,
    State1 = #state{
        obj_id = ObjId,
        srv_id = SrvId,
        links = nklib_links:new(),
        session = Session2,
        started = nklib_util:m_timestamp()
    },
    State2 = case Meta of
         #{register:=Link} ->
            links_add(Link, State1);
         _ ->
            erlang:send_after(?MIN_STARTED_TIME, self(), nkdomain_check_childs),
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

handle_call(nkdomain_get_childs, _From, #state{childs=Childs}=State) ->
    reply({ok, Childs}, State);

handle_call(nkdomain_get_state, _From, State) ->
    reply(State, State);

handle_call({nkdomain_update, Map}, _From, State) ->
    case is_stopped(State) of
        true ->
            reply({error, object_is_stopped}, State);
        false ->
            case do_update(Map, State) of
                {ok, State2} ->
                    reply(ok, State2);
                {error, Error, State2} ->
                    reply({error, Error}, State2)
            end
    end;

handle_call({nkdomain_delete, Reason}, From, #state{srv_id=SrvId, obj_id=ObjId}=State) ->
    case SrvId:object_store_find_childs(SrvId, ObjId, #{size=>0}) of
        {ok, 0, []} ->
            State2 = do_delete(Reason, State),
            gen_server:reply(From, ok),
            do_stop(object_deleted, State2);
        {ok, _N, []} ->
            reply({error, object_has_childs}, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({nkdomain_sync_op, Op}, From, State) ->
    case is_stopped(State) of
        true ->
            reply({error, object_is_stopped}, State);
        false ->
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
            end
    end;

handle_call({nkdomain_check_create_child, ObjType, ObjPath}, _From, State) ->
    Reply = case do_check_child(ObjType, ObjPath, State) of
        {ok, _Name, _Data} ->
            do_check_create_child(ObjType, ObjPath, State);
        {error, Error} ->
            {error, Error}
    end,
    reply(Reply, State);

handle_call({nkdomain_check_child, ObjType, ObjPath}, _From, State) ->
    case do_check_child(ObjType, ObjPath, State) of
        {ok, _Name, Data} ->
            reply({ok, Data}, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;


handle_call({nkdomain_set_child, ObjType, ObjId, ObjPath, Pid}, _From, State) ->
    case is_stopped(State) of
        true ->
            reply({error, object_is_stopped}, State);
        false ->
            #state{childs=Childs} = State,
            case do_check_child(ObjType, ObjPath, State) of
                {ok, ObjName, Data} ->
                    Child = #child{
                        obj_id = ObjId,
                        type_name = {ObjType, ObjName},
                        pid = Pid
                    },
                    monitor(process, Pid),
                    Childs2 = [Child|Childs],
                    reply({ok, Data}, State#state{childs=Childs2});
                {error, Error} ->
                    reply({error, Error}, State)
            end
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
    #obj_session{parent_id=ParentId, type=Type, path=Path} = Session,
    ?DEBUG("loading parent ~s", [ParentId], State),
    case load(SrvId, ParentId, #{}) of
        {ok, _ParentModule, ParentId, _Path, Pid} ->
            case do_call(Pid, {nkdomain_set_child, Type, ObjId, Path, self()}) of
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

handle_cast(nkdomain_save, State) ->
    noreply(do_save(State));

handle_cast({nkdomain_async_op, Op}, State) ->
    case is_stopped(State) of
        true ->
            noreply(State);
        false ->
            case handle(object_async_op, [Op], State) of
                {noreply, State2} ->
                    noreply(State2);
                {stop, Reason, State2} ->
                    do_stop(Reason, State2);
                {continue, State2} ->
                    do_async_op(Op, State2)
            end
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

handle_cast({nkdomain_unload, Error}, State) ->
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

handle_info(nkdomain_check_childs, State) ->
    do_check_links_down(State);

handle_info(nkdomain_destroy, State) ->
    {stop, normal, State};

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
                    ?DEBUG("reg '~p' down (~p)", [Link, Reason], State3),
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
                {value, #child{type_name={Type, Name}, obj_id=ObjId}, Rest} ->
                    ?DEBUG("child ~s:~s (~s) stopped", [Type, Name, ObjId], State),
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
            case do_update(#{enabled=>Enabled}, State) of
                {ok, UpdateState} -> UpdateState;
                {error, _Error, UpdateState} -> UpdateState
            end
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
set_log(#state{srv_id=SrvId, session=#obj_session{type=Type}}=State) ->
    Debug =
        case nkservice_util:get_debug_info(SrvId, ?MODULE) of
            {true, all} -> true;
            {true, #{types:=Types}} -> lists:member(Type, Types);
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

do_save(State) ->
    ?DEBUG("save object", [], State),
    State2 = case handle(object_save, [], State) of
        {ok, SaveState} ->
            SaveState;
        {error, _Error, SaveState} ->
            % Error will be managed by nkdomain_store
            SaveState
    end,
    #state{session=Session} = State2,
    Session2 = Session#obj_session{is_dirty=false},
    State#state{session=Session2}.


%% @private
do_delete(_Reason, #state{is_deleted=true}=State) ->
    State;

do_delete(Reason, #state{srv_id=SrvId}=State) ->
    {ok, State2} = handle(object_deleted, [Reason], State),
    {Code, Txt} = nkapi_util:api_error(SrvId, Reason),
    #state{session=#obj_session{obj=Obj}=Session} = State2,
    Obj2 = ?ADD_TO_OBJ(
        #{
            destroyed_time => nklib_util:m_timestamp(),
            destroyed_code => Code,
            destroyed_reason => Txt
        }, Obj),
    State3 = State2#state{session=Session#obj_session{obj=Obj, is_dirty=true}},
    State4 = case handle(object_archive, [Obj2], State3) of
        {ok, ArchState} ->
            ArchState;
        {error, _Error1, ArchState} ->
            ArchState
    end,
    ?DEBUG("delete object", [], State4),
    State5 = case handle(object_delete, [], State4) of
        {ok, RemState} ->
            RemState;
        {error, _Error2, RemState} ->
            %% Errors will be managed by nkdomain_store
            RemState
    end,
    State5#state{is_deleted=true}.


%% @private
do_stop(Reason, #state{srv_id=SrvId}=State) ->
    case is_stopped(State) of
        true ->
            noreply(State);
        false ->
            {ok, State2} = handle(object_stop, [Reason], State#state{stop_reason=Reason}),
            {Code, Txt} = nkapi_util:api_error(SrvId, Reason),
            State3 = do_add_timelog(#{msg=>stopped, code=>Code, reason=>Txt}, State2),
            % Give time for possible registrations to success and capture stop event
            State4 = do_status({stopped, Reason}, State3),
            #state{session=#obj_session{obj=Obj}=Session} = State4,
%%            State5 = case Obj of
%%                #{pid:=Pid} when Pid==self() ->
%%                    Session2 = Session#obj_session{obj=maps:remove(pid, Obj), is_dirty=true},
%%                    do_save(State4#state{session=Session2});
%%                _ ->
%%                    State4
%%            end,
            do_stop2(Reason, State4)
    end.


%% @private
do_stop2(Reason, #state{started=Started}=State) ->
    State2 = do_event({stopped, Reason}, State),
    #state{session = #obj_session{meta = Meta}} = State2,
    State3 = case Meta of
        #{remove_after_stop:=true} ->
            do_delete(object_stopped, State2);
        _ ->
            State2
    end,
    Now = nklib_util:m_timestamp(),
    case (Started + ?MIN_STARTED_TIME) - Now of
        Time when Time > 0 ->
            % Save a minimum running time
            erlang:send_after(Time, self(), nkdomain_destroy);
        _ ->
            % Process any remaining message
            self() ! nkdomain_destroy
    end,
    noreply(State3).




%% @private
is_stopped(#state{session=#obj_session{status={stopped, _Reason}}}) ->
    true;
is_stopped(_) ->
    false.


%% @private
do_check_create_child(ObjType, ObjPath, #state{srv_id=SrvId}) ->
    case nkdomain_util:get_parts(ObjType, ObjPath) of
        {ok, _Path, ObjName} ->
            case SrvId:object_store_find_path(SrvId, ObjPath) of
                {error, object_not_found} ->
                    ok;
                {ok, _, _} ->
                    {error, {name_is_already_used, ObjName}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_check_child(ObjType, ObjPath, State) ->
    #state{session=Session, childs=Childs} = State,
    #obj_session{path=Path} = Session,
    case nkdomain_util:get_parts(ObjType, ObjPath) of
        {ok, Path, ObjName} ->
            case lists:keymember({ObjType, ObjName}, #child.type_name, Childs) of
                true ->
                    {error, {name_is_already_used, ObjName}};
                false ->
                    Data = #{enabled=>Session#obj_session.enabled},
                    {ok, ObjName, Data}
            end;
        {ok, Path2, ObjName2} ->
            ?LLOG(notice, "error trying to check invalid child path (~s): ~s ~s:~s",
                  [ObjPath, Path2, ObjType, ObjName2], State),
            {error, invalid_object_path};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_update(Update, #state{srv_id=SrvId, session=Session}=State) ->
    #obj_session{type=Type, module=Module, obj=Obj}=Session,
    case SrvId:object_parse(SrvId, update, Type, Update) of
        {ok, Module, Update2} ->
            case ?ADD_TO_OBJ_DEEP(Update2, Obj) of
                Obj ->
                    {ok, State};
                Obj2 ->
                    Session2 = Session#obj_session{obj=Obj2, is_dirty=true},
                    {ok, State2} = handle(object_updated, [Update], State#state{session=Session2}),
                    State3 = do_save(State2),
                    {ok, do_event({updated, Update}, State3)}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


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


%% @private
do_call(Id, Msg) ->
    nkdomain_obj_lib:do_call(Id, Msg).

%% @private
do_cast(Id, Msg) ->
    nkdomain_obj_lib:do_cast(Id, Msg).