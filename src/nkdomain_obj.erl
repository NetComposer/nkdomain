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

-export([get_session/1, save/1, unload/2]).
-export([update/2, enable/2, delete/2, sync_op/2, async_op/2, apply/2]).
-export([register/2, unregister/2, get_childs/1]).
-export([wait_save/1, wait_save/2]).
-export([create_child/3, load_child/3, object_has_been_deleted/1]).
-export([start/3, init/1, terminate/2, code_change/3, handle_call/3,
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

-define(MIN_STARTED_TIME, 2000).
-define(MIN_FIRST_TIME, 60000).


-include("nkdomain.hrl").
-compile({no_auto_import, [register/2]}).

%% ===================================================================
%% Callbacks definitions
%% ===================================================================

-callback object_get_info() ->
    object_info().


-callback object_mapping() ->
    map() | disabled.


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
        type => nkdomain:type(),
        min_started_time => integer(),      %% msecs
        min_first_time => integer(),        %% msecs
        remove_after_stop => boolean()
    }.

-type session() :: #obj_session{}.

-type info() :: atom().

-type event() ::
    loaded |
    {updated, map()} |
    {enabled, boolean()} |
    {unloaded, nkservice:error()} |
    {info, info(), map()} |
    stopped.

-type start_meta() ::
    #{
        enabled => boolean(),
        is_dirty => boolean(),
        parent_id => nkdomain:id(),
        parent_pid => pid(),
        register => nklib:link()
    }.

-type status() ::
    init |
    {stopped, Reason::term()} |
    term().


%% ===================================================================
%% Public
%% ===================================================================

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
-spec enable(id(), boolean()) ->
    ok | {error, term()}.

enable(Id, Enabled) when is_boolean(Enabled)->
    do_call(Id, {nkdomain_enable, Enabled}).


%% @doc
-spec update(id(), map()) ->
    ok | {error, term()}.

update(Id, Map) ->
    do_call(Id, {nkdomain_update, Map}).


%% @doc
-spec sync_op(id(), term()) ->
    {ok, term()} | {error, term()}.

sync_op(Id, Op) ->
    do_call(Id, {nkdomain_sync_op, Op}).


%% @doc
-spec async_op(id(), term()) ->
    ok | {error, term()}.

async_op(Id, Op) ->
    do_cast(Id, {nkdomain_async_op, Op}).


%% @doc
-spec apply(id(), fun((session()) -> {ok, Reply::term()} | {ok, Reply::term(), session()} | {error, term()})) ->
    {ok, term()} | {error, term()}.

apply(Id, Fun) ->
    do_call(Id, {nkdomain_apply, Fun}).


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
    {ok, [{nkdomain:type(), nkdomain:obj_id(), nkdomain:name(), pid()}]} |
    {error, term()}.

get_childs(Id) ->
    do_call(Id, nkdomain_get_childs).


% @doc Waits for the object to be saved
-spec wait_save(id()) ->
    ok | {error, term()}.

wait_save(Id) ->
    wait_save(Id, 10000).


% @doc Waits for the object to be saved
-spec wait_save(id(), integer()) ->
    ok | {error, term()}.

wait_save(Id, Time) ->
    do_call(Id, nkdomain_wait_save, Time).


%% @private
create_child(Id, Obj, Meta) ->
    do_call(Id, {nkdomain_create_child, Obj, Meta}).


%% @private
load_child(Id, Obj, Meta) ->
    do_call(Id, {nkdomain_load_child, Obj, Meta}).


%% @private
object_has_been_deleted(Id) ->
    do_cast(Id, nkdomain_object_has_been_deleted).


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
-spec start(nkservice:id(), map(), start_meta()) ->
    {ok, pid()}.

start(SrvId, Obj, Meta) ->
    gen_server:start(?MODULE, {SrvId, Obj, Meta}, []).


-record(child, {
    obj_id :: nkdomain:obj_id(),
    type_name :: {Type::nkdomain:type(), Name::binary()},
    pid :: pid(),
    monitor :: reference()
}).

-record(state, {
    obj_id :: nkdomain:obj_id(),
    srv_id :: nkservice:id(),
    stop_reason = false :: false | nkservice:error(),
    links :: nklib_links:links(),
    session :: session(),
    started :: nklib_util:m_timestamp(),
    timer :: reference(),
    is_deleted = false :: boolean(),
    childs = [] :: [#child{}],
    timelog = [] :: [map()],
    wait_save = [] :: [{pid(), term()}]
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init({SrvId, Obj, Meta}) ->
    #{obj_id:=ObjId, type:=Type, path:=Path, parent_id:=ParentId} = Obj,
    Module = nkdomain_types:get_module(Type),
    false = Module==undefined,
    true = nklib_proc:reg({?MODULE, ObjId}, {Type, Path}),
    true = nklib_proc:reg({?MODULE, path, Path}, {Type, ObjId}),
    nklib_proc:put(?MODULE, {Type, ObjId, Path}),
    {ParentId, ParentPid} = case Meta of
        #{parent_id:=ParentId0, parent_pid:=ParentPid0} ->
            monitor(process, ParentPid0),
            {ParentId0, ParentPid0};
        _ when Type == ?DOMAIN_DOMAIN andalso ObjId == <<"root">> ->
            {<<>>, undefined}
    end,
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
        parent_id = ParentId,
        parent_pid = ParentPid,
        obj = Obj,
        srv_id = SrvId,
        status = init,
        meta = maps:without([srv_id, is_dirty, obj_id, parent_id, parent_pid], Meta),
        data = #{},
        is_dirty = maps:get(is_dirty, Meta, false),
        is_enabled = Enabled
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
             Info = Module:object_get_info(),
             case maps:get(min_first_time, Info, ?MIN_FIRST_TIME) of
                 MinTime when MinTime > 0 ->
                     erlang:send_after(MinTime, self(), nkdomain_check_childs);
                 _ ->
                     ok
             end,
             State1
    end,
    set_log(State2),
    nkservice_util:register_for_changes(SrvId),
    ?LLOG(info, "loaded (~p)", [self()], State2),
    gen_server:cast(self(), nkdomain_do_start),
    {ok, State3} = handle(object_init, [], State2),
    {ok, State3}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(nkdomain_get_session, _From, #state{session=Session}=State) ->
    reply({ok, Session}, State);

handle_call(nkdomain_get_timelog, _From, #state{timelog=Log}=State) ->
    reply({ok, Log}, State);

handle_call(nkdomain_get_childs, _From, #state{childs=Childs}=State) ->
    Data = lists:map(
        fun(#child{obj_id=ObjId, type_name={Type, Name}, pid=Pid}) ->
            {Type, ObjId, Name, Pid}
        end,
        Childs),
    reply({ok, Data}, State);

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

handle_call({nkdomain_delete, Reason}, From, State) ->
    case do_delete(Reason, State) of
        {ok, State2} ->
            gen_server:reply(From, ok),
            State3 = do_archive(State2),
            do_stop(object_deleted, State3);
        {error, Error, State2} ->
            reply({error, Error}, State2)
    end;

handle_call({nkdomain_enable, Enable}, _From, #state{session=#obj_session{obj=Obj}}=State) ->
    case maps:get(enabled, Obj, true) of
        Enable ->
            reply(ok, State);
        _ ->
            case do_update(#{enabled=>Enable}, State) of
                {ok, State2} ->
                    reply(ok, do_enabled(Enable, State2));
                {error, Error, State2} ->
                    reply({error, Error}, State2)
            end
    end;

handle_call({nkdomain_sync_op, Op}, From, State) ->
    case is_stopped(State) of
        true ->
            reply({error, object_is_stopped}, State);
        false ->
            case handle(object_sync_op, [Op, From], State) of
                {reply, Reply, #state{}=State2} ->
                    reply(Reply, State2);
                {reply_and_save, Reply, #state{}=State2} ->
                    reply(Reply, do_save(State2));
                {noreply, #state{}=State2} ->
                    noreply(State2);
                {noreply_and_save, #state{}=State2} ->
                    noreply(do_save(State2));
                {stop, Reason, Reply, #state{}=State2} ->
                    gen_server:reply(From, Reply),
                    do_stop(Reason, State2);
                {stop, Reason, #state{}=State2} ->
                    do_stop(Reason, State2);
                {continue, #state{}=State2} ->
                    ?LLOG(notice, "unknown async op: ~p", [Op], State2),
                    reply({error, unknown_op}, State2)
            end
    end;

handle_call({nkdomain_apply, Fun}, _From, #state{session=Session}=State) ->
    {Reply2, State2} = try Fun(Session) of
        {ok, Reply} ->
            {{ok, Reply}, State};
        {ok_and_save, Reply} ->
            {{ok, Reply}, do_save(State)};
        {ok, Reply, #obj_session{}=Session2} ->
            ?DEBUG("fun updated state", [], State),
            {{ok, Reply}, State#state{session=Session2}};
        {ok_and_save, Reply, #obj_session{}=Session2} ->
            ?DEBUG("fun updated state", [], State),
            {{ok, Reply}, do_save(State#state{session=Session2})};
        {error, Error} ->
            {{error, Error}, State}
    catch
        error:Error ->
            ?LLOG(warning, "error calling apply fun: ~p", [Error], State),
            {{error, internal_error}, State}
    end,
    reply(Reply2, State2);

handle_call({nkdomain_create_child, Obj, Meta}, From, State) ->
    #{type:=ChildType, path:=ChildPath} = Obj,
    ?DEBUG("creating child ~s (~s)", [ChildType, ChildPath], State),
    case do_check_create_path(ChildType, ChildPath, State) of
        ok ->
            handle_call({nkdomain_load_child, Obj, Meta#{is_dirty=>true}}, From, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({nkdomain_load_child, Obj, Meta}, _From, #state{srv_id=SrvId}=State) ->
    #{obj_id:=ChildId, type:=ChildType, path:=ChildPath} = Obj,
    ?DEBUG("loading child ~s (~s)", [ChildType, ChildPath], State),
    case do_check_child(ChildType, ChildPath, Meta, State) of
        {ok, ChildName, Meta2} ->
            case start(SrvId, Obj, Meta2) of
                {ok, ChildPid} ->
                    State2 = do_add_child(ChildType, ChildId, ChildName, ChildPid, State),
                    reply({ok, ChildPid}, State2);
                {error, Error} ->
                    ?LLOG(notice, "could not start child ~s:~s: ~p", [ChildType, ChildId, Error], State),
                    reply({error, could_not_start_child}, State)
            end;
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call(nkdomain_wait_save, _From, #state{session=#obj_session{is_dirty=false}}=State) ->
    {reply, ok, State};

handle_call(nkdomain_wait_save, From, #state{wait_save=Wait}=State) ->
    {noreply, State#state{wait_save=[From|Wait]}};

handle_call(nkdomain_get_state, _From, State) ->
    {reply, State, State};

handle_call(Msg, From, State) ->
    nklib_gen_server:handle_call(object_handle_call, Msg, From, State, #state.srv_id, #state.session).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(nkdomain_do_start, State) ->
    {ok, State2} = handle(object_start, [], State),
    case do_check_expire(State2) of
        false ->
            noreply(do_event(loaded, do_save(State2)));
        true ->
            do_stop(object_expired, State2)
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
                {noreply, #state{}=State2} ->
                    noreply(State2);
                {noreply_and_save, #state{}=State2} ->
                    noreply(do_save(State2));
                {stop, Reason, #state{}=State2} ->
                    do_stop(Reason, State2);
                {continue, #state{}=State2} ->
                    ?LLOG(notice, "unknown async op: ~p", [Op], State),
                    noreply(State2)
            end
    end;

handle_cast({nkdomain_parent_enabled, Enabled}, State) ->
    noreply(do_enabled(Enabled, State));

handle_cast({nkdomain_restart_timer, Time}, #state{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    NewTimer = erlang:start_timer(Time, self(), nkdomain_session_timeout),
    State#state{timer=NewTimer};

handle_cast({nkdomain_send_info, Info, Meta}, State) ->
    noreply(do_event({info, Info, Meta}, State));

handle_cast({nkdomain_register, Link}, State) ->
    noreply(links_add(Link, State));

handle_cast({nkdomain_unregister, Link}, State) ->
    State2 = links_remove(Link, State),
    do_check_links_down(State2);

handle_cast({nkdomain_unload, Error}, State) ->
    ?DEBUG("received stop: ~p", [Error], State),
    do_stop(Error, State);

%% Called from nkdomain_store
handle_cast(nkdomain_object_has_been_deleted, State) ->
    ?LLOG(notice, "received 'object deleted'", [], State),
    do_stop(object_deleted, State);

handle_cast({nkdomain_child_stopped, Pid}, State) ->
    case do_rm_child(Pid, State) of
        {true, State2} ->
            ?LLOG(notice, "received child ~p stopped", [Pid], State2),
            ?LLOG(notice, "childs: ~p", [State2#state.childs], State2),
            {noreply, State2};
        false ->
            {noreply, State}
    end;

handle_cast(Msg, State) ->
    nklib_gen_server:handle_cast(object_handle_cast, Msg, State, #state.srv_id, #state.session).


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

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{session=#obj_session{parent_pid=Pid}}=State) ->
    ?DEBUG("parent stopped", [], State),
    do_stop(parent_stopped, State);

handle_info({'DOWN', Ref, process, Pid, Reason}=Msg, State) ->
    #state{stop_reason=Stop} = State,
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
            case do_rm_child(Pid, State) of
                {true, State2} ->
                    noreply(State2);
                false ->
                    handle(object_handle_info, [Msg], State)
            end
    end;

handle_info(Msg, State) ->
    nklib_gen_server:handle_info(object_handle_info, Msg, State, #state.srv_id, #state.session).


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
    State4 = do_event(stopped, State3),
    {ok, _State5} = handle(object_terminate, [Reason], State4),
    ok.


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

do_save(#state{wait_save=Wait}=State) ->
    ?DEBUG("save object", [], State),
    case handle(object_save, [], State) of
        {ok, State2} ->
            lists:foreach(fun(From) -> gen_server:reply(From, ok) end, Wait),
            State2#state{wait_save=[]};
        {error, Error, State2} ->
            % Error will be managed by nkdomain_store
            lists:foreach(fun(From) -> gen_server:reply(From, {error, Error}) end, Wait),
            State2#state{wait_save=[]}
    end.


%% @private
do_delete(_Reason, #state{is_deleted=true}=State) ->
    {error, object_not_found, State};

do_delete(Reason, #state{childs=[], srv_id=SrvId}=State) ->
    case handle(object_delete, [], State) of
        {ok, State2} ->
            ?DEBUG("object deleted", [], State2),
            #state{session=#obj_session{obj=Obj}=Session} = State2,
            Obj2 = nkdomain_util:add_destroyed(SrvId, Reason, Obj),
            State3 = State2#state{
                session = Session#obj_session{obj=Obj2, is_dirty=true},
                is_deleted = true
            },
            {ok, State4} = handle(object_deleted, [Reason], State3),
            {ok, State4};
        {error, Error, State2} ->
            ?DEBUG("object NOT deleted: ~p", [Error], State2),
            {error, Error, State2}
    end;

do_delete(_Reason, State) ->
    {error, object_has_childs, State}.


%% @private
do_archive(State) ->
    #state{session = #obj_session{obj=Obj, module=Module}} = State,
    Info = Module:object_get_info(),
    case maps:get(archive, Info, true) of
        true ->
            case handle(object_archive, [Obj], State) of
                {ok, State2} ->
                    ?DEBUG("object archived", [], State2),
                    State2;
                {error, Error, State2} ->
                    ?DEBUG("object NOT archived: ~p", [Error], State2),
                    %% nkdomain_store will retry
                    State2
            end;
        false ->
            State
    end.


%% @private
do_stop(Reason, #state{srv_id=SrvId}=State) ->
    case is_stopped(State) of
        true ->
            noreply(State);
        false ->
            send_parent({nkdomain_child_stopped, self()}, State),
            {ok, State2} = handle(object_stop, [Reason], State#state{stop_reason=Reason}),
            {Code, Txt} = nkapi_util:api_error(SrvId, Reason),
            State3 = do_add_timelog(#{msg=>stopped, code=>Code, reason=>Txt}, State2),
            State4 = do_save(State3),
            % Give time for possible registrations to success and capture stop event
            State5 = do_status({stopped, Reason}, State4),
            do_stop2(Reason, State5)
    end.


%% @private
do_stop2(Reason, #state{started=Started}=State) ->
    State2 = do_event({unloaded, Reason}, State),
    #state{session = #obj_session{module=Module}} = State2,
    Info = Module:object_get_info(),
    State3 = case Info of
        #{remove_after_stop:=true} ->
            case do_delete(object_stopped, State2) of
                {ok, DeleteState} ->
                    do_archive(DeleteState);
                {error, _Error, DeleteState} ->
                    do_archive(DeleteState)
            end;
        _ ->
            State2
    end,
    Now = nklib_util:m_timestamp(),
    MinTime = maps:get(min_started_time, Info, ?MIN_STARTED_TIME),
    case (Started + MinTime) - Now of
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
do_check_create_path(ObjType, ObjPath, #state{srv_id=SrvId}=State) ->
    case nkdomain_util:get_parts(ObjType, ObjPath) of
        {ok, _Path, ObjName} ->
            case SrvId:object_store_find_obj(SrvId, ObjPath) of
                {error, object_not_found} ->
                    ok;
                {ok, _, _, _} ->
                    ?LLOG(notice, "cannot create child: path ~s exists", [ObjPath], State),
                    {error, {name_is_already_used, ObjName}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_check_child(ChildType, ChildPath, Meta, State) ->
    #state{obj_id=ParentId, session=Session, childs=Childs} = State,
    #obj_session{path=Path} = Session,
    case nkdomain_util:get_parts(ChildType, ChildPath) of
        {ok, Path, ChildName} ->
            case lists:keymember({ChildType, ChildName}, #child.type_name, Childs) of
                true ->
                    ?LLOG(info, "cannnot load child, name ~s/~s already used",
                          [ChildType, ChildName], State),
                    {error, {name_is_already_used, ChildName}};
                false ->
                    Meta2 = Meta#{
                        parent_id => ParentId,
                        parent_pid => self(),
                        enabled => Session#obj_session.is_enabled
                    },
                    {ok, ChildName, Meta2}
            end;
        {ok, Path2, ChildName2} ->
            ?LLOG(notice, "error trying to check invalid child path (~s): ~s ~s:~s",
                  [ChildPath, Path2, ChildType, ChildName2], State),
            {error, {invalid_object_path, Path}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_add_child(ChildType, ChildId, ChildName, ChildPid, #state{childs=Childs}=State) ->
    Child = #child{
        obj_id = ChildId,
        type_name = {ChildType, ChildName},
        pid = ChildPid,
        monitor = monitor(process, ChildPid)
    },
    Childs2 = [Child|Childs],
    State#state{childs=Childs2}.


%% @private
do_rm_child(Pid, #state{childs=Childs}=State) ->
    case lists:keytake(Pid, #child.pid, Childs) of
        {value, #child{type_name={Type, Name}, obj_id=ObjId, monitor=Mon}, Rest} ->
            demonitor(Mon),
            ?DEBUG("child ~s:~s (~s) stopped", [Type, Name, ObjId], State),
            {true, State#state{childs=Rest}};
        false ->
            false
    end.


%% @private
do_update(Update, #state{srv_id=SrvId, session=Session}=State) ->
    #obj_session{type=Type, obj=Obj}=Session,
    case SrvId:object_parse(SrvId, update, Type, Update) of
        {ok, Update2} ->
            case ?ADD_TO_OBJ_DEEP(Update2, Obj) of
                Obj ->
                    {ok, State};
                Obj2 ->
                    Session2 = Session#obj_session{obj=Obj2, is_dirty=true},
                    State2 = do_save(State#state{session=Session2}),
                    {ok, State3} = handle(object_updated, [Update], State2),
                    {ok, do_event({updated, Update}, State3)}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @private
do_enabled(Enabled, #state{session=#obj_session{is_enabled=Enabled}}=State) ->
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
    Session2 = Session#obj_session{is_enabled=Enabled},
    {ok, State2} = handle(object_enabled, [], State#state{session=Session2}),
    lists:foreach(
        fun(#child{pid=Pid}) -> gen_server:cast(Pid, {nkdomain_parent_enabled, Enabled}) end,
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
do_check_links_down(#state{childs=Childs}=State) when Childs /= [] ->
    noreply(State);

do_check_links_down(State) ->
    case links_is_empty(State) of
        true ->
            case handle(object_all_links_down, [], State) of
                {keepalive, State2} ->
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
send_parent(Msg, #state{session=#obj_session{parent_pid=Pid}}) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);
send_parent(_Msg, _State) ->
    ok.


%% @private
do_add_timelog(Msg, State) when is_atom(Msg); is_binary(Msg) ->
    do_add_timelog(#{msg=>Msg}, State);

do_add_timelog(#{msg:=_}=Data, #state{started=Started, timelog=Log}=State) ->
    Time = nklib_util:m_timestamp() - Started,
    State#state{timelog=[Data#{time=>Time}|Log]}.



%% @private
links_add(Link, #state{links=Links}=State) ->
    ?DEBUG("registered link (~p)", [Link], State),
    State#state{links=nklib_links:add(Link, Links)}.


%%%% @private
%%links_add(Link, Pid, #state{links=Links}=State) ->
%%    State#state{links=nklib_links:add(Link, none, Pid, Links)}.


%% @private
links_remove(Link, #state{links=Links}=State) ->
    ?DEBUG("proc unregistered (~p)", [Link], State),
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
    nkdomain_obj_lib:call(Id, Msg).

%% @private
do_call(Id, Msg, Time) ->
    nkdomain_obj_lib:call(Id, Msg, Time).

%% @private
do_cast(Id, Msg) ->
    nkdomain_obj_lib:cast(Id, Msg).