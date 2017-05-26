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

-module(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_session/1, save/1, unload/2]).
-export([update/2, enable/2, get_name/1, delete/1, sync_op/2, async_op/2, is_enabled/1, apply/2]).
-export([register/2, unregister/2, link/4, unlink/4, send_info/3, send_event/2, get_childs/1]).
-export([wait_for_save/2]).
-export([create_child/3, load_child/3, object_has_been_deleted/1]).
-export([start/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,  handle_cast/2, handle_info/2]).
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
-define(RELOAD_PARENT_TIME, 1000).
-define(MOVE_WAIT_TIME, 30000).

-include("nkdomain.hrl").
-compile({no_auto_import, [register/2]}).


%% ===================================================================
%% Callbacks definitions
%% ===================================================================

-type state() :: map().


-callback object_get_info() ->
    object_info().


-callback object_mapping() ->
    map() | disabled.


-callback object_parse(nkservice:id(), load|update, Obj::map()) ->
    {ok, nkdomain:obj()} | {error, term()} | nklib_syntax:syntax() | {type_obj, map()}.


-callback object_api_syntax(Cmd::binary(), nklib_syntax:syntax()) ->
    nklib_syntax:syntax() | continue.


-callback object_api_allow(Cmd::binary(), nkservice:req(), state()) ->
    {boolean, state()} | {true, nkservice:req(), state()}.


-callback object_api_cmd(Cmd::binary(), nkservice:req(), state()) ->
    {ok, map(), state()} |
    {ack, state()} |
    {login, Reply::term(), User::binary(), Meta::map(), state()} |
    {error, nkapi:error(), state()}.




%% ===================================================================
%% Types
%% ===================================================================


-type id() ::
    nkdomain:obj_id() | {nkservice:id(), nkdomain:path()} | pid().

-type object_info() ::
    #{
        type => nkdomain:type(),
        permanent => boolean(),
        min_first_time => integer(),                    %% msecs
        remove_after_stop => boolean(),
        dont_create_childs_on_disabled => boolean(),    %% Default false
        dont_update_on_disabled => boolean(),           %% Default false
        dont_delete_on_disabled => boolean(),           %% Default false
        default_token_ttl => integer(),
        max_token_ttl => integer()
    }.

-type session() :: #obj_session{}.

-type info() :: atom().

-type event() ::
    created |
    loaded |
    {status, status()} |
    saved |
    {updated, map()} |
    deleted |
    {enabled, boolean()} |
    {child_created, nkdomain:type(), nkdomain:obj_id()} |
    {child_loaded, nkdomain:type(), nkdomain:obj_id()} |
    {child_unloaded, nkdomain:type(), nkdomain:obj_id()} |
    {info, info(), map()} |
    {unloaded, nkservice:error()}.


-type status() ::
    loaded |
    {unloaded, nkservice:error()} |
    term().

-type start_meta() ::
    #{
        enabled => boolean(),
        register => nklib:link(),
        obj => nkdomain:obj()
    }.


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
-spec delete(id()) ->
    ok | {error, term()}.

delete(Id) ->
    do_call(Id, nkdomain_delete).


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
    {ok, UnknownFields::[binary()]} | {error, term()}.

update(Id, Map) ->
    do_call(Id, {nkdomain_update, Map}).


%% @doc
-spec get_name(id()) ->
    {ok, map()} | {error, term()}.

get_name(Id) ->
    do_call(Id, nkdomain_get_name).


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


%% @doc
-spec send_info(id(), atom(), map()) ->
    ok | {error, term()}.

send_info(Id, Info, Body) when is_map(Body) ->
    do_cast(Id, {nkdomain_send_info, Info, Body}).


%% @doc
-spec send_event(id(),  term()) ->
    ok | {error, term()}.

send_event(Id, Event) ->
    do_cast(Id, {nkdomain_send_event, Event}).


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
link(Pid, Type, ObjOrPid, Tag) when is_pid(Pid) andalso (Type==usage orelse Type==event) ->
    nkdomain_obj_lib:link_to_obj(Type, ObjOrPid, Pid, Tag);

link(Id, Type, ObjOrPid, Tag) when Type==usage; Type==event ->
    case nkdomain_obj_lib:find_loaded(Id) of
        #obj_id_ext{pid=Pid} ->
            link(Pid, Type, ObjOrPid, Tag);
        not_found ->
            {error, object_not_found}
    end.


%% @doc
unlink(Pid, Type, ObjOrPid, Tag) when is_pid(Pid) andalso (Type==usage orelse Type==event) ->
    nkdomain_obj_lib:unlink_to_obj(Type, ObjOrPid, Pid, Tag);

unlink(Id, Type, ObjOrPid, Tag) when Type==usage; Type==event ->
    case nkdomain_obj_lib:find_loaded(Id) of
        #obj_id_ext{pid=Pid} ->
            unlink(Pid, Type, ObjOrPid, Tag);
        not_found ->
            {error, object_not_found}
    end.


% @doc
-spec get_childs(id()) ->
    {ok, [{nkdomain:type(), nkdomain:obj_id(), nkdomain:name(), pid()}]} |
    {error, term()}.

get_childs(Id) ->
    do_call(Id, nkdomain_get_childs).


% @doc Waits for the object to be saved
-spec wait_for_save(id(), integer()) ->
    ok | {error, term()}.

wait_for_save(Id, Time) ->
    do_call(Id, nkdomain_wait_for_save, Time).


% @doc
-spec is_enabled(id()) ->
    {ok, boolean()} | {error, term()}.

is_enabled(Id) ->
    do_call(Id, nkdomain_is_enabled).


%% @private
create_child(Id, Obj, Meta) ->
    do_call(Id, {nkdomain_create_child, Obj, Meta}).


%% @private
load_child(Id, ObjIdExt, Meta) ->
    do_call(Id, {nkdomain_load_child, ObjIdExt, Meta}).


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



%% @private
-spec start(#obj_id_ext{}, start_meta()) ->
    {ok, pid()} | {error, term()}.

start(#obj_id_ext{obj_id=ObjId}=ObjIdExt, Meta) ->
    case nkdomain_obj_lib:get_node(ObjId) of
        {ok, Node} ->
            case rpc:call(Node, gen_server, start, [?MODULE, {ObjIdExt, Meta}, []]) of
                {ok, Pid} -> {ok, Pid};
                {error, {already_registered, Pid}} -> {ok, Pid};
                {error, Error} -> {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    obj_id :: nkdomain:obj_id(),
    srv_id :: nkservice:id(),
    stop_reason = false :: false | nkservice:error(),
    session :: session(),
    timer :: reference(),
    timelog = [] :: [map()],
    wait_save = [] :: [{pid(), term()}],
    obj_info :: map(),
    moved_to :: undefined | pid()
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init({#obj_id_ext{type=Type, obj_id=ObjId, path=Path}=ObjIdExt, Meta}) ->
    case nkdomain_obj_lib:register(Type, ObjId, Path) of
        ok ->
            do_init(ObjIdExt, Meta);
        {error, {pid_conflict, Pid}} ->
            lager:error("Already registered!"),
            {stop, {already_registered, Pid}};
        {error, Error} ->
            {stop, Error}
    end;

% Moved object
init({#state{srv_id=SrvId, session=Session}=State, Time, OldPid}) ->
    #obj_session{type=Type, obj_id=ObjId, path=Path, module=Module} = Session,
    case nkdomain_obj_lib:register(Type, ObjId, Path, OldPid) of
        ok ->
            nklib_proc:put(?MODULE, {Type, ObjId, Path}),
            ObjIdExt = #obj_id_ext{srv_id=SrvId, obj_id=ObjId, path=Path, pid=self()},
            ok = nkdomain_type:register(Module, ObjIdExt),
            Timer = case Time of
                permanent ->
                    undefined;
                _ ->
                    erlang:send_after(Time, self(), nkdomain_timer)
            end,
            set_log(State),
            nkservice_util:register_for_changes(SrvId),
            {ok, State#state{timer=Timer}};
        {error, Error} ->
            {stop, Error}
    end.


%% @private
do_init(ObjIdExt, Meta) ->
    #obj_id_ext{srv_id=SrvId, obj_id=ObjId, type=Type, path=Path} = ObjIdExt,
    nklib_proc:put(?MODULE, {Type, ObjId, Path}),
    Module = nkdomain_all_types:get_module(Type),
    false = Module==undefined,
    ok = nkdomain_type:register(Module, ObjIdExt#obj_id_ext{pid=self()}),
    {Name, ParentId} = case Path of
        <<"/">> when Type == ?DOMAIN_DOMAIN andalso ObjId == <<"root">> ->
            {<<>>, <<>>};
        _ ->
            {ok, Base, Name0} = nkdomain_util:get_parts(Type, Path),
            #obj_id_ext{obj_id=ParentId0} = nkdomain_obj_lib:load(SrvId, Base, #{}),
            % We will receive nkdist_reg messages if parent goes down
            ok = nkdomain_obj_lib:link_to_parent(Base, Type, Name0, ObjId),
            {Name0, ParentId0}
    end,
    {Obj, IsCreated} = case Meta of
        #{obj:=Obj0} ->
            {Obj0, true};
        _ ->
            {#{}, false}
    end,
    Enabled = case maps:find(enabled, Meta) of
        {ok, false} ->
            false;
        _ ->
            maps:get(enabled, Obj, true)
    end,
    IgnoreMeta = [enabled, obj, event_link, usage_link],
    Session = #obj_session{
        obj_id = ObjId,
        module = Module,
        path = Path,
        type = Type,
        name = Name,
        parent_id = ParentId,
        obj = Obj,
        srv_id = SrvId,
        status = init,
        meta = maps:without(IgnoreMeta, Meta),
        data = #{},
        is_dirty = IsCreated,
        is_enabled = Enabled,
        is_created = IsCreated,
        childs = #{},
        started = nkdomain_util:timestamp()
    },
    Info = Module:object_get_info(),
    State1 = #state{
        obj_id = ObjId,
        srv_id = SrvId,
        session = Session,
        obj_info = Info
    },
    State2 = case maps:get(permanent, Info, false) of
        true ->
            State1;
        false ->
            case do_check_expire(State1) of
                undefined ->
                    Time = maps:get(min_first_time, Info, ?MIN_FIRST_TIME),
                    Ref = erlang:send_after(Time, self(), nkdomain_timer),
                    State1#state{timer=Ref};
                _ ->
                    State1
            end
    end,
    case Meta of
        #{usage_link:={Id1, Tag1}} ->
            % We will receive {received_link, Tag}
            % If they die, we receive {received_link_down, Tag}
            % If we die, they receive {sent_link_down, Tag}
            ok = nkdomain_obj_lib:link_to_obj(usage, Id1, self(), Tag1);
        _ ->
            ok
    end,
    case Meta of
        #{event_link:={Id2, Tag2}} ->
            ok = nkdomain_obj_lib:link_to_obj(event, Id2, self(), Tag2);
        _ ->
            ok
    end,
    set_log(State2),
    nkservice_util:register_for_changes(SrvId),
    ?DEBUG("loaded (~p)", [self()], State2),
    {ok, State3} = handle(object_init, [], State2),
    case IsCreated of
        false ->
            gen_server:cast(self(), nkdomain_do_load);
        true ->
            ok
    end,
    gen_server:cast(self(), nkdomain_do_start),
    {ok, State3}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(Msg, _From, #state{moved_to=Pid}=State) when is_pid(Pid) ->
    ?DEBUG("forwarding call to new process", [], State),
    Reply = gen_server:call(Pid, Msg, infinity),
    reply(Reply, State);

handle_call(nkdomain_get_session, _From, State) ->
    {ok, #state{session=Session2}=State2} = handle(object_get_session, [], State),
    reply({ok, Session2}, State2);

handle_call(nkdomain_get_timelog, _From, #state{timelog=Log}=State) ->
    reply({ok, Log}, State);

handle_call(nkdomain_get_childs, _From, #state{session=#obj_session{childs=Childs}}=State) ->
    reply({ok, Childs}, State);

handle_call({nkdomain_update, _Map}, _From,
    #state{session=#obj_session{is_enabled=false}, obj_info=#{dont_update_on_disabled:=true}}=State) ->
    reply({error, object_is_disabled}, State);

handle_call({nkdomain_update, Map}, _From, State) ->
    case do_update(Map, State) of
        {ok, UnknownFields, State2} ->
            reply({ok, UnknownFields}, State2);
        {error, Error, State2} ->
            reply({error, Error}, State2)
    end;

handle_call(nkdomain_delete, _From,
    #state{session=#obj_session{is_enabled=false}, obj_info=#{dont_delete_on_disabled:=true}}=State) ->
    reply({error, object_is_disabled}, State);

handle_call(nkdomain_delete, From, State) ->
    case do_delete(State) of
        {ok, State2} ->
            gen_server:reply(From, ok),
            State3 = do_archive(object_deleted, State2),
            do_stop(object_deleted, State3);
        {error, Error, State2} ->
            reply({error, Error}, State2)
    end;

handle_call({nkdomain_enable, Enable}, From, #state{session=#obj_session{obj=Obj}}=State) ->
    case maps:get(enabled, Obj, true) of
        Enable ->
            reply(ok, State);
        _ ->
            case do_update(#{enabled=>Enable}, State) of
                {ok, [], State2} ->
                    gen_server:reply(From, ok),
                    do_enabled(Enable, State2);
                {error, Error, State2} ->
                    reply({error, Error}, State2)
            end
    end;

handle_call(nkdomain_get_name, _From, #state{session=Session}=State) ->
    Reply = nkdomain_obj_util:get_name(Session),
    reply({ok, Reply}, State);

handle_call(nkdomain_is_enabled, _From, #state{session=#obj_session{is_enabled=Enabled}}=State) ->
    reply({ok, Enabled}, State);

handle_call(nkdomain_get_time, _From, State) ->
    reply({ok, get_timer(State)}, State);

handle_call({nkdomain_sync_op, _Op}, _From, #state{session=#obj_session{is_enabled=false}}=State) ->
    reply({error, object_is_disabled}, State);

handle_call({nkdomain_sync_op, Op}, From, State) ->
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
            ?LLOG(notice, "unknown sync op: ~p", [Op], State2),
            reply({error, unknown_op}, State2)
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

handle_call({nkdomain_create_child, _Obj, _Meta}, _From,
    #state{session=#obj_session{is_enabled=false}, obj_info=#{dont_create_childs_on_disabled:=true}}=State) ->
    reply({error, object_is_disabled}, State);

handle_call({nkdomain_create_child, Obj, Meta}, _From, #state{srv_id=SrvId}=State) ->
    #{type:=Type, obj_id:=ObjId, path:=Path} = Obj,
    ObjIdExt = #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path},
    ?DEBUG("creating child ~s", [Path], State),
    Skip = maps:get(skip_path_check, Meta, false),
    case do_check_child(ObjIdExt, State) of
        ok when Skip ->
            State2 = do_event({child_created, Type, ObjId}, State),
            do_load_child(ObjIdExt, Meta#{obj=>Obj}, State2);
        ok ->
            case do_check_create_path(Path, State) of
                ok ->
                    State2 = do_event({child_created, Type, ObjId}, State),
                    do_load_child(ObjIdExt, Meta#{obj=>Obj}, State2);
                {error, Error} ->
                    reply({error, Error}, State)
            end;
        {error, object_is_already_loaded} ->
            reply({error, object_already_exists}, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call({nkdomain_load_child, ObjIdExt, Meta}, _From, State) ->
    #obj_id_ext{path=Path} = ObjIdExt,
    ?DEBUG("loading child ~s", [Path], State),
    % Check name is not present and has correct base path
    case do_check_child(ObjIdExt, State) of
        ok ->
            #state{session=#obj_session{is_enabled=Enabled}} = State,
            do_load_child(ObjIdExt, Meta#{enabled=>Enabled}, State);
        {error, Error} ->
            reply({error, Error}, State)
    end;

handle_call(nkdomain_wait_for_save, _From,
            #state{session=#obj_session{is_dirty=false}}=State) ->
    reply(ok, State);

handle_call(nkdomain_wait_for_save, From, #state{wait_save=Wait}=State) ->
    noreply(State#state{wait_save=[From|Wait]});

handle_call(nkdomain_get_state, _From, State) ->
    reply(State, State);

handle_call(Msg, From, State) ->
    nklib_gen_server:handle_call(object_handle_call, Msg, From, State, #state.srv_id, #state.session).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(Msg, #state{moved_to=Pid}=State) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg),
    noreply(State);

handle_cast(nkdomain_do_load, #state{srv_id=SrvId, obj_id=ObjId, session=Session}=State) ->
    case SrvId:object_load(SrvId, ObjId) of
        {ok, Obj, UnknownFields} ->
            case UnknownFields of
                [] ->
                    ok;
                _ ->
                    ?LLOG(notice, "Unknown fields loading object: ~p", [UnknownFields], State)
            end,
            #obj_session{is_enabled=Enabled} = Session,
            Enabled2 = case Enabled of
                false ->
                    false;
                true ->
                    maps:get(enabled, Obj, true)
            end,
            Session2 = Session#obj_session{obj=Obj, is_enabled=Enabled2},
            noreply(State#state{session=Session2});
        {error, Error} ->
            do_stop({obj_load_error, Error}, State)
    end;

handle_cast(nkdomain_do_start, State) ->
    {ok, State2} = handle(object_start, [], State),
    case do_check_expire(State2) of
        true ->
            do_stop(object_expired, State2);
        _ ->
            State3 = case State2 of
                #state{session=#obj_session{is_created=true}} ->
                    do_event(created, State2);
                _ ->
                    State2
            end,
            State4 = do_event(loaded, State3),
            State5 = do_save(State4),
            noreply(State5)
    end;

handle_cast({nkdomain_add_timelog, Data}, State) ->
    noreply(do_add_timelog(Data, State));

handle_cast(nkdomain_save, State) ->
    noreply(do_save(State));

handle_cast({nkdomain_async_op, Op}, #state{session=#obj_session{is_enabled=false}}=State) ->
    ?DEBUG("skipping async op '~p' on disabled object", [Op], State),
    noreply(State);

handle_cast({nkdomain_async_op, Op}, State) ->
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
    end;

handle_cast({nkdomain_parent_enabled, Enabled}, State) ->
    do_enabled(Enabled, State);

handle_cast({nkdomain_send_info, Info, Meta}, State) ->
    noreply(do_event({info, Info, Meta}, State));

handle_cast({nkdomain_send_event, Event}, State) ->
    noreply(do_event(Event, State));

handle_cast({nkdomain_unload, Error}, State) ->
    ?DEBUG("received unload: ~p", [Error], State),
    do_stop(Error, State);

%% Called from nkdomain_store
handle_cast(nkdomain_object_has_been_deleted, State) ->
    ?LLOG(info, "received 'object has been deleted'", [], State),
    do_stop(object_deleted, State);

handle_cast(Msg, State) ->
    nklib_gen_server:handle_cast(object_handle_cast, Msg, State, #state.srv_id, #state.session).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info(nkdomain_move_completed, State) ->
    ?DEBUG("move completed", [], State),
    {stop, normal, State};

handle_info({nkservice_updated, _SrvId}, State) ->
    noreply(set_log(State));

handle_info(nkdomain_check_expire, State) ->
    case do_check_expire(State) of
        false ->
            noreply(State);
        true ->
            do_stop(object_expired, State)
    end;

handle_info(nkdomain_timer, State) ->
    do_check_links_down(State);

handle_info(nkdomain_destroy, State) ->
    {stop, normal, State};

handle_info(nkdomain_find_parent, #state{session=Session}=State) ->
    #obj_session{srv_id=SrvId, obj_id=ObjId, type=Type, name=Name, parent_id=ParentId} = Session,
    case nkdomain_obj_lib:load(SrvId, ParentId, #{}) of
        #obj_id_ext{} ->
            case nkdomain_obj_lib:link_to_parent(ParentId, Type, Name, ObjId) of
                ok ->
                    ?LLOG(notice, "parent loaded again", [], State);
                {error, Error} ->
                    ?LLOG(notice, "object could not reload parent: ~p", [Error], State),
                    erlang:send_after(?RELOAD_PARENT_TIME, self(), nkdomain_find_parent)
            end;
        {error, Error} ->
            ?LLOG(notice, "object could not reload parent: ~p", [Error], State),
            erlang:send_after(?RELOAD_PARENT_TIME, self(), nkdomain_find_parent)
    end,
    noreply(State);

handle_info({nkdist, Msg}, State) ->
    do_nkdist(Msg, State);

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

terminate(_Reason, #state{moved_to=Pid}) when is_pid(Pid) ->
    ok;

terminate(Reason, State) ->
    State2 = do_stop2({terminate, Reason}, State),
    {ok, _State3} = handle(object_terminate, [Reason], State2),
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
    % lager:notice("DEBUG: ~p", [Debug]),
    put(object_debug, Debug),
    State.


%% @private
do_check_expire(#state{session=#obj_session{obj=Obj}}) ->
    case maps:get(expires_time, Obj, 0) of
        0 ->
            undefined;
        Expires ->
            case nkdomain_util:timestamp() of
                Now when Now >= Expires ->
                    true;
                Now ->
                    Remind = min(3600000, Expires - Now),
                    erlang:send_after(Remind, self(), nkdomain_check_expire),
                    false
            end
    end.


%% @private
do_load_child(#obj_id_ext{type=Type, obj_id=ObjId}=ObjIdExt, Meta, State) ->
    Meta2 = maps:without([skip_path_check], Meta),
    case start(ObjIdExt, Meta2) of
        {ok, ChildPid} ->
            % Wait for registration event
            State2 = do_event({child_loaded, Type, ObjId}, State),
            reply({ok, ChildPid}, State2);
        {error, Error} ->
            #obj_id_ext{path=Path} = ObjIdExt,
            ?LLOG(notice, "could not start child ~s: ~p", [Path, Error], State),
            reply({error, could_not_start_child}, State)
    end.


%% @private
do_save(#state{session=#obj_session{is_dirty=false}}=State) ->
    State;

do_save(#state{wait_save=Wait}=State) ->
    ?DEBUG("save object", [], State),
    case handle(object_save, [], State) of
        {ok, State2} ->
            lists:foreach(fun(From) -> gen_server:reply(From, ok) end, Wait),
            do_event(saved, State2#state{wait_save=[]});
        {error, Error, State2} ->
            % Error will be managed by nkdomain_store
            lists:foreach(fun(From) -> gen_server:reply(From, {error, Error}) end, Wait),
            State2#state{wait_save=[]}
    end.


%% @private
do_delete(#state{session=#obj_session{childs=Childs}}=State) when map_size(Childs)==0 ->
    case handle(object_delete, [], State) of
        {ok, State2} ->
            ?DEBUG("object deleted", [], State2),
            {ok, do_event(deleted, State2)};
        {error, Error, State2} ->
            ?DEBUG("object NOT deleted: ~p", [Error], State2),
            {error, Error, State2}
    end;

do_delete(State) ->
    {error, object_has_childs, State}.


%% @private
do_archive(Reason, State) ->
    #state{srv_id=SrvId, session=#obj_session{obj=Obj}=Session, obj_info=Info} = State,
    case maps:get(archive, Info, true) of
        true ->
            Obj2 = nkdomain_util:add_destroyed(SrvId, Reason, Obj),
            Session2 = Session#obj_session{obj=Obj2},
            case handle(object_archive, [], State#state{session=Session2}) of
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
do_stop(Reason, State) ->
    {stop, normal, do_stop2(Reason, State)}.


%% @private
do_stop2(Reason, #state{srv_id=SrvId, stop_reason=false, timelog=Log, obj_info=Info}=State) ->
    {ok, State2} = handle(object_stop, [Reason], State#state{stop_reason=Reason}),
    {Code, Txt} = nkservice_util:error(SrvId, Reason),
    State3 = do_add_timelog(#{msg=>stopped, code=>Code, reason=>Txt}, State2),
    State4 = do_save(State3),
    State5 = do_event({unloaded, Reason}, State4),
    State6 = do_event({record, lists:reverse(Log)}, State5),
    case Info of
        #{remove_after_stop:=true} ->
            case do_delete(State6) of
                {ok, DeleteState} ->
                    do_archive(Reason, DeleteState);
                {error, _Error, DeleteState} ->
                    do_archive(Reason, DeleteState)
            end;
        _ ->
            State6
    end;

do_stop2(_Reason, State) ->
    State.


%% @private
do_check_create_path(ObjPath, #state{srv_id=SrvId}=State) ->
    case SrvId:object_store_find_obj(SrvId, ObjPath) of
        {error, object_not_found} ->
            ok;
        {ok, _, _, _} ->
            ?LLOG(notice, "cannot create child: path ~s exists", [ObjPath], State),
            {error, object_already_exists};
        {error, Error} ->
            {error, Error}
    end.


%% @private Checks has correct path and is not already present
do_check_child(#obj_id_ext{type=Type, path=Path}, #state{session=Session}=State) ->
    #obj_session{path=Base, childs=Childs} = Session,
    case nkdomain_util:get_parts(Type, Path) of
        {ok, Base, Name} ->
            TypeChilds = maps:get(Type, Childs, #{}),
            case maps:is_key(Name, TypeChilds) of
                true ->
                    ?LLOG(notice, "cannnot load child, ~s is already loaded", [Path], State),
                    {error, object_is_already_loaded};
                false ->
                    ok
            end;
        {ok, Base2, _Name} ->
            ?LLOG(notice, "cannnot load chil, invalid base ~s", [Base2], State),
            {error, {invalid_object_path, Path}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_add_child(Type, ObjId, Name, #state{session=Session}=State) ->
    #obj_session{childs=Childs} = Session,
    TypeChilds1 = maps:get(Type, Childs, #{}),
    TypeChilds2 = TypeChilds1#{Name => ObjId},
    Childs2 = Childs#{Type => TypeChilds2},
    Session2 = Session#obj_session{childs=Childs2},
    State#state{session=Session2}.


%% @private
do_rm_child(Type, Name, #state{session=Session}=State) ->
    #obj_session{childs=Childs} = Session,
    TypeChilds1 = maps:get(Type, Childs),
    TypeChilds2 = maps:remove(Name, TypeChilds1),
    Childs2 = case map_size(TypeChilds2) of
        0 ->
            maps:remove(Type, Childs);
        _ ->
            Childs#{Type => TypeChilds2}
    end,
    Session2 = Session#obj_session{childs=Childs2},
    State#state{session=Session2}.


%% @private
do_update(Update, #state{srv_id=SrvId, session=Session}=State) ->
    #obj_session{obj=Obj, obj_id=_ObjId, path=_Path, type=Type}=Session,
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
                            Session2 = Session#obj_session{obj=Obj5, is_dirty=true},
                            State2 = do_save(State#state{session=Session2}),
                            {ok, UnknownFields, do_event({updated, Update3}, State2)};
                        {error, Error} ->
                            {error, Error}
                    end
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @private Sets an enabled state at the object
do_enabled(Enabled, #state{session=#obj_session{is_enabled=Enabled}}=State) ->
    noreply(State);

do_enabled(false, State) ->
    do_enabled2(false, State);

do_enabled(true, #state{session=#obj_session{obj=Obj}}=State) ->
    case maps:get(enabled, Obj, true) of
        true ->
            do_enabled2(true, State);
        false ->
            noreply(State)
    end.


%% @private
do_enabled2(Enabled, #state{session=Session}=State) ->
    State2 = State#state{session=Session#obj_session{is_enabled=Enabled}},
    send_childs({nkdomain_parent_enabled, Enabled}, State2),
    noreply(do_event({enabled, Enabled}, State2)).


%% @private
do_event(Event, #state{session=Session}=State) ->
    ?DEBUG("sending 'event': ~p", [Event], State),
    Session2 = nkdomain_obj_util:event(Event, Session),
    State#state{session=Session2}.


%% @private
do_check_links_down(#state{obj_info=#{permanent:=true}}=State) ->
    noreply(State);

do_check_links_down(#state{session=#obj_session{childs=Childs}}=State) when map_size(Childs) > 0 ->
    noreply(State);

do_check_links_down(#state{session=#obj_session{link_usages=Usages}}=State) ->
    case maps:size(Usages) of
        0 ->
            do_stop(no_usages, State);
        _ ->
            noreply(State)
    end.


%% @private
do_nkdist({received_link, {nkdomain_child, Type, Name, ChildId}}, State) ->
    ?DEBUG("child ~s:~s registered with us", [Type, Name], State),
    #state{session = #obj_session{is_enabled = Enabled}} = State,
    nkdomain_obj_lib:cast(ChildId, {nkdomain_parent_enabled, Enabled}),
    State2 = do_add_child(Type, ChildId, Name, State),
    noreply(State2);

do_nkdist({received_link_down, {nkdomain_child, Type, Name, ChildId}}, State) ->
    ?DEBUG("child ~s:~s has stopped", [Type, Name], State),
    State2 = do_rm_child(Type, Name, State),
    State3 = do_event({child_unloaded, Type, ChildId}, State2),
    do_check_links_down(State3);

do_nkdist({received_link, {usage, Tag}}, State) ->
    ?DEBUG("new received usage link: ~p", [Tag], State),
    noreply(add_link(usage, Tag, State));

do_nkdist({received_link_down, {usage, Tag}}, State) ->
    ?DEBUG("usage link down: ~p", [Tag], State),
    State2 = remove_link(usage, Tag, State),
    do_check_links_down(State2);

do_nkdist({received_link, {event, Tag}}, State) ->
    ?DEBUG("new received event link: ~p", [Tag], State),
    noreply(add_link(event, Tag, State));

do_nkdist({received_link_down, {event, Tag}}, State) ->
    ?DEBUG("event link down: ~p", [Tag], State),
    noreply(remove_link(event, Tag, State));

do_nkdist({removed_link, Link}, State) ->
    do_nkdist({received_link_down, Link}, State);

do_nkdist({sent_link_down, {nkdomain_child, _Type, _Name, ObjId}}, #state{obj_id = ObjId} = State) ->
    ?LLOG(notice, "parent stopped!", [], State),
    self() ! nkdomain_find_parent,
    do_enabled(false, State);

do_nkdist({must_move, Node}, #state{timer = Timer, session = Session} = State) ->
    Time = get_timer(State),
    nklib_util:cancel_timer(Timer),
    case rpc:call(Node, gen_server, start, [?MODULE, {State, Time, self()}, []]) of
        {ok, NewPid} ->
            ?LLOG(info, "starting move to ~p (~p -> ~p)", [Node, self(), NewPid], State),
            erlang:send_after(?MOVE_WAIT_TIME, self(), nkdomain_move_completed),
            noreply(State#state{moved_to=NewPid, session=Session#obj_session{obj=#{}}});
        {error, Error} ->
            ?LLOG(warning, "could not move process to ~p: ~p", [Node, Error], State),
            noreply(State)
    end;

do_nkdist({vnode_pid, _Pid}, State) ->
    noreply(State);

do_nkdist(Msg, State) ->
    ?LLOG(notice, "unexpected NkDIST event: ~p", [Msg], State),
    noreply(State).


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
send_childs(Msg, #state{session=#obj_session{childs=Childs}}) ->
    lists:foreach(
        fun({_Type, ChildNames}) ->
            lists:foreach(
                fun({_Name, ObjId}) -> nkdomain_obj_lib:cast(ObjId, Msg) end,
                maps:to_list(ChildNames))
        end,
        maps:to_list(Childs)).


%% @private
do_add_timelog(Msg, State) when is_atom(Msg); is_binary(Msg) ->
    do_add_timelog(#{msg=>Msg}, State);

do_add_timelog(#{msg:=_}=Data, #state{session=Session, timelog=Log}=State) ->
    #obj_session{started=Started} = Session,
    Time = nkdomain_util:timestamp() - Started,
    State#state{timelog=[Data#{time=>Time}|Log]}.


%% @private
add_link(usage, Link, #state{session=Session}=State) ->
    #obj_session{link_usages=Links} = Session,
    Session2 = Session#obj_session{link_usages=Links#{Link => ok}},
    State#state{session=Session2};

add_link(event, Link, #state{session=Session}=State) ->
    #obj_session{link_events=Links} = Session,
    Session2 = Session#obj_session{link_events=nklib_util:store_value(Link, Links)},
    State#state{session=Session2}.


%% @private
remove_link(usage, Link, #state{session=Session}=State) ->
    #obj_session{link_usages=Links} = Session,
    Session2 = Session#obj_session{link_usages=maps:remove(Link, Links)},
    State#state{session=Session2};

remove_link(event, Link, #state{session=Session}=State) ->
    #obj_session{link_events=Links} = Session,
    Session2 = Session#obj_session{link_events=Links--[Link]},
    State#state{session=Session2}.


%% @private
do_call(Id, Msg) ->
    nkdomain_obj_lib:call(Id, Msg).

%% @private
do_call(Id, Msg, Time) ->
    nkdomain_obj_lib:call(Id, Msg, Time).

%% @private
do_cast(Id, Msg) ->
    nkdomain_obj_lib:cast(Id, Msg).


%% @private
get_timer(#state{timer=Timer}) when is_reference(Timer) ->
    erlang:read_timer(Timer);
get_timer(_) ->
    permanent.
