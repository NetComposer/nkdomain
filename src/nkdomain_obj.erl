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
%%

%% @doc Core obj behaviour
%% One of this objects is started for each object, distributed in the cluster
%%

-module(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([find_id/2, load/3, create/3, save/1, get_all/0]).
-export([find/1, do_call/2, do_call/3, do_cast/2, do_info/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
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
            {type, State#state.type}
        ],
        "NkDOMAIN Obj (~s:~s) "++Txt,
        [State#state.obj_id, State#state.type | Args])).

-define(SRV_DELAYED_DESTROY, 5000).
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


-callback object_get_syntax() ->
    nklib_syntax:syntax().


-callback object_store(nkdomain:object()) ->
    map().


%% ===================================================================
%% Types
%% ===================================================================


-type id() ::
    {nkdomain:type(), nkdomain:obj_id()} | nkdomain:domain() | pid().

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

-type session() ::
    #{
        obj_id => nkdomain:obj_id(),
        type => nkdomain:type(),
        obj => nkdomain:obj(),
        srv_id => nkservice:id(),
        callback => module(),
        is_dirty => boolean(),
        enabled => boolean(),                       % 'Real' enabled or not (father)
        meta => create_opts() | load_opts()
    }.


-type info() :: atom().

-type event() ::
    created                                             |
    {stopped, nkservice:error()}                        |
    {info, info(), map()}                               |
    destroyed.








%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds the type and obj_id from path in disk
-spec find_id(nkservice:id(), nkdomain:domain()) ->
    {ok, nkdomain:type(), nkdomain:obj_id()} | {error, object_not_found|term()}.

find_id(Srv, Path) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            nkdomain_types:find_obj_path(SrvId, Path);
        not_found ->
            {error, service_not_found}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), id(), load_opts()) ->
    {ok, pid()} | {error, obj_not_found|term()}.

load(Srv, {Type, ObjId}=Id, Meta) ->
    case find(Id) of
        {ok, Pid} ->
            {ok, Pid};
        not_found ->
            case catch Type:object_get_desc() of
                Desc when is_map(Desc) ->
                    case nkservice_srv:get_srv_id(Srv) of
                        {ok, SrvId} ->
                            Meta2 = Meta#{
                                srv_id => SrvId,
                                is_dirty => false
                            },
                            case Srv:object_load(Id, Meta2) of
                                {ok, Obj} ->
                                    {ok, ObjPid} = gen_server:start(?MODULE, {Obj, Meta2}, []),
                                    {ok, ObjId, ObjPid};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        not_found ->
                            {error, service_not_found}
                    end;
                _ ->
                    {error, invalid_type}
            end
    end.


%% @doc Creates a new object
-spec create(nkservice:id(), nkdomain:obj(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()}.

create(Srv, #{type:=Type}=Obj, Meta) ->
    case catch Type:object_get_desc() of
        Desc when is_map(Desc) ->
            case nkservice_srv:get_srv_id(Srv) of
                {ok, SrvId} ->
                    {ObjId, Obj2} = nkmedia_util:add_id(obj_id, Obj, Type),
                    Meta2 = Meta#{
                        srv_id => SrvId,
                        is_dirty => true
                    },
                    Obj3 = Obj2#{
                        created_time => nklib_util:m_timestamp()
                    },
                    case SrvId:object_parse(Obj3, Meta2) of
                        {ok, Obj4} ->
                            {ok, ObjPid} = gen_server:start(?MODULE, {Obj4, Meta2}, []),
                            {ok, ObjId, ObjPid};
                        {error, Error} ->
                            {error, Error}
                    end;
                not_found ->
                    {error, service_not_found}
            end;
        _ ->
            {error, invalid_type}
    end.


%% @doc
save(Id) ->
    do_cast(Id, do_save).


%% @doc
-spec get_all() ->
    [{nkdomain:type(), nkdomain:obj_id()}].

get_all() ->
    nklib_proc:values(?MODULE).




% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    obj_id :: nkdomain:obj_id(),
    type :: nkdomain:type(),
    srv_id :: nkservice:id(),
    stop_reason = false :: false | nkservice:error(),
    links :: nklib_links:links(),
    session :: session(),
    started :: nklib_util:m_timestamp(),
    timer :: reference(),
    timelog = [] :: [map()]
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init({Obj, Meta}) ->
    #{obj_id:=ObjId, type:=Type} = Obj,
    true = nklib_proc:reg({?MODULE, {Type, ObjId}}),
    nklib_proc:put(?MODULE, {Type, ObjId}),
    #{srv_id:=SrvId, is_dirty:=IsDirty} = Meta,
    Session = Meta#{
        obj_id => ObjId,
        type => Type,
        obj => Obj,
        srv_id => SrvId,
        meta => maps:without([srv_id, callback, is_dirty], Meta),
        is_dirty => IsDirty,
        enabled => maps:get(enabled, Obj, true)
    },
    State1 = #state{
        obj_id = ObjId,
        type = Type,
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
    ?LLOG(info, "starting (~p)", [self()], State2),
    gen_server:cast(self(), do_start),
    gen_server:cast(self(), do_save),
    {ok, State3} = handle(object_init, [], State2),
    {ok, event(created, State3)}.


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

handle_call(Msg, From, State) ->
    handle(object_handle_call, [Msg, From], State).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(do_start, State) ->
    {ok, State2} = handle(object_start, [], State),
    noreply(State2);

handle_cast(do_save, State) ->
    noreply(do_save(State));

handle_cast({restart_timer, Time}, #state{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    NewTimer = erlang:start_timer(Time, self(), session_timeout),
    State#state{timer=NewTimer};

handle_cast({timelog, Data}, State) ->
    {noreply, do_add_timelog(Data, State)};

handle_cast({send_info, Info, Meta}, State) ->
    noreply(event({info, Info, Meta}, State));

handle_cast({register, Link}, State) ->
    ?DEBUG("registered link (~p)", [Link], State),
    noreply(links_add(Link, State));

handle_cast({unregister, Link}, State) ->
    ?DEBUG("proc unregistered (~p)", [Link], State),
    noreply(links_remove(Link, State));

handle_cast({stop, Error}, State) ->
    do_stop(Error, State);

handle_cast(Msg, State) ->
    handle(object_handle_cast, [Msg], State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info(do_save, State) ->
    noreply(do_save(State));

handle_info({timeout, Ref, session_timeout}, #state{timer=Ref}=State) ->
    ?LLOG(info, "session timeout", [], State),
    do_stop(session_timeout, State);

handle_info({'DOWN', Ref, process, _Pid, Reason}=Msg, State) ->
    #state{stop_reason=Stop} = State,
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
            {noreply, State2};
        not_found ->
            handle(object_handle_info, [Msg], State)
    end;

handle_info(destroy, State) ->
    {stop, normal, State};

handle_info({nkservice_updated, _SrvId}, State) ->
    {noreply, set_log(State)};

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
    case Stop of
        false ->
            {noreply, State2} = do_stop({terminate, Reason}, State);
        _ ->
            State2 = State
    end,
    State3 = event({record, lists:reverse(Log)}, State2),
    State4 = event(destroyed, State3),
    {ok, _State5} = handle(object_terminate, [Reason], State4),
    ok.




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
set_log(#state{srv_id=SrvId, type=Type}=State) ->
    Debug =
        case nkservice_util:get_debug_info(SrvId, ?MODULE) of
            {true, #{types:=Types}} -> lists:member(Type, Types);
            {true, _} -> true;
            _ -> false
        end,
    put(object_debug, Debug),
    State.


%% @private
do_save(#state{session=#{is_dirty:=false}}=State) ->
    {ok, State};

do_save(State) ->
    ?LLOG(info, "saving object", [], State),
    case handle(object_save, [], State) of
        {ok, State2} ->
            add_to_session(is_dirty, false, State2);
        {error, Error, State2} ->
            ?LLOG(warning, "could not save object: ~p", [Error], State2),
            erlang:send_after(?SAVE_RETRY, self(), do_save),
            State2
    end.


%% ===================================================================
%% Util
%% ===================================================================

%% @private
reply(Reply, State) ->
    {reply, Reply, State}.


%% @private
noreply(State) ->
    {noreply, State}.


%% @private
do_stop(Reason, #state{srv_id=SrvId, stop_reason=false}=State) ->
    ?DEBUG("stopped: ~p", [Reason], State),
    State2 = State#state{stop_reason=Reason},
    State3 = add_to_obj(destroyed_time, nklib_util:m_timestamp(), State2),
    % Give time for possible registrations to success and capture stop event
    timer:sleep(100),
    State4 = event({stopped, Reason}, State3),
    {ok, State5} = handle(object_stop, [Reason], State4),
    {_Code, Txt} = nkservice_util:error_code(SrvId, Reason),
    State6 = do_add_timelog(#{msg=>stopped, reason=>Txt}, State5),
    case Reason of
        session_stop ->
            {stop, normal, State6};
        _ ->
            % Delay the destroyed event
            erlang:send_after(?SRV_DELAYED_DESTROY, self(), destroy),
            {noreply, State6}
    end;

do_stop(session_stop, State) ->
    {stop, normal, State};

do_stop(_Reason, State) ->
    % destroy already sent
    {noreply, State}.


%% @private
event(Event, State) ->
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
handle(Fun, Args, #state{type=Type}=State) ->
    nklib_gen_server:handle_any(Fun, [Type|Args], State,
                                #state.srv_id, #state.session).


%% @private
find(Pid) when is_pid(Pid) ->
    {ok, Pid};

find({Type, ObjId}) ->
    case nklib_proc:values({?MODULE, Type, ObjId}) of
        [{undefined, Pid}] -> {ok, Pid};
        [] -> not_found
    end;

find(_) ->
    not_found.


%% @private
do_call(Id, Msg) ->
    do_call(Id, Msg, ?DEF_SYNC_CALL).


%% @private
do_call(Id, Msg, Timeout) ->
    case find(Id) of
        {ok, Pid} -> nkservice_util:call(Pid, Msg, Timeout);
        not_found -> {error, obj_not_found}
    end.


%% @private
do_cast(Id, Msg) ->
    case find(Id) of
        {ok, Pid} -> gen_server:cast(Pid, Msg);
        not_found -> {error, obj_not_found}
    end.


%% @private
do_info(Id, Msg) ->
    case find(Id) of
        {ok, Pid} -> Pid ! Msg;
        not_found -> {error, obj_not_found}
    end.


%% @private
add_to_obj(Key, Val, #state{session=#{obj:=Obj}}=State) ->
    add_to_session(obj, ?ADD_TO_OBJ(Key, Val, Obj), State).


%% @private
add_to_session(Key, Val, #state{session=Session}=State) ->
    State#state{session=?ADD_TO_SESSION(Key, Val, Session)}.


%%%% @private
%%remove_from_obj(Key, #state{session=#{obj:=Obj}=Sess}=State) ->
%%    Obj2 = maps:remove(Key, Obj),
%%    State#state{session=Sess#{obj:=Obj2}}.


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

