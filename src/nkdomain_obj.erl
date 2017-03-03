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

-export([create/3]).
-export([do_call/3, do_call/4, do_cast/3, do_info/3]).
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


%% ===================================================================
%% Types
%% ===================================================================


-type create_opts() ::
    #{
        obj_id => nkdomain:obj_id(),
        register => nklib:link(),
        user_id => nkdomain:obj_id(),
        user_session => nkservice:user_session(),
        events => [nkservice_events:type()]
    }.

-type session() ::
    create_opts() |
    #{
        type => nkdomain:type(),
        obj => nkdomain:obj()
    }.


-type info() :: atom().

-type event() ::
    created                                             |
    {stopped, nkservice:error()}                        |
    {info, info(), map()}                               |
    destroyed.






%% ===================================================================
%% Callbacks definitions
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================

%%%% @doc Finds an objects's pid or loads it from storage
%%-spec load(nkservice:id(), nkdomain:type(), nkdomain:obj_id(), binary()) ->
%%    {ok, pid()} | {error, obj_not_found|term()}.
%%
%%load(SrvId, Type, ObjId, _Token) ->
%%    ObjId2 = nklib_util:to_binary(ObjId),
%%    case nkdist:get(?MODULE, {Type, ObjId2}) of
%%        {ok, proc, [{Meta, Pid}]} ->
%%            {ok, Meta, Pid};
%%        {error, obj_not_found} ->
%%            domain_load()
%%
%%
%%            case SrvId:domain_load(Type, ObjId) of
%%                {ok, Meta, Data} ->
%%                    start_obj(Type, ObjId2, Meta, Data);
%%                not_found ->
%%                    {error, object_not_found};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%start_obj(_Type, _ObjId, _Meta, _Data) ->
%%    ok.


%% @doc Creates a new object
-spec create(nkservice:id(), nkdomain:obj(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()}.

create(Srv, #{type:=Type}=Obj, Meta) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            {ObjId, Meta2} = nkmedia_util:add_id(obj_id, Meta#{srv_id=>SrvId}, Type),
            {ok, ObjPid} = gen_server:start(?MODULE, {create, Obj, Meta2}, []),
            {ok, ObjId, ObjPid};
        not_found ->
            {error, service_not_found}
    end.




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
    started :: nklib_util:l_timestamp(),
    timer :: reference(),
    timelog = [] :: [map()]
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init({create, #{type:=Type}=Obj, #{obj_id:=Id, srv_id:=SrvId}=Meta}) ->
    true = nklib_proc:reg({?MODULE, Type, Id}),
    nklib_proc:put(?MODULE, {Type, Id}),
    Obj2 = Obj#{
        created_time => nklib_util:m_timestamp()
    },
    Session = Meta#{
        type => Type,
        obj => Obj2
    },
    State1 = #state{
        obj_id = Id,
        type = Type,
        srv_id = SrvId,
        links = nklib_links:new(),
        session = Session,
        started = nklib_util:l_timestamp()
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
    {ok, State3} = handle(object_init, [], State2),
    {ok, event(created, State3)}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(get_obj, _From, #state{session=#{obj:=Obj}}=State) ->
    reply({ok, Obj}, State);

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
    {noreply, State2};

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

handle_info({timeout, _, session_timeout}, State) ->
    ?LLOG(info, "operation timeout", [], State),
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
    State2 = add_to_obj(destroyed_time, nklib_util:timestamp(), State),
    % Give time for possible registrations to success and capture stop event
    timer:sleep(100),
    State3 = event({stopped, Reason}, State2),
    {ok, State4} = handle(object_stop, [Reason], State3),
    {_Code, Txt} = nkservice_util:error_code(SrvId, Reason),
    State5 = do_add_timelog(#{msg=>stopped, reason=>Txt}, State4),
    % Delay the destroyed event
    erlang:send_after(?SRV_DELAYED_DESTROY, self(), destroy),
    {noreply, State5#state{stop_reason=Reason}};

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
find(_Type, Pid) when is_pid(Pid) ->
    {ok, Pid};

find(Type, ObjId) ->
    case nklib_proc:values({?MODULE, Type, ObjId}) of
        [{undefined, Pid}] -> {ok, Pid};
        [] -> not_found
    end.


%% @private
do_call(Type, ObjId, Msg) ->
    do_call(Type, ObjId, Msg, ?DEF_SYNC_CALL).


%% @private
do_call(Type, ObjId, Msg, Timeout) ->
    case find(Type, ObjId) of
        {ok, Pid} -> nkservice_util:call(Pid, Msg, Timeout);
        not_found -> {error, obj_not_found}
    end.


%% @private
do_cast(Type, ObjId, Msg) ->
    case find(Type, ObjId) of
        {ok, Pid} -> gen_server:cast(Pid, Msg);
        not_found -> {error, obj_not_found}
    end.


%% @private
do_info(Type, ObjId, Msg) ->
    case find(Type, ObjId) of
        {ok, Pid} -> Pid ! Msg;
        not_found -> {error, obj_not_found}
    end.


%% @private
add_to_obj(Key, Val, #state{session=#{obj:=Obj}=Sess}=State) ->
    Obj2 = maps:put(Key, Val, Obj),
    State#state{session=Sess#{obj:=Obj2}}.


%%%% @private
%%remove_from_obj(Key, #state{session=#{obj:=Obj}=Sess}=State) ->
%%    Obj2 = maps:remove(Key, Obj),
%%    State#state{session=Sess#{obj:=Obj2}}.


%% @private
do_add_timelog(Msg, State) when is_atom(Msg); is_binary(Msg) ->
    do_add_timelog(#{msg=>Msg}, State);

do_add_timelog(#{msg:=_}=Data, #state{started=Started, timelog=Log}=State) ->
    Time = (nklib_util:l_timestamp() - Started) div 1000,
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

