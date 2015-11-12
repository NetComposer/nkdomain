%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Gonzalez Florido.  All Rights Reserved.
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
-behaviour(nkdist_proc).
-behaviour(gen_server).

-export([get_pid/2, get_obj/2, get_meta/2, load/4, export/2, remove_obj/2]).
-export([do_call/4, do_cast/3]).
-export([start/2, start_and_join/2, join/2, init/1, init_and_join/1]).
-export([terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export_type([base_obj/0]).


-type call_opts() :: #{timeout=>timeout()}.

%% All objects have at least these values
-type base_obj() ::
    #{
        desc => binary(),
        roles => nkrole:rolemap(),
        disabled => boolean
    }.

%% Metadata stored on disk
-type meta_obj() ::
    #{
        updated => nklib_util:l_timestamp()
    }.


-define(LG(Level, Class, ObjId, Text, Args),
    lager:Level("NkDOMAIN ~p '~s' "++Text, [Class, nklib_util:to_binary(ObjId) | Args])).


%% ===================================================================
%% Callbacks definitions
%% ===================================================================
%%
%% Optional callbacks
%%
%% -callback export(nkdomain:obj_id(), nkdomain_obj()) ->
%%      map().
%%
%% -callback get_backend(nkbase:class_meta) ->
%%      nkbase:class_meta().
%%
%% -callback removed(nkdomain:obj(), mod_state()) ->
%%    ok.
%%
%% -callback handle_call(term(), {pid(), term()}, nkdomain:obj(), mod_state()) ->
%%     {reply, term(), mod_state()} |
%%     {reply, term(), mod_state(), timeout() | hibernate} |
%%     {noreply, mod_state()} |
%%     {noreply, mod_state(), timeout() | hibernate} |
%%     {stop, term(), term(), mod_state()} |
%%     {stop, term(), mod_state()} |
%%     {removed, term() |
%%     removed.
%%
%% -callback handle_cast(term(), nkdomain:ob(), mod_state()) ->
%%     {noreply, mod_state()} |
%%     {noreply, mod_state(), timeout() | hibernate} |
%%     {stop, term(), mod_state()} |
%%     removed.
%%
%% -callback handle_info(term(), nkdomain:obj(), mod_state()) ->
%%     {noreply, mod_state()} |
%%     {noreply, mod_state(), timeout() | hibernate} |
%%     {stop, term(), mod_state()} |
%%     removed.
%%
%% -callback code_change(term()|{down, term()}, mod_state(), term()) ->
%%     {ok, NewState :: term()} | {error, Reason :: term()}.
%%
%% -callback terminate(term(), nkdomain:obj(), mod_state()) ->
%%     any().

-type mod_state() :: term().

-type init_opts() ::
    #{
        timeout => timeout()
    }.


%% Called  when the object is intializaed
-callback init(nkdomain:obj_id(), nkdomain:obj()) ->
    {ok, init_opts(), nkdomain:obj(), mod_state()}.


%% Called when a new specification must be loaded into the object
-callback load(map(), nkdomain_load:load_opts(), nkdomain:obj(), mod_state()) ->
    {ok, nkdomain:obj(), mod_state()} | removed | {error, term()}.





%% ===================================================================
%% Public
%% ===================================================================


%% @private
-spec get_pid(nkdomain:class(), nkdomain:obj_id()) ->
    {ok, pid()} | {error, not_found|term()}.

get_pid(Class, ObjId) ->
    ObjId1 = nklib_util:to_binary(ObjId),
    case nkdist:find_proc(?MODULE, {Class, ObjId1}) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            case nkbase:get(nkdomain, Class, ObjId1) of
                {ok, _, {nkdata, Meta, Data}} ->
                    start_obj(Class, ObjId1, Meta, Data);
                {deleted, _} ->
                    {error, not_found};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets a full object
-spec get_obj(nkdomain:class(), nkdomain:obj_id()) ->
    {ok, nkdomain:obj()} | {error, term()}.
 
get_obj(Class, ObjId) ->
    do_call(Class, ObjId, get_obj, #{}).


%% @doc Get object's metadata
-spec get_meta(nkdomain:class(), nkdomain:obj_id()) ->
    {ok, meta_obj()} | {error, term()}.

get_meta(Class, ObjId) ->
    do_call(Class, ObjId, get_meta, #{}).


%% @doc Loads an object definition
-spec load(nkdomain:class(), nkdomain:obj_id(), map(), nkdomain_load:load_opts()) ->
    {loaded, map()} | not_modified | removed | {error, term()}.

load(Class, ObjId, Data, Opts) ->
    CallOpts = #{timeout=>180000},
    case get_pid(Class, ObjId) of
        {error, not_found} ->
            case start_obj(Class, ObjId, #{}, #{}) of
                {ok, Pid} ->
                    nklib_util:call(Pid, {load, Data, Opts}, CallOpts);
                {error, Error} ->
                    {error, Error}
            end;
        {ok, Pid} ->
            nklib_util:call(Pid, {load, Data, Opts}, CallOpts)
    end.


%% @doc
-spec export(module(), nkdomain:obj_id()) ->
    {ok, map()} | {error, term()}.

export(Class, ObjId) ->
    case get_obj(Class, ObjId) of
        {ok, Obj} ->
            Obj1 = maps:remove(owner, Obj),
            Module = nkdomain_util:get_module(Class),
            case erlang:function_exported(Module, export, 2) of
                true ->
                    ObjId1 = nklib_util:to_binary(ObjId),
                    {ok, Module:export(ObjId1, Obj1)};
                false ->
                    {ok, Obj1}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private Removes an object
%% Most objects can also be removed loading a remove => true field
-spec remove_obj(nkdomain:class(), nkdomain:obj_id()) ->
    ok | {error, term()}.

remove_obj(Class, ObjId) ->
    do_call(Class, ObjId, remove, #{}).


%% ===================================================================
%% nkdist_proc behaviour
%% ===================================================================


%% @doc Start a new process
-spec start(nkdist:proc_id(), Args::term()) ->
    {ok, pid()} | {error, term()}.

start({Class, ObjId}, {Module, Meta, Obj}) ->
    proc_lib:start_link(?MODULE, init, [{Class, ObjId, Module, Meta, Obj}]).


%% @doc Starts a new clone process
-spec start_and_join(nkdist:proc_id(), pid()) ->
    {ok, pid()} | {error, term()}.

start_and_join({Class, ObjId}, Pid) ->
    proc_lib:start_link(?MODULE, init_and_join, [{Class, ObjId, Pid}]).


%% @doc Joins two existing processes
-spec join(Current::pid(), Old::pid()) ->
    ok | {error, term()}.

join(Current, Old) ->
    gen_server:cast(Current, {join, Old}).


% %% @doc Custom hash function
% %% Uses only first 6 chars of the object for the key
% -spec get_hash(nkdist:proc_id()) ->
%     binary().

% get_hash({_Class, ObjId}) ->
%     Base = nklib_util:hash(ObjId),
%     nklib_util:sha(Base). 


%% ===================================================================
%% gen_server behaviour
%% ===================================================================

-record(state, {
    class :: nkdomain:class(),
    id :: nkdomain:obj_id(),
    module :: module(),
    meta :: meta_obj(),
    obj :: nkdomain:obj(),
    mod_data :: term(),
    timeout :: timeout()
}).

-define(GS1, #state.module).
-define(GS2, #state.mod_data).


%% @private
-spec init({nkdomain:class(), nkdomain:obj_id(), module(), 
            meta_obj(), nkdomain:obj()}) ->
    {ok, tuple()}.

init({Class, ObjId, Module, Meta, Obj}) ->
    ok = proc_lib:init_ack({ok, self()}),
    State1 = #state{
        class = Class, 
        id = ObjId,
        module = Module, 
        meta = maps:merge(#{updated=>0}, Meta),
        obj = Obj
    },
    Obj1 = maps:merge(#{roles=>#{}}, Obj),
    {ok, Opts, Obj2, UserState} = Module:init(ObjId, Obj1),
    Timeout = maps:get(timeout, Opts, infinity),
    State2 = State1#state{
        obj = Obj2,
        mod_data = UserState, 
        timeout = Timeout
    },
    ?LG(debug, Class, ObjId, "started (~p)", [self()]),
    gen_server:enter_loop(?MODULE, [], State2, Timeout).


%% @private
-spec init_and_join(pid()) ->
    {ok, tuple()}.

init_and_join({Class, ObjId, Pid}) ->
    ok = proc_lib:init_ack({ok, self()}),
    case catch gen_server:call(Pid, transfer) of
        {ok, #state{class=Class, id=ObjId}=State1} ->
            ?LG(notice, Class, ObjId, "started and joined (~p)", [self()]),
            #state{timeout=Timeout} = State1,
            gen_server:enter_loop(?MODULE, [], State1, Timeout);
        Error ->
            ?LG(warning, Class, ObjId, "could not transfer domain: ~p", [Error])
    end.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(get_obj, _From, #state{obj=Obj}=State) ->
    reply({ok, Obj}, State);

handle_call(get_rolemap, _From, #state{obj=Obj}=State) ->
    Roles = maps:get(roles, Obj, #{}),
    Owner = maps:get(owner, Obj, <<"root">>),
    reply({ok, Roles#{owner=>Owner}}, State);

handle_call(get_meta, _From, #state{meta=Meta}=State) ->
    reply({ok, Meta}, State);

handle_call({load, #{remove:=true}, _Opts}, _From, State) ->
    remove(State),
    {stop, normal, removed, State};

handle_call({load, Data, Opts}, _From, State) ->
    case load(Data, Opts, State) of
        {removed, State1} ->
            remove(State),
            {stop, normal, removed, State1};
        {Reply, State1} ->
            reply(Reply, State1)
    end;

handle_call(transfer, _From, State) ->
    {stop, normal, {ok, State}, State};

handle_call(remove, _From, State) ->
    remove(State),
    {stop, normal, ok, State};

handle_call(Msg, From, #state{obj=Obj}=State) -> 
    case nklib_gen_server:handle_any(handle_call, [Msg, From, Obj], State, ?GS1, ?GS2) of
        {reply, Reply, State1} -> 
            reply(Reply, State1);
        {reply, Reply, State1, Timeout} -> 
            reply(Reply, State1#state{timeout=Timeout});
        {noreply, State1} -> 
            noreply(State1);
        {noreply, State1, Timeout} -> 
            noreply(State1#state{timeout=Timeout});
        ok ->
            lager:warning("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
            noreply(State);
        {removed, Reply} ->
            remove(State),
            {stop, normal, Reply, State};
        removed ->
            remove(State),
            {stop, normal, State};
        Other ->
            Other
    end.
          

%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast({join, Pid}, #state{id=ObjId, class=Class}=State) ->
    case catch gen_server:call(Pid, trasfer, 60000) of
        {ok, #state{class=Class, id=ObjId}} ->
            noreply(State);
        Error ->
            ?LG(warning, Class, ObjId, "Could not join domain: ~p", [Error]),
            noreply(State)
    end;

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, #state{obj=Obj}=State) -> 
    case nklib_gen_server:handle_any(handle_cast, [Msg, Obj], State, ?GS1, ?GS2) of
        {noreply, State1} -> 
            noreply(State1);
        {noreply, State1, Timeout} -> 
            noreply(State1#state{timeout=Timeout});
        ok ->
            lager:warning("Module ~p received unexpected cast: ~p", [?MODULE, Msg]),
            noreply(State);
        removed ->
            remove(State),
            {stop, normal, State};
        Other -> 
            Other
    end.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info(Msg, #state{obj=Obj}=State) ->
    case nklib_gen_server:handle_any(handle_info, [Msg, Obj], State, ?GS1, ?GS2) of
        {noreply, State1} -> 
            noreply(State1);
        {noreply, State1, Timeout} -> 
            noreply(State1#state{timeout=Timeout});
        ok ->
            lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Msg]),
            noreply(State);
        removed ->
            remove(State),
            {stop, normal, State};
        Other -> 
            Other
    end.


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(OldVsn, State, Extra) ->
    nklib_gen_server:code_change(OldVsn, State, Extra, ?GS1, ?GS2).


%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(Reason, #state{class=Class, id=ObjId, obj=Obj}=State) ->
    ?LG(debug, Class, ObjId, "terminating", []),
    nklib_gen_server:handle_any(terminate, [Reason, Obj], State, ?GS1, ?GS2),
    nkdomain_role:stop({Class, ObjId}).

    

%% ===================================================================
%% Internal
%% ===================================================================


%% @private
start_obj(Class, ObjId, Meta, Data) ->
    Module = nkdomain_util:get_module(Class),
    ObjId1 = nklib_util:to_binary(ObjId),
    case nkdist:start_proc(?MODULE, {Class, ObjId1}, {Module, Meta, Data}) of
        {ok, Pid} -> 
            {ok, Pid};
        {error, 
            {already_started, Pid}} -> {ok, Pid};
        {error, Error} -> 
            {error, Error}
    end.


%% @private
-spec load(map(), nkdomain_load:load_opts(), #state{}) ->
    {{loaded, map()} | not_modified | removed | {error, term()}, #state{}}.

load(Data, Opts, #state{obj=OldObj}=State) ->
    Args = [Data, Opts, OldObj],
    case nklib_gen_server:handle_any(load, Args, State, ?GS1, ?GS2) of
        {ok, NewObj, State1} ->
            RoleMap = maps:get(roles, Data, #{}),
            load_roles(RoleMap, NewObj, Opts, State1);
        removed -> 
            remove(State),
            {removed, State};
        {error, Error, State1} -> 
            {{error, Error}, State1}
    end.


%% @private
load_roles(RoleMap1, NewObj, Opts, State) ->
    #state{class=Class, id=ObjId, meta=Meta, obj=OldObj} = State,
    OldRoleMap = maps:get(roles, OldObj, #{}),
    RoleMap2 = nkdomain_role:resolve_roles(RoleMap1),
    RoleMap3 = case Opts of
        #{replace:=true} -> 
            Empties = maps:from_list(
                [{Role, []} || Role <- maps:keys(OldRoleMap)]),
            Merged = maps:merge(Empties, RoleMap2),
            send_updated_roles(Class, ObjId, Merged),
            RoleMap2;
        _ -> 
            Merged = maps:merge(OldRoleMap, RoleMap2),
            send_updated_roles(Class, ObjId, Merged),
            Merged
    end,
    NewObj2 = NewObj#{roles=>RoleMap3},
    NewObj3 = case NewObj2 of
        #{owner:=_} -> 
            NewObj2;
        _ -> 
            NewObj2#{owner=>maps:get(owner, Opts, <<"root">>)}
    end,
    Updated = maps:get(updated, Meta),
    case NewObj3 of
        OldObj when Updated/=0 ->
            {not_modified, State};
        _ ->
            load_save(NewObj3, State)
    end.


%% @private
load_save(NewObj, State) ->
    #state{
        class = Class, 
        id = ObjId, 
        meta = Meta,
        obj = OldObj
    } = State,
    Meta1 = Meta#{updated=>nklib_util:l_timestamp()},
    DiskObj = {nkdata, Meta1, NewObj},
    case nkbase:put(nkdomain, Class, ObjId, DiskObj) of
        ok ->
            case maps:get(updated, Meta) of
                0 ->
                    ?LG(info, Class, ObjId, "loaded config", []);
                _ ->
                    % ?LG(info, Class, ObjId, "updated config\nFrom: ~p\nTo:   ~p", 
                    %     [OldObj, NewObj])
                    ?LG(info, Class, ObjId, "updated config: ~p", 
                        [get_diffs(OldObj, NewObj)])
            end,
            {{loaded, NewObj}, State#state{meta=Meta1, obj=NewObj}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
send_updated_roles(Class, ObjId, RoleMap) ->
    spawn_link(
        fun() ->
            lists:foreach(
                fun({Role, RoleSpecs}) ->
                    nkdomain_role:set_role(Role, {Class, ObjId}, RoleSpecs)
                end,
                maps:to_list(RoleMap))
        end).


%% @private
-spec remove(#state{}) ->
    ok.

remove(#state{class=Class, id=ObjId, obj=Obj}=State) ->
   nklib_gen_server:handle_any(removed, [Obj], State, ?GS1, ?GS2),
    case nkbase:del(nkdomain, Class, ObjId) of
        ok ->
            ?LG(info, Class, ObjId, "removed", []);
        {error, Error} ->
            ?LG(warning, Class, ObjId, "could not delete: ~p", [Error])
    end.


%% @private
reply(Reply, #state{timeout=Timeout}=State) ->
    {reply, Reply, State, Timeout}.


%% @private
noreply(#state{timeout=Timeout}=State) ->
    {noreply, State, Timeout}.



%% @private Safe call to an object with retries
%% Objects with timeouts can disappear betwen the pid get and the call, 
%% so the call if retries if it fails
-spec do_call(nkdomain:class(), nkdomain:obj_id(), term(), call_opts()) ->
    term().

do_call(Class, ObjId, Op, Opts) ->
    do_call(Class, ObjId, Op, Opts, 3).


%% @private
do_call(Class, ObjId, Op, Opts, Tries) ->
    case get_pid(Class, ObjId) of
        {ok, Pid} ->
            case nklib_util:call(Pid, Op, Opts) of
                {error, {exit, _}} when Tries > 1 ->
                    ObjId1 = nklib_util:to_binary(ObjId),
                    lager:notice("NkDOMAIN Obj ~s (~p) call exit (~p), retrying", 
                                 [ObjId1, Class, Op]),
                    timer:sleep(100),
                    do_call(Class, ObjId1, Op, Opts, Tries-1);
                Other ->
                    Other
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_cast(Class, ObjId, Op) ->
    case get_pid(Class, ObjId) of
        {ok, Pid} ->
            gen_server:cast(Pid, Op);
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_diffs(Map1, Map2) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Map1) of
                {ok, V} -> Acc;
                _ -> maps:put(K, V, Acc)
            end
        end,
        #{},
        Map2).






