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

-module(nkdomain_proc).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([register/1, find/1, find_id/1, find_path/1, monitor/0]).
-export([get_all/0]).
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export_type([ets/0]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Proc "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec register(#obj_id_ext{}) ->
    ok | {already_registered, pid()}.

register(#obj_id_ext{pid=Pid}=ObjIdExt) when is_pid(Pid) ->
    do_call({register, ObjIdExt}).


%% @doc
-spec find(nkdomain:obj_id()|nkdomain:path()) ->
    #obj_id_ext{} | not_found.

find(Id) ->
    case nkdomain_util:is_path(Id) of
        {true, Path} ->
            find_path(Path);
        {false, ObjId} ->
            find_id(ObjId)
    end.


%% @doc
-spec find_id(nkdomain:obj_id()) ->
    #obj_id_ext{} | not_found.

find_id(ObjId) ->
    do_call({find_id, ObjId}).


%% @doc
-spec find_path(nkdomain:path()) ->
    #obj_id_ext{} | not_found.

find_path(ObjId) ->
    do_call({find_path, ObjId}).


%% @doc
monitor() ->
    {ok, Pid} = do_call(self),
    monitor(process, Pid).


%% @private
get_all() ->
    [
        {ObjId, Path, Pid} ||
        {{id, _}, #obj_id_ext{obj_id=ObjId, path=Path, pid=Pid}, _} <- ets:tab2list(?MODULE)
    ].



% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-type ets() ::
    {{id, nkdomain:obj_id()}, #obj_id_ext{}, Mon::reference()} |
    {{path, nkdomain:path()}, #obj_id_ext{}} |
    {{pid, pid()}, nkdomain:obj_id()}.


-record(state, {
    master_pid = undefined :: pid()
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init([]) ->
    Master = find_master(100),
    ets:new(?MODULE, [named_table, protected]),
    {ok, #state{master_pid=Master}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call({register, #obj_id_ext{obj_id=ObjId, path=Path, pid=Pid}=ObjIdExt}, _From, State) ->
    Reply = case do_find_id(ObjId) of
        not_found ->
            case do_find_path(Path) of
                not_found ->
                    do_insert(ObjIdExt);
                #obj_id_ext{pid=Pid} ->
                    do_remove(ObjId),
                    do_insert(ObjIdExt);
                #obj_id_ext{pid=OldPid} ->
                    {error, {already_registered, OldPid}}
            end;
        #obj_id_ext{pid=Pid} ->
            do_remove(ObjId),
            do_insert(ObjIdExt);
        #obj_id_ext{pid=OldPid} ->
            {error, {already_registered, OldPid}}
    end,
    {reply, Reply, State};

handle_call({find_id, ObjId}, _From, State) ->
    {reply, do_find_id(ObjId), State};

handle_call({find_path, Path}, _From, State) ->
    {reply, do_find_path(Path), State};

handle_call(self, _From, State) ->
    {reply, {ok, self()}, State};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{master_pid=Pid}=State) ->
    self() ! find_master,
    {noreply, State#state{master_pid=undefined}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case ets:lookup(?MODULE, {pid, Pid}) of
        [{_, ObjId}] ->
            do_remove(ObjId);
        [] ->
            lager:warning("Module ~p received unexpected down: ~p (~p)", [?MODULE, Pid, State])
    end,
    {noreply, State};

handle_info({global_name_conflict, ?MODULE}, State) ->
    ?LLOG(warning, "other process is master, stopping", []),
    {stop, global_name_conflict, State};

handle_info(Info, State) ->
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(_Reason, _State) ->
    ok.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
find_master(0) ->
    error(no_master_process);

find_master(Iters) ->
    case global:whereis_name(?MODULE) of
        Pid when is_pid(Pid) ->
            monitor(process, Pid),
            ?LLOG(notice, "new master is ~p", [Pid]),
            Pid;
        undefined ->
            case global:register_name(?MODULE, self(), fun global:random_notify_name/3) of
                yes ->
                    ?LLOG(notice, "WE are the new master (~p)", [self()]),
                    self();
                no ->
                    timer:sleep(100),
                    find_master(Iters-1)
            end
    end.


%% @private
do_find_id(ObjId) ->
    case ets:lookup(?MODULE, {id, ObjId}) of
        [{_, ObjIdExt, _Mon}] ->
            ObjIdExt;
        [] ->
            not_found
    end.


%% @private
do_find_path(Path) ->
    case ets:lookup(?MODULE, {path, Path}) of
        [{_, ObjIdExt}] ->
            ObjIdExt;
        [] ->
            not_found
    end.


%% @private
do_insert(#obj_id_ext{obj_id=ObjId, path=Path, pid=Pid}=ObjIdExt) ->
    Mon = monitor(process, Pid),
    ets:insert(?MODULE, [
        {{id, ObjId}, ObjIdExt, Mon},
        {{path, Path}, ObjIdExt},
        {{pid, Pid}, ObjId}
    ]),
    ok.


%% @private
do_remove(ObjId) ->
    case ets:lookup(?MODULE, {id, ObjId}) of
        [{_, #obj_id_ext{path=Path, pid=Pid}, Mon}] ->
            nklib_util:demonitor(Mon),
            ets:delete(?MODULE, {id, ObjId}),
            ets:delete(?MODULE, {path, Path}),
            ets:delete(?MODULE, {pid, Pid}),
            ok;
        [] ->
            ok
    end.


%% @doc
-spec do_call(term()) ->
    term() | {error, timeout|process_not_found|object_not_found|term()}.

do_call(Op) ->
    do_call(Op, 5).


%% @doc
-spec do_call(term(), timeout()) ->
    term() | {error, timeout|process_not_found|object_not_found|term()}.

do_call(Op, Timeout) ->
    do_call(Op, Timeout, 60).


%% @private
do_call(_Op, _Timeout, 0) ->
    error(nkdomain_proc_master_not_found);

do_call(Op, Timeout, Tries) ->
    case global:whereis_name(?MODULE) of
        Pid when is_pid(Pid) ->
            case nkservice_util:call(Pid, Op, Timeout) of
                {error, Error} when Error==timeout; Error==process_not_found ->
                    do_call_retry(Op, Timeout, Tries);
                Other ->
                    Other
            end;
        unefined ->
            do_call_retry(Op, Timeout, Tries)
    end.


%% @private
do_call_retry(Op, Timeout, Tries) ->
    ?LLOG(notice, "SyncOP failed, retrying...", []),
    timer:sleep(1000),
    do_call(Op, Timeout, Tries-1).



%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).