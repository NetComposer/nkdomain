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

-module(nkdomain_store).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([save/3, delete/2, archive/3, find/2, find_archive/2, clean/1]).
-export([delete_all_childs/2, delete_all_childs_type/3]).
-export([get_data/0, start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).


-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store "++Txt, Args)).


-define(RETRY_TIME, 5000).
-define(MAX_SAVE_TIME, 60000).
-define(MAX_DELETE_TIME, 60000).
-define(MAX_ARCHIVE_TIME, 60000).

-include("nkdomain.hrl").

%% ===================================================================
%% Public
%% ===================================================================


%% @doc Tries to save an object, or it is stored to be saved later
-spec save(nkservice:id(), nkdomain:obj_id(), map()) ->
    {ok, term()} | {error, term()}.

save(Srv, ObjId, Obj) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            wait_remove_op(SrvId, ObjId),
            case SrvId:object_store_save_raw(SrvId, ObjId, Obj) of
                {ok, Vsn} ->
                    {ok, Vsn};
                {error, Error} ->
                    ?LLOG(info, "could not save object ~s (~s), delaying", [ObjId, SrvId:name()]),
                    gen_server:cast(?MODULE, {save, SrvId, ObjId, Obj}),
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.



%% @doc Tries to remove an object, or it is stored to be removed later on
-spec delete(nkservice:id(), nkdomain:obj_id()) ->
    ok | {error, term()}.

delete(Srv, ObjId) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            wait_remove_op(SrvId, ObjId),
            case SrvId:object_store_delete_raw(SrvId, ObjId) of
                ok ->
                    % If the object is deleted (it didn't has childs, etc.) notify the owner (if still present)
                    nkdomain_obj_lib:do_cast(ObjId, nkdomain_obj_deleted),
                    ok;
                {error, object_not_found} ->
                    {error, object_not_found};
                {error, object_has_childs} ->
                    {error, object_has_childs};
                {error, Error} ->
                    ?LLOG(info, "could not delete object ~s (~s), delaying", [ObjId, SrvId:name()]),
                    gen_server:cast(?MODULE, {delete, SrvId, ObjId}),
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.



%% @doc Tries to remove an object, or it is stored to be removed later on
-spec archive(nkservice:id(), nkdomain:obj_id(), nkdomain:obj()) ->
    ok | {error, term()}.

archive(Srv, ObjId, Obj) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            wait_remove_op(SrvId, ObjId),
            case SrvId:object_store_archive_save_raw(SrvId, ObjId, Obj) of
                ok ->
                    ok;
                {error, Error} ->
                    ?LLOG(info, "could not delete object ~s (~s), delaying", [ObjId, SrvId:name()]),
                    gen_server:cast(?MODULE, {archive, SrvId, ObjId, Obj}),
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.



%% @doc
delete_all_childs(Srv, Id) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id = SrvId, path = Path} ->
            SrvId:object_store_delete_all_childs(SrvId, Path, #{});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
delete_all_childs_type(Srv, Id, Type) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            Opts = #{filters => #{type=>Type}},
            SrvId:object_store_delete_all_childs(SrvId, Path, Opts);
        not_found ->
            {error, service_not_found}
    end.



%% @doc
find(Srv, Spec) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            SrvId:object_store_find(SrvId, Spec);
        not_found ->
            {error, service_not_found}
    end.


%% @private
find_archive(Srv, Spec) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            SrvId:object_store_archive_find(SrvId, Spec);
        not_found ->
            {error, service_not_found}
    end.


%% @private Performs a periodic cleanup
-spec clean(nkservice:id()) ->
    {ok, map()} | {error, term()}.

clean(Srv) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            SrvId:object_store_clean(SrvId);
        not_found ->
            {error, service_not_found}
    end.


%% @private
get_data() ->
    gen_server:call(?MODULE, get_data).



% ===================================================================
%% gen_server behaviour
%% ===================================================================

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-type operation() :: save | remove.

-record(queue_op, {
    operation :: operation(),
    obj_id :: nkdomain:obj_id(),
    obj :: nkdomain:obj(),
    expire :: nklib_util:m_timestamp()
}).

-record(state, {
    operations = #{} :: #{{nkservice:id(), nkdomain:obj_id()} => operation()},
    queue = #{} :: #{nkservice:id() => [#queue_op{}]}
}).


%% @private
-spec init(term()) ->
    {ok, #state{}} | {error, term()}.

init([]) ->
    self() ! retry_operations,
    {ok, #state{}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(get_data, _From, #state{operations=Ops, queue=Queue}=State) ->
    {reply, {Ops, Queue}, State};

handle_call({remove_op, SrvId, ObjId}, _From, State) ->
    reply(ok, remove_op(SrvId, ObjId, State));

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    noreply(State).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast({delete, SrvId, ObjId}, State) ->
    noreply(add_op(SrvId, delete, ObjId, undefined, State));

handle_cast({save, SrvId, ObjId, Obj}, State) ->
    noreply(add_op(SrvId, save, ObjId, Obj, State));

handle_cast({archive, SrvId, ObjId, Obj}, State) ->
    noreply(add_op(SrvId, archive, ObjId, Obj, State));

handle_cast({remove_op, SrvId, ObjId}, State) ->
    noreply(remove_op(SrvId, ObjId, State));

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    noreply(State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info(retry_operations, State) ->
    State2 = do_save(State),
    erlang:send_after(?RETRY_TIME, self(), retry_operations),
    noreply(State2);

handle_info(Info, State) ->
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    noreply(State).


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
add_op(SrvId, Op, ObjId, Obj, #state{operations=Operations, queue=Queue}=State) ->
    List1 = maps:get(SrvId, Queue, []),
    Time = case Op of
        save -> ?MAX_SAVE_TIME;
        delete -> ?MAX_DELETE_TIME;
        archive -> ?MAX_ARCHIVE_TIME
    end,
    QueueOp = #queue_op{
        operation = Op,
        obj_id = ObjId,
        obj = Obj,
        expire = nklib_util:m_timestamp() + Time
    },
    List2 = case maps:is_key({SrvId, ObjId}, Operations) of
        false ->
            [QueueOp|List1];
        true ->
            [QueueOp|lists:keydelete(ObjId, #queue_op.obj_id, List1)]
    end,
    Operations2 = Operations#{{SrvId, ObjId} => Op},
    State#state{operations=Operations2, queue=Queue#{SrvId=>List2}}.


%% @private
remove_op(SrvId, ObjId, #state{operations=Operations, queue=Queue}=State) ->
    case maps:is_key({SrvId, ObjId}, Operations) of
        false ->
            State;
        true ->
            Operations2 = maps:remove({SrvId, ObjId}, Operations),
            List1 = maps:get(SrvId, Queue),
            Queue2 = case lists:keydelete(ObjId, #queue_op.obj_id, List1) of
                [] ->
                    maps:remove(SrvId, Queue);
                List2 ->
                    Queue#{SrvId:=List2}
            end,
            State#state{operations=Operations2, queue=Queue2}
    end.


%% @private
do_save(#state{queue=Queue}=State) ->
    do_save(maps:keys(Queue), State).


%% @private
do_save([], State) ->
    State;

do_save([SrvId|Rest], #state{queue=Queue}=State) ->
    case do_save_srv_id(SrvId, maps:get(SrvId, Queue), nklib_util:m_timestamp(), State) of
        {ok, State2} ->
            Queue2 = maps:remove(SrvId, Queue),
            do_save(Rest, State2#state{queue=Queue2});
        {error, List, State2} ->
            Queue2 = Queue#{SrvId:=List},
            do_save(Rest, State2#state{queue=Queue2})
    end.


%% @private
do_save_srv_id(_SrvId, [], _Now, State) ->
    {ok, State};

do_save_srv_id(SrvId, [QueueOp|Rest]=List, Now, State) ->
    #queue_op{operation=Op, obj_id=ObjId, obj=Obj, expire=Expire} = QueueOp,
    #state{operations=Operations} = State,
    case Now > Expire of
        true ->
            ?LLOG(notice, "giving up ~p for '~s' (~s)", [Op, SrvId:name(), ObjId]),
            Operations2 = maps:remove({SrvId, ObjId}, Operations),
            do_save_srv_id(SrvId, Rest, Now, State#state{operations=Operations2});
        false ->
            Res = case Op of
                save ->
                    case SrvId:object_store_save_raw(SrvId, ObjId, Obj) of
                        {ok, _Vsn} ->
                            ok;
                        {error, ResError} ->
                            {error, ResError}
                    end;
                delete ->
                    case SrvId:object_store_delete_raw(SrvId, ObjId) of
                        ok ->
                            nkdomain_obj_lib:do_cast(ObjId, nkdomain_obj_deleted),
                            ok;
                        {error, object_not_found} ->
                            ok;
                        {error, object_has_childs} ->
                            ok;
                        {error, ResError} ->
                            {error, ResError}
                    end;
                archive ->
                    case SrvId:object_store_archive_save_raw(SrvId, ObjId) of
                        ok ->
                            ok;
                        {error, ResError} ->
                            {error, ResError}
                    end
            end,
            case Res of
                ok ->
                    Operations2 = maps:remove({SrvId, ObjId}, Operations),
                    do_save_srv_id(SrvId, Rest, Now, State#state{operations=Operations2});
                {error, Error} ->
                    ?LLOG(warning, "could not ~p object for '~s' (~s): ~p", [Op, SrvId:name(), ObjId, Error]),
                    {error, List, State}
            end
    end.


%% @private
reply(Reply, State) ->
    {reply, Reply, State}.


%% @private
noreply(State) ->
    {noreply, State}.



%% @private
wait_remove_op(SrvId, ObjId) ->
    _ = gen_server:call(?MODULE, {remove_op, SrvId, ObjId}, 60000).





