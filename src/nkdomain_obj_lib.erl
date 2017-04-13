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


%% @doc Basic Obj utilities


-module(nkdomain_obj_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([find/2, load/3, create/3]).
-export([make_obj/4, make_and_create/4]).
-export([unload/4, sync_op/5, async_op/5]).
-export([reg/3, find_loaded/1, call/2, call/3, cast/2, info/2]).

-include("nkdomain.hrl").

-define(DEF_SYNC_CALL, 5000).


%% ===================================================================
%% Types
%% ===================================================================


-type make_opts() ::
    #{
        obj_id => binary(),
        name => binary(),
        parent => binary(),
        referred_id => nkdomain:obj_id(),
        active => boolean(),
        description => binary(),
        aliases => [binary()],
        type_obj => map(),
        wait_for_save => integer()
    }.


%% ===================================================================
%% Public
%% ===================================================================







%% @doc Adds type, obj_id, parent_id, path, created_time
-spec make_obj(nkservice:id(), nkdomain:obj_id(), nkdomain:type(), make_opts()) ->
    {ok, nkdomain:obj()} | {error, term()}.

make_obj(Srv, Parent, Type, Opts) ->
    case find(Srv, Parent) of
        #obj_id_ext{obj_id=ParentId, path=ParentPath} ->
            Type2 = to_bin(Type),
            ObjId = case Opts of
                #{obj_id:=ObjId0} ->
                    to_bin(ObjId0);
                _ ->
                    <<Type2/binary, $-, (nklib_util:luid())/binary>>
            end,
            Name1 = case Opts of
                #{name:=Name0} ->
                    case to_bin(Name0) of
                        <<>> -> ObjId;
                        Name0bin -> nkdomain_util:name(Name0bin)
                    end;
                _ ->
                    ObjId
            end,
            Name2 = case Type2 of
                ?DOMAIN_DOMAIN ->
                    Name1;
                _ ->
                    <<Type2/binary, "s/", Name1/binary>>
            end,
            BasePath = case ParentPath of
                <<"/">> -> <<>>;
                _ -> ParentPath
            end,
            Obj = [
                {obj_id, ObjId},
                {type, Type2},
                {parent_id, ParentId},
                {path, <<BasePath/binary, $/, Name2/binary>>},
                {created_time, nklib_util:m_timestamp()},
                case Opts of
                    #{referred_id:=ReferredId} ->
                        {referred_id, ReferredId};
                    _ ->
                        []
                end,
                case Opts of
                    #{description:=Description} ->
                        {description, Description};
                    _ ->
                        []
                end,
                case Opts of
                    #{subtype:=SubType} ->
                        {subtype, SubType};
                    _ ->
                        []
                end,
                case Opts of
                    #{aliases:=Aliases} ->
                        {aliases, Aliases};
                    _ ->
                        []
                end,
                case Opts of
                    #{active:=true} ->
                        {active, true};
                    _ ->
                        []
                end,
                case Opts of
                    #{type_obj:=TypeObj} ->
                        {Type, TypeObj};
                    _ ->
                        []
                end
            ],
            {ok, maps:from_list(lists:flatten(Obj))};
        {error, object_not_found} ->
            {error, {could_not_load_parent, Parent}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec make_and_create(nkservice:id(), nkdomain:id(), nkdomain:type(), make_opts()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

make_and_create(Srv, Parent, Type, Opts) ->
    case make_obj(Srv, Parent, Type, Opts) of
        {ok, Obj} ->
            %% lager:warning("Obj: ~p", [Obj]),
            Meta1 = case maps:is_key(obj_id, Opts) orelse maps:is_key(name, Opts) of
                true ->
                    #{};
                false ->
                    #{skip_path_check=>true}
            end,
            Meta2 = case maps:find(wait_for_save, Opts) of
                {ok, Time} when is_integer(Time), Time > 1 ->
                    Meta1#{wait_for_save=>Time};
                error ->
                    Meta1
            end,
            case create(Srv, Obj, Meta2) of
                #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
                    {ok, ObjId, Path, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates a new object
-spec create(nkservice:id(), map(), nkdomain:create_opts()) ->
    #obj_id_ext{} | {error, term()}.

create(Srv, #{obj_id:=ObjId, type:=Type}=Obj, Meta) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:object_parse(SrvId, load, Type, Obj) of
                {ok, #{obj_id:=ObjId, path:=Path}=Obj2} ->
                    % We know type is valid here
                    Ext = #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path},
                    do_create(Ext, Obj2, Meta);
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @private
do_create(#obj_id_ext{srv_id=SrvId}=Ext, #{parent_id:=ParentId}=Obj, Meta) ->
    case load(SrvId, ParentId, #{}) of
        #obj_id_ext{pid=ParentPid} ->
            case nkdomain_obj:create_child(ParentPid, Obj, Meta) of
                {ok, ObjPid} ->
                    case Meta of
                        #{wait_for_save:=Time} ->
                            case nkdomain_obj:wait_for_save(ObjPid, Time) of
                                ok ->
                                    Ext#obj_id_ext{pid=ObjPid};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        _ ->
                            Ext#obj_id_ext{pid=ObjPid}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, {could_not_load_parent, ParentId}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case find_loaded(IdOrPath) of
                #obj_id_ext{}=ObjIdExt ->
                    ObjIdExt#obj_id_ext{srv_id=SrvId};
                not_found ->
                    case SrvId:object_store_find_obj(SrvId, IdOrPath) of
                        {ok, Type, ObjId, Path} ->
                            #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path};
                        {error, Error} ->
                            {error, Error}
                    end
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path(), nkdomain:load_opts()) ->
    #obj_id_ext{} | {error, obj_not_found|term()}.

load(Srv, IdOrPath, Meta) ->
    case find(Srv, IdOrPath) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            case Meta of
                #{register:=Link} ->
                    nkdomain_obj:register(Pid, Link);
                _ ->
                    ok
            end,
            ObjIdExt;
        #obj_id_ext{}=ObjIdExt ->
            do_load2(ObjIdExt, Meta);
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_load2(#obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path}=ObjIdExt, Meta) ->
    case SrvId:object_load(SrvId, ObjId) of
        {ok, #{type:=Type, obj_id:=ObjId, path:=Path}=Obj} ->
            do_load3(ObjIdExt, Obj, Meta);
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_load3(#obj_id_ext{type = ?DOMAIN_DOMAIN, path = <<"/">>, obj_id = <<"root">>}=ObjIdExt, Obj, Meta) ->
    #obj_id_ext{srv_id=SrvId} = ObjIdExt,
    {ok, Pid} = nkdomain_obj:start(SrvId, Obj, Meta),
    ObjIdExt#obj_id_ext{pid=Pid};

do_load3(#obj_id_ext{srv_id=SrvId}=ObjIdExt, #{parent_id:=ParentId}=Obj, Meta) ->
    LoadPath = maps:get(load_path, Meta, []),
    case lists:member(ParentId, LoadPath) of
        true ->
            {error, object_parent_conflict};
        false ->
            case load(SrvId, ParentId, #{load_path=>[ParentId|LoadPath]}) of
                #obj_id_ext{obj_id=ParentId, pid=ParentPid} when is_pid(ParentPid) ->
                    case nkdomain_obj:load_child(ParentPid, Obj, Meta) of
                        {ok, ObjPid} ->
                            ObjIdExt#obj_id_ext{pid=ObjPid};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, object_not_found} ->
                    {error, {could_not_load_parent, ParentId}};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
unload(Srv, Id, Reason, NotFound) ->
    case find(Srv, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:unload(Pid, Reason);
        #obj_id_ext{} ->
            {error, object_not_started};
        {error, object_not_found} ->
            {error, NotFound};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
sync_op(Srv, Id, Type, Msg, NotFound) ->
    case load(Srv, Id, #{}) of
        #obj_id_ext{type=Type, pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:sync_op(Pid, Msg);
        #obj_id_ext{} ->
            {error, invalid_object};
        {error, object_not_found} ->
            {error, NotFound};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
async_op(Srv, Id, Type, Msg, NotFound) ->
    case load(Srv, Id, #{}) of
        #obj_id_ext{type=Type, pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:async_op(Pid, Msg);
        #obj_id_ext{} ->
            {error, invalid_object};
        {error, object_not_found} ->
            {error, NotFound};
        {error, Error} ->
            {error, Error}
    end.






%% ===================================================================
%% Private
%% ===================================================================


%% @private
reg(Type, ObjId, Path) ->
    case nkdist_reg:reg(proc, nkdomain, ObjId, #{meta=>{Type, path, Path}}) of
        ok ->
            nkdist_reg:reg(reg, nkdomain, Path, #{meta=>{Type, obj_id, ObjId}});
        {error, Error} ->
            {error, Error}
    end.


%% @private
find_loaded(IdOrPath) when is_binary(IdOrPath) ->
    case nkdist_reg:find(nkdomain, IdOrPath) of
        {ok, {Type, path, Path}, Pid} ->
            #obj_id_ext{type=Type, obj_id=IdOrPath, path=Path, pid=Pid};
        {ok, {Type, obj_id, ObjId}, Pid} ->
            #obj_id_ext{type=Type, obj_id=ObjId, path=IdOrPath, pid=Pid};
        _ ->
            not_found
    end;

%%find_loaded(IdOrPath) when is_binary(IdOrPath) ->
%%    case nklib_proc:values({nkdomain_obj, IdOrPath}) of
%%        [{{Type, Path}, Pid}] ->
%%            #obj_id_ext{type=Type, obj_id=IdOrPath, path=Path, pid=Pid};
%%        [] ->
%%            case nklib_proc:values({nkdomain_obj, path, IdOrPath}) of
%%                [{{Type, ObjId}, Pid}] ->
%%                    #obj_id_ext{type=Type, obj_id=ObjId, path=IdOrPath, pid=Pid};
%%                [] ->
%%                    not_found
%%            end
%%    end;

find_loaded(ObjId) ->
    find_loaded(nklib_util:to_binary(ObjId)).


%% @private
call(Id, Msg) ->
    call(Id, Msg, ?DEF_SYNC_CALL).


%% @private
call(Pid, Msg, Timeout) when is_pid(Pid) ->
    nkservice_util:call(Pid, Msg, Timeout);

call(Id, Msg, Timeout) ->
    case find_loaded(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            call(Pid, Msg, Timeout);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);

cast(Id, Msg) ->
    case find_loaded(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            cast(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
info(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;

info(Id, Msg) ->
    case find_loaded(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            info(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
to_bin(T) -> nklib_util:to_binary(T).