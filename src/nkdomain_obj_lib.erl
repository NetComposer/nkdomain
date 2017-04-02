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
-export([make_obj/4, make_and_create/4, archive/3, delete/3, delete_all_childs/2]).
-export([do_find/1, do_call/2, do_call/3, do_cast/2, do_info/2]).

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
        type_obj => map()
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
                    nkdomain_util:name(Name0);
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
        {error, path_not_found} ->
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
            case nkdomain_obj_lib:create(Srv, Obj, #{}) of
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
                {ok, #{obj_id:=ObjId, parent_id:=ParentId, path:=Path}=Obj2} ->
                    % We know type is valid here
                    case load(SrvId, ParentId, #{}) of
                        #obj_id_ext{obj_id=ParentId, pid=ParentPid} ->
                            case nkdomain_obj:create_child(ParentPid, Obj2, Meta) of
                                {ok, ObjPid} ->
                                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=ObjPid};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, object_not_found} ->
                            {error, {could_not_load_parent, ParentId}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    #obj_id_ext{} | {error, object_not_found|path_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case nkdomain_util:is_path(IdOrPath) of
                {true, Path} ->
                    case SrvId:object_store_find_path(SrvId, Path) of
                        {ok, Type, ObjId} ->
                            case do_find(ObjId) of
                                #obj_id_ext{type=Type}=ObjIdExt ->
                                    ObjIdExt#obj_id_ext{srv_id=SrvId};
                                not_found ->
                                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path}
                            end;
                        {error, object_not_found} ->
                            {error, path_not_found};
                        {error, Error} ->
                            {error, Error}
                    end;
                false ->
                    ObjId = nklib_util:to_binary(IdOrPath),
                    case do_find(ObjId) of
                        #obj_id_ext{}=ObjIdExt ->
                            ObjIdExt#obj_id_ext{srv_id=SrvId};
                        not_found ->
                            case SrvId:object_store_find_obj_id(SrvId, ObjId) of
                                {ok, Type, Path} ->
                                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path};
                                {error, Error} ->
                                    {error, Error}
                            end
                    end
            end;
        not_found ->
            {error, service_not_found}
    end.



%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path(), nkdomain:load_opts()) ->
    #obj_id_ext{} | {error, obj_not_found|term()}.

%%load(Srv, IdOrPath, Meta) ->
%%    case find(Srv, IdOrPath) of
%%        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
%%            case Meta of
%%                #{register:=Link} ->
%%                    register(Pid, Link);
%%                _ ->
%%                    ok
%%            end,
%%            ObjIdExt;
%%        #obj_id_ext{}=ObjIdExt ->
%%            do_load2(ObjIdExt, Meta);
%%        {error, path_not_found} ->
%%            {error, object_not_found};
%%        {error, object_not_found} ->
%%            case nkservice_srv:get_srv_id(Srv) of
%%                {ok, SrvId} ->
%%                    ObjIdExt = #obj_id_ext{
%%                        srv_id = SrvId,
%%                        obj_id = nklib_util:to_binary(IdOrPath)
%%                    },
%%                    do_load2(ObjIdExt, Meta);
%%                not_found ->
%%                    {error, service_not_found}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.

load(Srv, IdOrPath, Meta) ->
    case find(Srv, IdOrPath) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            case Meta of
                #{register:=Link} ->
                    register(Pid, Link);
                _ ->
                    ok
            end,
            ObjIdExt;
        #obj_id_ext{}=ObjIdExt ->
            do_load2(ObjIdExt, Meta);
        {error, path_not_found} ->
            {error, object_not_found};
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


%% @doc Archives an object
-spec archive(nkservice:id(), nkdomain:obj_id(), nkservice:error()) ->
    ok | {error, term()}.

archive(SrvId, ObjId, Reason) ->
    case SrvId:object_load(SrvId, ObjId) of
        {ok, Obj} ->
            Obj2 = nkdomain_util:add_destroyed(SrvId, Reason, Obj),
            case nkdomain_store:archive(SrvId, ObjId, Obj2) of
                ok ->
                    nkdomain_store:delete(SrvId, ObjId);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc Remove an object
%% If the object can be loaded, it is sent a delete message
%% If not, it is deleted from disk
-spec delete(nkservice:id(), nkdomain:id(), nkservice:error()) ->
    ok | {error, term()}.

delete(Srv, Id, Reason) ->
    case find(Srv, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:delete(Pid, Reason);
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            nkdomain_store:delete(SrvId, ObjId);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
delete_all_childs(Srv, Id) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            SrvId:object_store_delete_all_childs(SrvId, Id, #{});
        not_found ->
            {error, service_not_found}
    end.




%% ===================================================================
%% Private
%% ===================================================================

%% @private
do_find({Srv, Path}) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:object_store_find_path(SrvId, Path) of
                {ok, _Type, ObjId} when is_binary(ObjId) ->
                    do_find(ObjId);
                _ ->
                    not_found
            end;
        not_found ->
            not_found
    end;

do_find(ObjId) when is_binary(ObjId) ->
    case nklib_proc:values({nkdomain_obj, ObjId}) of
        [{{Type, Path}, Pid}] ->
            #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid};
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
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            do_call(Pid, Msg, Timeout);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);

do_cast(Id, Msg) ->
    case do_find(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            do_cast(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_info(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;

do_info(Id, Msg) ->
    case do_find(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            do_info(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
to_bin(T) -> nklib_util:to_binary(T).