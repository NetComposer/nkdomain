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

%% @doc NkDomain main module
-module(nkdomain_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([find/2, find_path/2, find_obj/2, find_loaded/2, load/2, create/2, create/3]).
-export([get_node/1]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()|undefined} | {error, object_not_found|term()}.

find(SrvId, Id) ->
    Id2 = to_bin(Id),
    case find_loaded(SrvId, Id2) of
        {ok, Type, ObjId, Pid} ->
            {ok, Type, ObjId, Pid};
        {error, object_not_found} ->
            case find_in_db(SrvId, Id) of
                {ok, Type, ObjId, _Path} ->
                    {ok, Type, ObjId, undefined};
                {alias, Type, ObjId, _Path} ->
                    case find_loaded(SrvId, ObjId) of
                        {ok, Type, ObjId, Pid} ->
                            {ok, Type, ObjId, Pid};
                        {error, object_not_found}    ->
                            {ok, Type, ObjId, undefined};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds object's path from UUID or Path, in memory and disk
-spec find_path(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

find_path(SrvId, Id) ->
    case find_loaded(SrvId, Id) of
        {ok, Type, ObjId, Pid} ->
            case nkdomain_obj:sync_op(SrvId, Pid, get_path) of
                {ok, Path} ->
                    {ok, Type, ObjId, Path};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            case find_in_db(SrvId, to_bin(Id)) of
                {ok, Type, ObjId, Path} ->
                    {ok, Type, ObjId, Path};
                {alias, Type, ObjId, Path} ->
                    {ok, Type, ObjId, Path};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds objects with full #obj_id_ext{}
-spec find_obj(nkservice:id(), nkdomain:id()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find_obj(SrvId, Id) ->
    case find(SrvId, Id) of
        {ok, Type, ObjId, Pid} ->
            case nkdomain_obj:sync_op(SrvId, Pid, get_path) of
                {ok, Path} ->
                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc
%%find_loaded(SrvId, Id) ->
%%    Id2 = to_bin(Id),
%%    case nkdist_reg:find({nkdomain, SrvId}, Id2) of
%%        {ok, {Type, path, Path}, Pid} ->
%%            #obj_id_ext{type=Type, obj_id=Id2, path=Path, pid=Pid, srv_id=SrvId};
%%        {ok, {Type, obj_id, ObjId}, Pid} ->
%%            #obj_id_ext{type=Type, obj_id=ObjId, path=Id2, pid=Pid, srv_id=SrvId};
%%        _ ->
%%            not_found
%%    end.


-spec find_loaded(nkservice:id(), binary()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} | {error, object_not_found|term()}.

find_loaded(SrvId, Id) ->
    case nkdomain_util:is_path(Id) of
        {false, Id2} ->
            case nkdist_reg:find({nkdomain, SrvId}, Id2) of
                {ok, Type, Pid} ->
                    {ok, Type, Id2, Pid};
                _ ->
                    {error, object_not_found}
            end;
        {true, Path} ->
            nkdomain_domain_obj:find_path(SrvId, Path)
    end.


%% @private
find_in_db(SrvId, Id) ->
    case SrvId:object_db_find_obj(SrvId, Id) of
        {ok, Type, ObjId, Path} ->
            {ok, Type, ObjId, Path};
        {error, object_not_found} ->
            case SrvId:object_db_search_alias(SrvId, Id) of
                {ok, 0, []} ->
                    {error, object_not_found};
                {ok, N, [{Type, ObjId, Path}|_]}->
                    case N > 1 of
                        true ->
                            lager:notice("NkDOMAIN: duplicated alias for ~s", [Id]);
                        false ->
                            ok
                    end,
                    {alias, Type, ObjId, Path};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:id()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

load(SrvId, Id) ->
    case find(SrvId, Id) of
        {ok, Type, ObjId, Pid} when is_pid(Pid) ->
            case nkdomain_obj:sync_op(SrvId, Pid, get_path) of
                {ok, Path} ->
                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {ok, Type, ObjId, _} ->
            case SrvId:object_db_read(SrvId, ObjId) of
                {ok, #{path:=Path}=Obj, _Meta} ->
                    Obj2 = case Obj of
                        #{obj_name:=_} ->
                            Obj;
                        #{type:=Type} ->
                            {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
                            Obj#{obj_name=>ObjName}
                    end,
                    case nkdomain_obj:start(SrvId, Obj2, #{}) of
                        {ok, Pid} ->
                            #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=Pid};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates a new object
-spec create(nkservice:id(), nkdomain:obj()) ->
    #obj_id_ext{} | {error, term()}.

create(SrvId, Obj) ->
    create(SrvId, Obj, #{}).


%% @doc Creates a new object
-spec create(nkservice:id(), nkdomain:obj(), nkdomain:start_opts()) ->
    #obj_id_ext{} | {error, term()}.

create(SrvId, #{type:=Type, obj_id:=ObjId, path:=Path}=Obj, Meta) ->
    case SrvId:object_db_find_obj(SrvId, Path) of
        {error, object_not_found} ->
            case SrvId:object_db_save(SrvId, Obj) of
                {ok, _Meta} ->
                    case nkdomain_obj:start(SrvId, Obj, Meta#{is_created=>true}) of
                        {ok, Pid} ->
                            #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid, srv_id=SrvId};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {ok, _, _, _} ->
            {error, object_already_exists};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_node(ObjId) ->
    case nkdist:get_vnode(nkdomain, ObjId, #{}) of
        {ok, Node, _Idx} ->
            {ok, Node};
        {error, Error} ->
            {error, Error}
    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
