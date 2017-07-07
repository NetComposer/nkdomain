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

-export([find/2, find_loaded/2, load/2, create/2, create/3]).
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
    #obj_id_ext{} | {error, object_not_found|term()}.

find(SrvId, Id) ->
    Id2 = to_bin(Id),
    case find_loaded(SrvId, Id2) of
        #obj_id_ext{}=ObjIdExt ->
            ObjIdExt;
        not_found ->
            case SrvId:object_db_find_obj(SrvId, Id2) of
                {ok, Type, ObjId, Path} ->
                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path};
                {error, object_not_found} ->
                    case SrvId:object_db_search_alias(SrvId, Id2) of
                        {ok, 0, []} ->
                            {error, object_not_found};
                        {ok, N, [{Type, ObjId, Path}|_]}->
                            case N > 1 of
                                true ->
                                    lager:notice("NkDOMAIN: duplicated alias for ~s", [Id]);
                                false ->
                                    ok
                            end,
                            case find_loaded(SrvId, ObjId) of
                                #obj_id_ext{}=ObjIdExt ->
                                    ObjIdExt;
                                not_found ->
                                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end
            end
    end.


%% @doc
find_loaded(SrvId, Id) ->
    Id2 = to_bin(Id),
    case nkdist_reg:find({nkdomain, SrvId}, Id2) of
        {ok, {Type, path, Path}, Pid} ->
            #obj_id_ext{type=Type, obj_id=Id2, path=Path, pid=Pid, srv_id=SrvId};
        {ok, {Type, obj_id, ObjId}, Pid} ->
            #obj_id_ext{type=Type, obj_id=ObjId, path=Id2, pid=Pid, srv_id=SrvId};
        _ ->
            not_found
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:id()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

load(SrvId, Id) ->
    case find(SrvId, Id) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            ObjIdExt;
        #obj_id_ext{obj_id=ObjId}=ObjIdExt ->
            case SrvId:object_db_read(SrvId, ObjId) of
                {ok, Obj, _Meta} ->
                    case nkdomain_obj:start(SrvId, Obj, #{}) of
                        {ok, Pid} ->
                            ObjIdExt#obj_id_ext{pid=Pid};
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
