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

-export([find/1, find_loaded/1, load/1, create/1, create/2]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkdomain:obj_id()|nkdomain:path()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find(Id) ->
    Id2 = to_bin(Id),
    case find_loaded(Id2) of
        #obj_id_ext{}=ObjIdExt ->
            ObjIdExt;
        not_found ->
            case find_in_db(Id2) of
                #obj_id_ext{}=ObjIdExt ->
                    ObjIdExt;
                {alias, #obj_id_ext{obj_id=ObjId}=ObjIdExt} ->
                    case find_loaded(ObjId) of
                        #obj_id_ext{}=ObjIdExt2 ->
                            ObjIdExt2;
                        not_found ->
                            ObjIdExt
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private It will not find by aliases
-spec find_loaded(binary()) ->
    #obj_id_ext{} | not_found.

find_loaded(Id) ->
    nkdomain_proc:find(to_bin(Id)).


%% @private
find_in_db(Id) ->
    case ?CALL_NKROOT(object_db_find_obj, [Id]) of
        {ok, Srv, Type, ObjId, Path} ->
            {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
            SrvId = load_srv(Srv),
            #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, obj_name=ObjName};
        {error, object_not_found} ->
            case ?CALL_NKROOT(object_db_search_alias, [Id]) of
                {ok, 0, []} ->
                    {error, object_not_found};
                {ok, N, [{Srv, Type, ObjId, Path}|_]}->
                    case N > 1 of
                        true ->
                            lager:notice("NkDOMAIN: duplicated alias for ~s", [Id]);
                        false ->
                            ok
                    end,
                    {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
                    SrvId = load_srv(Srv),
                    Alias = #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, obj_name=ObjName},
                    {alias, Alias};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkdomain:id()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

load(Id) ->
    case find(Id) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            ObjIdExt;
        #obj_id_ext{obj_id=ObjId, path=Path}=ObjIdExt ->
            case ?CALL_NKROOT(object_db_read, [ObjId]) of
                {ok, #{path:=Path}=Obj, _Meta} ->
                    case nkdomain_obj:start(Obj, loaded, #{}) of
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


%% @private
load_srv(Srv) ->
    case catch binary_to_existing_atom(Srv, latin1) of
        {'EXIT', _} ->
            lager:warning("NkDOMAIN: loading object with unknown service ~p", [Srv]),
            binary_to_atom(Srv, latin1);
        SrvId ->
            SrvId
    end.



%% @doc Creates a new object
-spec create(nkdomain:obj()) ->
    #obj_id_ext{} | {error, term()}.

create(Obj) ->
    create(Obj, #{}).


%% @doc Creates a new object
-spec create(nkdomain:obj(), nkdomain:start_opts()) ->
    #obj_id_ext{} | {error, term()}.

create(#{srv_id:=SrvId, type:=Type, obj_id:=ObjId, path:=Path}=Obj, Meta) ->
    case ?CALL_NKROOT(object_db_find_obj, [Path]) of
        {error, object_not_found} ->
            case nkdomain_obj:start(Obj, created, Meta) of
                {ok, Pid} ->
                    #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {ok, _, _, _, _} ->
            {error, object_already_exists};
        {error, Error} ->
            {error, Error}
    end.



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
