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

%% @doc NkDomain library module
-module(nkdomain_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([find/1, find/2, find_loaded/1, read/1, read/2, load/1, load/2]).
-export([type_apply/3]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN LIB "++Txt, Args)).


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
    find(?NKROOT, Id).


%% @doc Finds and object using a service's functions
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find(SrvId, Id) ->
    Id2 = to_bin(Id),
    case find_loaded(Id2) of
        #obj_id_ext{}=ObjIdExt ->
            ObjIdExt;
        not_found ->
            case find_in_db(SrvId, Id2) of
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
find_in_db(SrvId, Id) ->
    case ?CALL_SRV(SrvId, object_db_find_obj, [Id]) of
        {ok, Type, ObjId, Path} ->
            {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
            #obj_id_ext{type=Type, obj_id=ObjId, path=Path, obj_name=ObjName};
        {error, object_not_found} ->
            case ?CALL_SRV(SrvId, object_db_search_alias, [Id]) of
                {ok, 0, []} ->
                    {error, object_not_found};
                {ok, N, [{Type, ObjId, Path}|_]}->
                    case N > 1 of
                        true ->
                            lager:notice("NkDOMAIN: duplicated alias for ~s", [Id]);
                        false ->
                            ok
                    end,
                    {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
                    Alias = #obj_id_ext{type=Type, obj_id=ObjId, path=Path, obj_name=ObjName},
                    {alias, Alias};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Reads an object from memory if loaded, or disk if not
-spec read(nkdomain:obj_id()) ->
    {ok, #obj_id_ext{}, nkdomain:obj()} | {error, term()}.

read(Id) ->
    read(?NKROOT, Id).


%% @doc Reads an object from memory if loaded, or disk if not
-spec read(nkservice:id(), nkdomain:obj_id()) ->
    {ok, #obj_id_ext{}, nkdomain:obj()} | {error, term()}.

read(SrvId, Id) ->
    case find(SrvId, Id) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            case nkdomain:get_obj(Pid) of
                {ok, Obj} ->
                    {ok, ObjIdExt, Obj};
                {error, Error} ->
                    {error, Error}
            end;
        #obj_id_ext{obj_id=ObjId}=ObjIdExt ->
            case ?CALL_SRV(SrvId, object_db_read, [ObjId]) of
                {ok, Map, _Meta} ->
                    case ?CALL_SRV(SrvId, object_parse, [load, Map]) of
                        {ok, Obj, _Unknown} ->
                            case check_object(SrvId, Obj) of
                                ok ->
                                    {ok, ObjIdExt, Obj};
                                removed ->
                                    {error, object_not_found}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
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
    load(?NKROOT, Id).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:id()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

load(SrvId, Id) ->
    case find(SrvId, Id) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            ObjIdExt;
        #obj_id_ext{obj_id=ObjId, path=Path}=ObjIdExt ->
            case ?CALL_SRV(SrvId, object_db_read, [ObjId]) of
                {ok, Map, _Meta} ->
                    case ?CALL_SRV(SrvId, object_parse, [load, Map]) of
                        {ok, #{path:=Path}=Obj, _Unknown} ->
                            case check_object(SrvId, Obj) of
                                ok ->
                                    case nkdomain_obj:start(Obj, loaded, #{}) of
                                        {ok, Pid} ->
                                            ObjIdExt#obj_id_ext{pid=Pid};
                                        {error, Error} ->
                                            {error, Error}
                                    end;
                                removed ->
                                    {error, object_not_found}
                            end;
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
check_object(SrvId, #{obj_id:=ObjId}=Obj) ->
    Res1 = case Obj of
        #{expires_time:=Expires} ->
            Now = nkdomain_util:timestamp(),
            case Now > Expires of
                true ->
                    removed = ?CALL_SRV(SrvId, object_do_expired, [ObjId]);
                false ->
                    ok
            end;
        _ ->
            ok
    end,
    case Res1 of
        removed ->
            removed;
        ok ->
            case Obj of
                #{active:=true, type:=Type} ->
                    case ?CALL_SRV(SrvId, object_do_active, [Type, ObjId]) of
                        ok ->
                            ok;
                        processed ->
                            ok;
                        removed ->
                            removed
                    end;
                _ ->
                    ok
            end
    end.


%% @doc Calls an object's function
-spec type_apply(nkdomain:type()|module(), atom(), list()) ->
    not_exported | term().

type_apply(Module, Fun, Args) when is_atom(Module) ->
    case erlang:function_exported(Module, Fun, length(Args)) of
        true ->
            apply(Module, Fun, Args);
        false ->
            not_exported
    end;

type_apply(Type, Fun, Args) when is_binary(Type) ->
    Module = nkdomain_reg:get_type_module(Type),
    true = is_atom(Module),
    type_apply(Module, Fun, Args).



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
