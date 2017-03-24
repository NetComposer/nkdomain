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

-export([find/2, load/2, load/3, create/3]).
-export([make_obj/4, remove/2]).

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
        father => binary(),
        referred_id => nkdomain:obj_id()
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, nkdomain:type(), domain:obj_id(), nkdomain:path(), pid()|undefined} |
    {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkdomain_util:is_path(IdOrPath) of
        {true, Path} ->
            case nkservice_srv:get_srv_id(Srv) of
                {ok, SrvId} ->
                    case SrvId:object_store_find_path(SrvId, Path) of
                        {ok, Type, ObjId} ->
                            case do_find(ObjId) of
                                {ok, Type, ObjId, Path, Pid} ->
                                    {ok, Type, ObjId, Path, Pid};
                                not_found ->
                                    {ok, Type, ObjId, Path, undefined}
                            end;
                        {error, object_not_found} ->
                            {error, {path_not_found, Path}};
                        {error, Error} ->
                            {error, Error}
                    end;
                not_found ->
                    {error, service_not_found}
            end;
        false ->
            ObjId = nklib_util:to_binary(IdOrPath),
            case do_find(ObjId) of
                {ok, Type, ObjId, Path, Pid} ->
                    {ok, Type, ObjId, Path, Pid};
                not_found ->
                    case nkservice_srv:get_srv_id(Srv) of
                        {ok, SrvId} ->
                            case SrvId:object_store_find_obj_id(SrvId, ObjId) of
                                {ok, Type, Path} ->
                                    {ok, Type, ObjId, Path, undefined};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        not_found ->
                            {error, service_not_found}
                    end
            end
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, IdOrPath) ->
    load(Srv, IdOrPath, #{}).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path(), nkdomain:load_opts()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, IdOrPath, Meta) ->
    case find(Srv, IdOrPath) of
        {ok, Type, ObjId, Path, Pid} when is_pid(Pid) ->
            case Meta of
                #{register:=Link} ->
                    register(Pid, Link);
                _ ->
                    ok
            end,
            {ok, Type, ObjId, Path, Pid};
        {ok, _Type, ObjId, _Path, undefined} ->
            do_load2(Srv, ObjId, Meta);
        {error, {path_not_found, _}} ->
            {error, object_not_found};
        {error, object_not_found} ->
            ObjId = nklib_util:to_binary(IdOrPath),
            do_load2(Srv, ObjId, Meta)
    end.


%% @private
do_load2(Srv, ObjId, Meta) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            Meta2 = Meta#{
                srv_id => SrvId,
                is_dirty => false
            },
            case SrvId:object_load(SrvId, ObjId) of
                {ok, _Module, Obj} ->
                    case Obj of
                        #{expires_time:=Expires} ->
                            case nklib_util:m_timestamp() of
                                Now when Now >= Expires ->
                                    nkdomain_store:delete(SrvId, ObjId),
                                    {error, object_not_found};
                                _ ->
                                    do_load3(ObjId, Obj, Meta2)
                            end;
                        _ ->
                            do_load3(ObjId, Obj, Meta2)
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private
do_load3(ObjId, #{type:=Type, path:=Path}=Obj, Meta2) ->
    {ok, ObjPid} = nkdomain_obj:start(Obj, Meta2),
    {ok, Type, ObjId, Path, ObjPid}.


%% @doc Adds type, obj_id, parent_id, path, created_time
-spec make_obj(nkservice:id(), nkdomain:type(), map(), make_opts()) ->
    {ok, nkdomain:obj()} | {error, term()}.

make_obj(Srv, Type, Base, Opts) ->
    Type2 = to_bin(Type),
    ObjId = case Opts of
        #{obj_id:=ObjId0} ->
            nkdomain_util:name(ObjId0);
        _ ->
             <<Type2/binary, $-, (nklib_util:luid())/binary>>
    end,
    Name1 = case Opts of
        #{name:=Name0} -> nkdomain_util:name(Name0);
        _ -> ObjId
    end,
    Name2 = case Type2 of
        ?DOMAIN_DOMAIN ->
            Name1;
        _ ->
            <<Type2/binary, "s/", Name1/binary>>
    end,
    Obj1 = Base#{
        obj_id => ObjId,
        type => Type,
        created_time => nklib_util:m_timestamp()
    },
    Obj2 = case Opts of
        #{referred_id:=ReferredId} ->
            Obj1#{referred_id => nkdomain_util:name(ReferredId)};
        _ ->
            Obj1
    end,
    case Opts of
        #{father:=Father} ->
            do_make_obj(Srv, Name2, Father, Obj2);
        _ ->
            case nkdomain_util:get_service_domain(Srv) of
                undefined ->
                    {error, missing_father};
                FatherId ->
                    do_make_obj(Srv, Name2, FatherId, Obj2)
            end

    end.


%% @private
do_make_obj(Srv, Name, Father, Obj) ->
    case find(Srv, Father) of
        {ok, _FatherType, FatherId, FatherPath, _FatherPid} ->
            BasePath = case FatherPath of
                <<"/">> -> <<>>;
                _ -> FatherPath
            end,
            Obj2 = Obj#{
                parent_id => FatherId,
                path => <<BasePath/binary, $/, Name/binary>>
            },
            lager:info("OBJ: ~p", [Obj2]),
            {ok, Obj2};
        {error, object_not_found} ->
            {error, father_not_found};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates a new object
-spec create(nkservice:id(), map(), nkdomain:create_opts()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path(), pid()}.

create(Srv, #{obj_id:=ObjId, type:=Type}=Obj, Meta) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:object_store_read_raw(SrvId, ObjId) of
                {error, object_not_found} ->
                    do_create(SrvId, Type, Obj, Meta);
                {ok, _} ->
                    {error, object_already_exists};
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @private
do_create(SrvId, Type, Obj, Meta) ->
    case SrvId:object_parse(SrvId, load, Type, Obj) of
        {ok, _Module, #{obj_id:=ObjId, path:=Path}=Obj2} ->
            % We know type is valid here
            case do_create_check_parent(SrvId, Obj2) of
                {ok, Obj3} ->
                    Meta2 = Meta#{
                        srv_id => SrvId,
                        is_dirty => true
                    },
                    {ok, ObjPid} = nkdomain_obj:start(Obj3, Meta2),
                    {ok, Type, ObjId, Path, ObjPid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
remove(Srv, Id) ->
    case load(Srv, Id) of
        {ok, _Type, _ObjId, _Path, Pid} when is_pid(Pid) ->
            nkdomain_obj:delete(Pid, normal);
        {ok, _Type, ObjId, _Path, _Pid} ->
            {ok, SrvId} = nkservice_srv:get_srv_id(Srv),
            SrvId:object_store_delete_raw(SrvId, ObjId);
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Private
%% ===================================================================


%% @private
do_create_check_parent(_SrvId, #{type:=<<"domain">>, obj_id:=<<"root">>, path:=<<"/">>, parent_id:=<<>>}=Obj) ->
    {ok, Obj};

do_create_check_parent(SrvId, #{parent_id:=ParentId, type:=Type, path:=Path}=Obj) ->
    case load(SrvId, ParentId, #{}) of                          % TODO: Use some usage?
        {ok, _ParentType, ParentId, _ParentPath, Pid} ->
            case do_call(Pid, {nkdomain_check_create_child, Type, Path}) of
                ok ->
                    {ok, Obj};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            lager:notice("Error loading parent object ~s (~p)", [ParentId, Error]),
            {error, could_not_load_parent}
    end.


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
            {ok, Type, ObjId, Path, Pid};
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
        {ok, _Type, _ObjId, _Path, Pid} when is_pid(Pid) ->
            do_call(Pid, Msg, Timeout);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);

do_cast(Id, Msg) ->
    case do_find(Id) of
        {ok, _Type, _ObjId, _Path, Pid} when is_pid(Pid) ->
            do_cast(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
do_info(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;

do_info(Id, Msg) ->
    case do_find(Id) of
        {ok, _Type, _ObjId, _Path, Pid} when is_pid(Pid) ->
            do_info(Pid, Msg);
        not_found ->
            {error, obj_not_found}
    end.


%% @private
to_bin(T) -> nklib_util:to_binary(T).