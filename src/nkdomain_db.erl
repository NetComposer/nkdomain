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

%% @doc NkDomain db-related module
-module(nkdomain_db).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([find/1, find/2, find_loaded/1, read/1, read/2, load/1, load/2]).
-export([delete/1, delete/2, hard_delete/1, hard_delete/2]).
-export([search/1, search/2, iterate/3, iterate/4, aggs/1, aggs/2]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN DB "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

-type opts() ::
    #{
        srv_id => atom(),
        get_deleted => boolean()
    }.

-type obj_id() :: nkdomain:obj_id().
-type type() :: nkdomain:type().
-type path() :: nkdomain:path().


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(obj_id()|path()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find(Id) ->
    find(Id, #{}).


%% @doc Finds and object using a service's functions
-spec find(obj_id()|path(), opts()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find(Id, Opts) ->
    Id2 = to_bin(Id),
    case find_loaded(Id2) of
        #obj_id_ext{}=ObjIdExt ->
            ObjIdExt;
        not_found ->
            case find_in_db(Id2, Opts) of
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
find_in_db(Id, Opts) ->
    SrvId = maps:get(srv_id, Opts, ?NKROOT),
    FindDeleted = maps:get(get_deleted, Opts, false),
    case ?CALL_SRV(SrvId, object_db_find_obj, [Id, FindDeleted]) of
        {ok, Type, ObjId, Path} ->
            {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
            #obj_id_ext{type=Type, obj_id=ObjId, path=Path, obj_name=ObjName};
        {error, object_not_found} ->
            case search(core, {alias, Id, #{get_deleted=>FindDeleted}}, Opts) of
                {ok, 0, []} ->
                    {error, object_not_found};
                {ok, N, [#{<<"type">>:=Type, <<"obj_id">>:=ObjId, <<"path">>:=Path}|_]}->
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
-spec read(obj_id()) ->
    {ok, #obj_id_ext{}, nkdomain:obj()} | {deleted, #obj_id_ext{}, nkdomain:obj()} | {error, term()}.

read(Id) ->
    read(Id, #{}).


%% @doc Reads an object from memory if loaded, or disk if not
-spec read(obj_id(), opts()) ->
    {ok, #obj_id_ext{}, nkdomain:obj()} | {error, term()}.

read(Id, Opts) ->
    case find(Id, Opts) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            case nkdomain:get_obj(Pid) of
                {ok, Obj} ->
                    {ok, ObjIdExt, Obj};
                {error, Error} ->
                    {error, Error}
            end;
        #obj_id_ext{obj_id=ObjId}=ObjIdExt ->
            case do_read(ObjId, Opts) of
                {ok, Obj} ->
                    {ok, ObjIdExt, Obj};
                {deleted, Obj} ->
                    {deleted, ObjIdExt, Obj};
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
    load(Id, #{}).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkdomain:id(), opts()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

load(Id, Opts) ->
    case find(Id, Opts) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            ObjIdExt;
        #obj_id_ext{obj_id=ObjId, path=Path}=ObjIdExt ->
            case do_read(ObjId, Opts#{get_deleted=>false}) of
                {ok, #{path:=Path}=Obj} ->
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


%% @doc Marks an object as deleted
-spec delete(nkdomain:id()) ->
    ok | {error, object_not_found|term()}.

delete(Id) ->
    delete(Id, #{}).


%% @doc Marks an object as deleted
-spec delete(nkdomain:id(), opts()) ->
    ok | {error, object_not_found|term()}.

delete(Id, Opts) ->

    %%case nkdomain_store_es_search:search_childs(ObjId, #{size=>0}, EsOpts) of
    %%{ok, 0, []} ->
    %%case nkdomain_lib:find_loaded(ObjId) of
    %%#obj_id_ext{pid=Pid} ->
    %%nkdomain_obj:object_deleted(Pid);
    %%not_found ->
    %%ok
    %%end,

    case find(Id, Opts#{get_deleted=>false}) of
        #obj_id_ext{obj_id=ObjId, pid=Pid} ->
            case is_pid(Pid) of
                true ->
                    nkdomain_obj:object_deleted(Pid);
                false ->
                    ok
            end,
            SrvId = maps:get(srv_id, Opts, ?NKROOT),
            case ?CALL_SRV(SrvId, object_db_read, [ObjId]) of
                {ok, Map, _Meta} ->
                    case ?CALL_SRV(SrvId, object_parse, [load, Map]) of
                        {ok, Obj, _Unknown} ->
                            Obj2 = Obj#{
                                is_deleted => true,
                                deleted_time => nkdomain_util:timestamp()
                            },
                            Obj3 = maps:without([active, expires_time, in_alarm, alarms, enabled], Obj2),
                            case ?CALL_SRV(SrvId, object_db_save, [Obj3]) of
                                {ok, _Meta2} ->
                                    ok;
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            ?LLOG(notice, "cannot parse object ~s (~p): ~p", [ObjId, Error, Map]),
                            io:format("Invalid object: ~s\n\n", [nklib_json:encode_pretty(Map)]),
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc Marks an object as deleted
-spec hard_delete(nkdomain:id()) ->
    ok | {error, object_not_found|term()}.

hard_delete(Id) ->
    hard_delete(Id, #{}).


%% @doc Marks an object as hard_deleted
-spec hard_delete(nkdomain:id(), opts()) ->
    ok | {error, object_not_found|term()}.

hard_delete(Id, Opts) ->
    case find(Id, Opts#{get_deleted=>true}) of
        #obj_id_ext{obj_id=ObjId} ->
            SrvId = maps:get(srv_id, Opts, ?NKROOT),
            case ?CALL_SRV(SrvId, object_db_delete, [ObjId]) of
                {ok, _Meta} ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Internal search (non GraphQL). See nkdomain:search_type()
-spec search(nkdomain:search_type()) ->
    {ok, integer(), RawObj::map()} | {error, term()}.

search(SearchType) ->
    search(core, SearchType, #{}).


%% @doc
-spec search(type()|core, nkdomain:search_type()) ->
    {ok, integer(), RawObj::map()} | {error, term()}.

search(Type, SearchType) ->
    search(Type, SearchType, #{}).


%% @doc
-spec search(type()|core, nkdomain:search_type(), opts()) ->
    {ok, integer(), RawObj::map()} | {error, term()}.

search(Type, SearchType, Opts) ->
    SrvId = maps:get(srv_id, Opts, ?NKROOT),
    case ?CALL_SRV(SrvId, object_db_search_objs, [SrvId, Type, SearchType]) of
        {ok, Total, Data} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


-type iter_fun() :: fun(({type(), obj_id(), path(), Fields::map()}, term()) -> {ok, term()}).

%% @doc Internal iteration
-spec iterate(nkdomain:search_type(), iter_fun(), term()) ->
    {ok, term()} | {error, term()}.

iterate(SearchType, Fun, Acc) ->
    iterate(core, SearchType, Fun, Acc, #{}).


%% @doc Internal iteration
-spec iterate(type()|core, nkdomain:search_type(), iter_fun(), term()) ->
    {ok, term()} | {error, term()}.

iterate(Type, SearchType, Fun, Acc) ->
    iterate(Type, SearchType, Fun, Acc, #{}).


%% @doc
-spec iterate(type()|core, nkdomain:search_type(), iter_fun(), term(), opts()) ->
    {ok, term()} | {error, term()}.

iterate(Type, SearchType, Fun, Acc, Opts) ->
    SrvId = maps:get(srv_id, Opts, ?NKROOT),
    ?CALL_SRV(SrvId, object_db_iterate_objs, [SrvId, Type, SearchType, Fun, Acc]).


%% @doc Internal aggregation
-spec aggs(nkdomain:aggs_type()) ->
    {ok, integer(), [{binary(), integer()}]} | {error, term()}.

aggs(AggType) ->
    aggs(core, AggType, #{}).


%% @doc Internal aggregation
-spec aggs(type()|core, nkdomain:aggs_type()) ->
    {ok, integer(), [{binary(), integer()}]} | {error, term()}.

aggs(Type, AggType) ->
    aggs(Type, AggType, #{}).


%% @doc
-spec aggs(type()|core, nkdomain:agg_type(), opts()) ->
    {ok, integer(), [{binary(), integer()}]} | {error, term()}.

aggs(Type, AggType, Opts) ->
    SrvId = maps:get(srv_id, Opts, ?NKROOT),
    case ?CALL_SRV(SrvId, object_db_agg_objs, [SrvId, Type, AggType]) of
        {ok, Total, Data, _Meta} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_read(ObjId, Opts) ->
    SrvId = maps:get(srv_id, Opts, ?NKROOT),
    case ?CALL_SRV(SrvId, object_db_read, [ObjId]) of
        {ok, #{<<"is_deleted">>:=true}=Map, _Meta} ->
            case Opts of
                #{get_deleted:=true} ->
                    case ?CALL_SRV(SrvId, object_parse, [load, Map]) of
                        {ok, Obj, _Unknown} ->
                            {deleted, Obj};
                        {error, Error} ->
                            ?LLOG(notice, "cannot parse object ~s (~p): ~p", [ObjId, Error, Map]),
                            io:format("Invalid object: ~s\n\n", [nklib_json:encode_pretty(Map)]),
                            {error, Error}
                    end;
                _ ->
                    {error, object_not_found}
            end;
        {ok, Map, _Meta} ->
            case ?CALL_SRV(SrvId, object_parse, [load, Map]) of
                {ok, Obj, _Unknown} ->
                    case check_object(SrvId, Obj) of
                        ok ->
                            {ok, Obj};
                        removed ->
                            {error, object_not_found}
                    end;
                {error, Error} ->
                    ?LLOG(notice, "cannot parse object ~s (~p): ~p", [ObjId, Error, Map]),
                    io:format("Invalid object: ~s\n\n", [nklib_json:encode_pretty(Map)]),
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


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
