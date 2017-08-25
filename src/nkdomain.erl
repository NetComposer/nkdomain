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
-module(nkdomain).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').


-export([find/2, load/2, unload/2, unload/3, get_obj/2, get_info/2, get_name/2, get_domain_id/2]).
-export([enable/3, update/3, update_name/3, delete/2, send_info/4]).
-export([search/2, search_type/2, delete_all_childs/2, delete_all_childs_type/3, search_agg_field/5]).
-export([clean/1]).
-export_type([obj_id/0, obj_name/0, obj/0, path/0, id/0, type/0]).
-export_type([timestamp/0]).

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: binary().

-type obj_name() :: binary().

-type path() :: [binary()].

-type id() :: obj_id() | path().

-type type() :: binary().

%% @see nkdomain_callbacks:domain_store_base_mapping/0
-type obj() :: map().

-type search_spec() :: map().

-type timestamp() :: nklib_util:m_timestamp().

%% ===================================================================
%% Public
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), id()) ->
    {ok, type(), obj_id(), path(), pid()|undefined} | {error, object_not_found|term()}.

find(SrvId, Id) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%%%% @doc Finds and object from UUID or Path, in memory and disk
%%-spec find_obj(nkservice:id(), id()) ->
%%    {ok, type(), obj_id(), path(), pid()|undefined} | {error, object_not_found|term()}.
%%
%%find_obj(SrvId, Id) ->
%%    case nkdomain_lib:find_obj(SrvId, Id) of
%%        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
%%            {ok, Type, ObjId, Path, Pid};
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), id()) ->
    {ok, type(), obj_id(), path(), pid()} |  {error, object_not_found|term()}.

load(SrvId, Id) ->
    case nkdomain_lib:load(SrvId, Id) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_obj(nkservice:id(), id()) ->
    {ok, obj()} | {error, term()}.

get_obj(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, get_obj).


%% @doc
-spec get_info(nkservice:id(), id()) ->
    {ok, map()} | {error, term()}.

get_info(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, get_obj_info).


%% @doc
-spec get_name(nkservice:id(), id()) ->
    {ok, map()} | {error, term()}.

get_name(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, get_obj_name).


%% @doc
-spec get_domain_id(nkservice:id(), id()) ->
    {ok, nkdomain:obj_id()} | {error, term()}.

get_domain_id(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, get_domain_id).


%% @doc Enables/disabled an object
-spec enable(nkservice:id(), id(), boolean()) ->
    ok | {error, term()}.

enable(SrvId, Id, Enable) ->
    nkdomain_obj:sync_op(SrvId, Id, {enable, Enable}).


%% @doc Updates an object
-spec update(nkservice:id(), id(), map()) ->
    {ok, UnknownFields::[binary()]} | {error, term()}.

update(SrvId, Id, Update) ->
    nkdomain_obj:sync_op(SrvId, Id, {update, Update}).


%% @doc Updates an object's obj_name
-spec update_name(nkservice:id(), id(), binary()) ->
    ok | {error, term()}.

update_name(SrvId, Id, ObjName) ->
    nkdomain_obj:sync_op(SrvId, Id, {update_name, ObjName}).


%% @doc Remove an object
-spec delete(nkservice:id(), id()) ->
    {ok, binary()} | {error, term()}.

delete(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, delete).


%% @doc Sends an INFO
-spec send_info(nkservice:id(), id(), Info::atom()|binary(), Body::map()) ->
    ok | {error, term()}.

send_info(SrvId, Id, Info, Body) when is_map(Body) ->
    nkdomain_obj:async_op(SrvId, Id, {send_info, Info, Body}).


%% @doc Unloads the object
-spec unload(nkservice:id(), id()) ->
    ok | {error, term()}.

unload(SrvId, Id) ->
    unload(SrvId, Id, user_stop).

%% @doc Unloads the object
-spec unload(nkservice:id(), id(), Reason::nkservice:error()) ->
    ok | {error, term()}.

unload(SrvId, Id, Reason) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:async_op(SrvId, Pid, {unload, Reason});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec search(nkservice:id(), search_spec()) ->
    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.

search(SrvId, Spec) ->
    case SrvId:object_db_search(SrvId, Spec) of
        {ok, Total, List, _Aggs, Meta} ->
            {ok, Total, List, Meta};
        {error, Error} ->
            {error, Error}
    end.

%% @doc
-spec search_type(nkservice:id(), type()) ->
    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.

search_type(SrvId, Type) ->
    search(SrvId, #{filters=>#{type=>Type}}).



%% @doc Finds types
-spec search_agg_field(nkdomain:obj_id(), binary(), nkdomain:search_spec(), boolean(), nkelastic:opts()) ->
    {ok, integer(), [{nkdomain:type(), integer()}], map()} | {error, term()}.

search_agg_field(SrvId, Id, Field, Spec, SubChilds) ->
    case SrvId:object_db_search_agg_field(SrvId, Id, Field, Spec, SubChilds) of
        {ok, Total, Data, Meta} ->
            {ok, Total, Data, Meta};
        {error, Error} ->
            {error, Error}
    end.


%%%% @doc Archives an object
%%-spec archive(nkservice:id(), obj_id(), nkservice:error()) ->
%%    ok | {error, term()}.
%%
%%archive(SrvId, ObjId, Reason) ->
%%    case SrvId:object_db_read(SrvId, ObjId) of
%%        {ok, Obj, _Meta} ->
%%            Obj2 = nkdomain_util:add_destroyed(SrvId, Reason, Obj),
%%            case nkdomain_store:archive(SrvId, ObjId, Obj2) of
%%                ok ->
%%                    nkdomain_store:delete(SrvId, ObjId);
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.



%% @doc
delete_all_childs(SrvId, Id) ->
    SrvId:object_db_delete_all_childs(SrvId, Id, #{}).


%% @doc
delete_all_childs_type(SrvId, Id, Type) ->
    Spec = #{filters => #{type=>nklib_util:to_binary(Type)}},
    SrvId:object_db_delete_all_childs(SrvId, Id, Spec).


%% @private Performs a periodic cleanup
-spec clean(nkservice:id()) ->
    {ok, map()} | {error, term()}.

clean(SrvId) ->
    SrvId:object_db_clean(SrvId).


