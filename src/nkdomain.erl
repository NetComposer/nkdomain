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


-export([find/1, load/1, unload/1, unload/2, get_obj/1, get_info/1, get_name/1, get_domain_id/1]).
-export([enable/2, update/2, update_name/2, delete/1, send_info/3]).
-export([search/1, search/2, search/3, delete_all_childs/1, delete_all_childs_type/2, search_agg_field/4]).
-export([clean/0, upgrade/0]).
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


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(id()) ->
    {ok,  type(), obj_id(), path(), pid()|undefined} | {error, object_not_found|term()}.

find(Id) ->
    case nkdomain_lib:find(Id) of
        #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, SrvId, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(id()) ->
    {ok,  type(), obj_id(), path(), pid()} |  {error, object_not_found|term()}.

load(Id) ->
    case nkdomain_lib:load(Id) of
        #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, SrvId, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_obj(id()|pid()) ->
    {ok, obj()} | {error, term()}.

get_obj(Id) ->
    nkdomain_obj:sync_op(Id, get_obj).


%% @doc
-spec get_info(id()|pid()) ->
    {ok, map()} | {error, term()}.

get_info(Id) ->
    nkdomain_obj:sync_op(Id, get_obj_info).


%% @doc
-spec get_name(id()|pid()) ->
    {ok, map()} | {error, term()}.

get_name(Id) ->
    nkdomain_obj:sync_op(Id, get_obj_name).


%% @doc
-spec get_domain_id(id()|pid()) ->
    {ok, nkdomain:obj_id()} | {error, term()}.

get_domain_id(Id) ->
    nkdomain_obj:sync_op(Id, get_domain_id).


%% @doc Enables/disabled an object
-spec enable(id()|pid(), boolean()) ->
    ok | {error, term()}.

enable(Id, Enable) ->
    nkdomain_obj:sync_op(Id, {enable, Enable}).


%% @doc Updates an object
-spec update(id()|pid(), map()) ->
    {ok, UnknownFields::[binary()]} | {error, term()}.

update(Id, Update) ->
    nkdomain_obj:sync_op(Id, {update, Update}).


%% @doc Updates an object's obj_name
-spec update_name(id()|pid(), binary()) ->
    ok | {error, term()}.

update_name(Id, ObjName) ->
    nkdomain_obj:sync_op(Id, {update_name, ObjName}).


%% @doc Remove an object
-spec delete(id()|pid()) ->
    ok | {error, term()}.

delete(Id) ->
    nkdomain_obj:sync_op(Id, delete).


%% @doc Sends an INFO
-spec send_info(id()|pid(), Info::atom()|binary(), Body::map()) ->
    ok | {error, term()}.

send_info(Id, Info, Body) when is_map(Body) ->
    nkdomain_obj:async_op(Id, {send_info, Info, Body}).


%% @doc Unloads the object
-spec unload(id()|pid()) ->
    ok | {error, term()}.

unload(Id) ->
    unload(Id, normal).

%% @doc Unloads the object
-spec unload(id()|pid(), Reason::nkservice:error()) ->
    ok | {error, term()}.

unload(Id, Reason) ->
    case nkdomain_lib:find(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:async_op(Pid, {unload, Reason});
        #obj_id_ext{} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec search(search_spec()) ->
    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.

search(Spec) ->
    case ?CALL_NKROOT(object_db_search, [Spec]) of
        {ok, Total, List, _Aggs, Meta} ->
            {ok, Total, List, Meta};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec search(nkdomain:id(), search_spec()) ->
    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.

search(Domain, Spec) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{path=Path} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
            search(Spec#{filters=>Filters2});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec search(nkdomain:id(), nkdomain:type(), search_spec()) ->
    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.

search(Domain, Type, Spec) ->
    Filters1 = maps:get(filters, Spec, #{}),
    Filters2 = Filters1#{type=>Type},
    search(Domain, Spec#{filters=>Filters2}).


%% @doc Finds types
-spec search_agg_field(nkdomain:obj_id(), binary(), nkdomain:search_spec(), boolean()) ->
    {ok, integer(), [{nkdomain:type(), integer()}], map()} | {error, term()}.

search_agg_field(Id, Field, Spec, SubChilds) ->
    case ?CALL_NKROOT(object_db_search_agg_field, [Id, Field, Spec, SubChilds]) of
        {ok, Total, Data, Meta} ->
            {ok, Total, Data, Meta};
        {error, Error} ->
            {error, Error}
    end.


%%%% @doc Archives an object
%%-spec archive( obj_id(), nkservice:error()) ->
%%    ok | {error, term()}.
%%
%%archive(ObjId, Reason) ->
%%    case SrvId:object_db_read(ObjId) of
%%        {ok, Obj, _Meta} ->
%%            Obj2 = nkdomain_util:add_destroyed(Reason, Obj),
%%            case nkdomain_store:archive(ObjId, Obj2) of
%%                ok ->
%%                    nkdomain_store:delete(ObjId);
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.



%% @doc
delete_all_childs(Id) ->
    ?CALL_NKROOT(object_db_delete_all_childs, [Id, #{}]).


%% @doc
delete_all_childs_type(Id, Type) ->
    Spec = #{filters => #{type=>nklib_util:to_binary(Type)}},
    ?CALL_NKROOT(object_db_delete_all_childs, [Id, Spec]).


%% @private Performs a periodic cleanup
-spec clean() ->
    {ok, map()} | {error, term()}.

clean() ->
    ?CALL_NKROOT(object_db_clean, []).


upgrade() ->
    {ok, E} = nkdomain_store_es_util:get_opts(),
    Fun = fun(Obj, Acc) ->
        lager:notice("Obj: ~p", [Obj]),
        Acc+1
    end,
    nkdomain_store_es:iterate_objects(E, Fun, 0).

