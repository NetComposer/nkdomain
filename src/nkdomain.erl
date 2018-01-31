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

%% Roles
%%
%% - User roles will have the format Srv:Role (like <<"company:role">>)
%% - For personalities, the user object will have a role like "company:doctor" with the obj_id of doctor object,
%%   and the doctor object will have a role "user" with the obj_id of the doctor
%%
%% Standard roles
%%
%% - admin: all capabilities over object
%% - member: membership, whatever it means for any object
%% - operator: can access the object, unload, delete, etc.
%% - user: this object "belongs" (it is a personality) of this user

-export([find/1, load/1, unload/1, unload/2, get_obj/1, get_info/1, get_name/1, get_domain_id/1]).
-export([enable/2, update/2, update_name/2, delete/1, send_info/3]).
-export([get_roles/1, add_roles/3, remove_roles/3]).
-export([get_types/1]).
-export([get_paths/1, get_paths_type/2, get_paths_type/3, remove_path/1, remove_path_type/2, print_path_type/2]).
-export([get_childs/1, get_childs_type/2, remove_with_childs/1]).
-export([clean/0]).
-export([print_fun/0, delete_fun/0, recursive_delete_fun/0]).
-export_type([obj_id/0, obj_name/0, obj/0, path/0, id/0, type/0, role/0, role_spec/0]).
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

-type role() :: binary().

-type role_spec() ::
    obj_id() |
    {Role::role(), ObjId::obj_id()}.   % Objects having 'Role' over ObjId


%% @see nkdomain_callbacks:domain_store_base_mapping/0
-type obj() :: map().


-type timestamp() :: nklib_util:m_timestamp().



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(id()) ->
    {ok, type(), obj_id(), path(), pid()|undefined} | {error, object_not_found|term()}.

find(Id) ->
    case nkdomain_db:find(Id) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(id()) ->
    {ok, type(), obj_id(), path(), pid()} |  {error, object_not_found|term()}.

load(Id) ->
    case nkdomain_db:load(Id) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
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
    case nkdomain_db:find(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:async_op(Pid, {unload, Reason});
        #obj_id_ext{} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_roles(id()) ->
    {ok, #{role() => {[obj_id()], [{role(), obj_id()}]}}} | {error, term()}.

get_roles(Id) ->
    nkdomain_obj:sync_op(Id, get_roles).


%% @doc
-spec add_roles(id(), role(), role_spec() | [role_spec()]) ->
    ok | {error, term()}.

add_roles(Id, Role, RoleSpec) ->
    nkdomain_obj:sync_op(Id, {add_roles, Role, RoleSpec}).


%% @doc
-spec remove_roles(id(), role(), role_spec() | [role_spec()]) ->
    ok | {error, term()}.

remove_roles(Id, Role, RoleSpec) ->
    nkdomain_obj:sync_op(Id, {remove_roles, Role, RoleSpec}).


%%%% @doc
%%-spec search(search_spec()) ->
%%    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.
%%
%%search(Spec) ->
%%    case ?CALL_NKROOT(object_db_search, [Spec]) of
%%        {ok, Total, List, _Aggs, Meta} ->
%%            {ok, Total, List, Meta};
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @doc
%%-spec search(id(), search_spec()) ->
%%    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.
%%
%%search(Domain, Spec) ->
%%    case nkdomain_db:find(Domain) of
%%        #obj_id_ext{path=Path} ->
%%            Filters1 = maps:get(filters, Spec, #{}),
%%            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
%%            search(Spec#{filters=>Filters2});
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%%% @doc
%%-spec search(id(), nkdomain:type(), search_spec()) ->
%%    {ok, integer(), Data::[map()], Meta::map()} | {error, term()}.
%%
%%search(Domain, Type, Spec) ->
%%    Filters1 = maps:get(filters, Spec, #{}),
%%    Filters2 = Filters1#{type=>Type},
%%    search(Domain, Spec#{filters=>Filters2}).


%%%% @doc Finds types
%%-spec search_agg_field(nkdomain:obj_id(), binary(), nkdomain:search_spec(), boolean()) ->
%%    {ok, integer(), [{nkdomain:type(), integer()}], map()} | {error, term()}.
%%
%%search_agg_field(Id, Field, Spec, SubChilds) ->
%%    case ?CALL_NKROOT(object_db_search_agg_field, [Id, Field, Spec, SubChilds]) of
%%        {ok, Total, Data, Meta} ->
%%            {ok, Total, Data, Meta};
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc
-spec get_types(id()) ->
    {ok, integer(), [{binary(), integer()}]} | {error, term()}.

get_types(Domain) ->
    nkdomain_db:aggs(core, {query_types, Domain, #{deep=>true}}).


%% @doc Gets objects under a path, sorted by path
-spec get_paths(id()) ->
    {ok, integer(), [#{binary() => term()}]} | {error, term()}.

get_paths(Id) ->
    case nkdomain_db:search(core, {query_paths, Id, #{deep=>true, sort=>path, size=>100}}) of
        {ok, Total, Data, _Meta} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets objects under a path with a type, sorted by path
-spec get_paths_type(id(), type()) ->
    {ok, integer(), [#{binary() => term()}]} | {error, term()}.

get_paths_type(Id, Type) ->
    case nkdomain_db:search(core, {query_paths, Id, #{deep=>true, type=>Type, sort=>path, size=>100}}) of
        {ok, Total, Data, _Meta} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets objects under a path with a type, sorted by path
-spec get_paths_type(id(), type(), term()) ->
    {ok, integer(), [#{binary() => term()}]} | {error, term()}.

get_paths_type(Id, Type, Session) ->
    %case nkdomain_db:search(core, {query_paths, Id, #{deep=>true, type=>Type, sort=>path, size=>100}}) of
    case nkdomain_admin_util:db_search(core, {query_paths, Id, #{deep=>true, type=>Type, sort=>path, size=>100}}, Session) of
        {ok, Total, Data, _Meta} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec remove_path(id()) ->
    {ok, integer()} | {error, term()}.

remove_path(Id) ->
    case nkdomain_db:iterate(core, {query_paths, Id, #{deep=>true, sort=>rpath, get_deleted=>true}}, delete_fun(), 0) of
        {ok, Count} ->
            case nkdomain_db:hard_delete(Id) of
                ok ->
                    {ok, Count+1};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec remove_path_type(id(), type()) ->
    {ok, integer()} | {error, term()}.

remove_path_type(Id, Type) ->
    nkdomain_db:iterate(core, {query_paths, Id, #{type=>Type, deep=>true, sort=>rpath, get_deleted=>true}}, delete_fun(), 0).


%% @doc
print_path_type(Id, Type) ->
    nkdomain_db:iterate(core, {query_paths, Id, #{type=>Type, deep=>true, sort=>path, get_deleted=>true}}, print_fun(), 0).


%% @doc Gets objects having a parent, sorted by path
-spec get_childs(id()) ->
    {ok, integer(), [#{binary() => term()}]} | {error, term()}.

get_childs(Id) ->
    case nkdomain_db:search(core, {query_childs, Id, #{sort=>path}}) of
        {ok, Total, Data, _Meta} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets objects having a parent with a type, sorted by path
-spec get_childs_type(id(), type()) ->
    {ok, integer(), [#{binary() => term()}]} | {error, term()}.

get_childs_type(Id, Type) ->
    case nkdomain_db:search(core, {query_childs, Id, #{type=>Type, sort=>path}}) of
        {ok, Total, Data, _Meta} ->
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
remove_with_childs(Id) ->
    case nkdomain_db:find(Id) of
        #obj_id_ext{obj_id=ObjId} ->
            case nkdomain_db:iterate(core, {query_childs, ObjId, #{sort=>rpath, get_deleted=>true}}, recursive_delete_fun(), 0) of
                {ok, Count} ->
                    case nkdomain_db:hard_delete(ObjId) of
                        ok ->
                            {ok, Count+1};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private Performs a periodic cleanup
-spec clean() ->
    {ok, map()} | {error, term()}.

clean() ->
    ?CALL_NKROOT(object_db_clean, []).



%% ===================================================================
%% Internal
%% ===================================================================

print_fun() ->
    fun(#{<<"obj_id">>:=ObjId, <<"path">>:=Path}, Acc) ->
        lager:info("Object ~s (~s)", [Path, ObjId]),
        {ok, Acc+1}
    end.


delete_fun() ->
    fun(#{<<"obj_id">>:=ObjId, <<"path">>:=Path}, Acc) ->
        case nkdomain_db:hard_delete(ObjId) of
            ok ->
                lager:info("Deleted object ~s (~s)", [Path, ObjId]),
                {ok, Acc+1};
            {error, Error} ->
                lager:info("Could node deleted object ~s (~s): ~p", [Path, ObjId, Error]),
                {ok, Acc}
        end
    end.

recursive_delete_fun() ->
    fun(#{<<"obj_id">>:=ObjId, <<"path">>:=Path}, Acc) ->
        case nkdomain:remove_with_childs(ObjId) of
            {ok, NAcc} ->
                lager:info("Deleted object ~s (~s)", [Path, ObjId]),
                {ok, Acc+NAcc};
            {error, Error} ->
                lager:info("Could node deleted object ~s (~s): ~p", [Path, ObjId, Error]),
                {ok, Acc}
        end
    end.
