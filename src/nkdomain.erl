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

-export([load_file/1, load/2, export/1, get_aliases/1, get_pid/1, get_obj/1, remove/1]).
-export([get_roles/1, get_role_objs/2, find_role_objs/2, has_role/3]).
-export([resolve/1, multi_resolve/1]).
-export([register_update_callback/3]).
-export_type([obj_id/0, name/0, obj/0, path/0, type/0, class/0, history/0, history_op/0]).

%%-include_lib("nklib/include/nklib.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: binary().

-type name() :: binary().

-type path() :: [binary()].

-type type() :: atom().

-type class() :: atom().

-type history_op() :: term().

-type history() :: [{nklib_util:m_timestamp(), User::obj_id(), history_op()}].


%% @see nkdomain_callbacks:domain_store_base_mapping/0
-type obj() :: #{
    obj_id => obj_id(),
    domain => path(),
    type => type(),
    subtype => atom(),
    description => binary(),
    created_by => obj_id(),
    created_time => nklib_util:m_timestamp(),
    parent_id => obj_id(),
    enabled => boolean(),
    expires_time => nklib_util:m_timestamp(),
    destroyed_time => nklib_util:m_timestamp(),
    destroyed_reason => term(),
    icon_id => binary(),
    aliases => [binary()],
    service_id => [nkservice:id()],         % Only for service-related objects
    class() => map()
}.

%%    register => nklib:link(),
%%    events => [nkservice_events:type()],
%%    history => history(),



%% ===================================================================
%% Public
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Loads a domain configuration from a YAML or JSON file
-spec load_file(string()|binary()) ->
    {ok, #{obj_id() => nkdomain_load:load_result()}} | {error, term()}.

load_file(File) ->
    nkdomain_load:load_file(File, #{token=>admin}).


%% @doc Loads a domain configuration from an erlang map string or binary
-spec load(map(), nkdomain_load:load_opts()) ->
    {ok, #{nkdomain:obj_id() => nkdomain_load:load_result()}} | {error, term()}.

load(Data, Opts) ->
    nkdomain_load:load(map, Data, Opts).


%% @doc Exports a full domain specification as a map
-spec export(string()|binary()) ->
    {ok, map()} | {error, term()} .

export(DomainId) ->
    nkdomain_obj2:export(domain, DomainId).


%% @doc Finds all pointing objects for an alias
-spec get_aliases(string()|binary()) ->
    [obj_id()].

get_aliases(Alias) ->
    case nkdomain_obj2:get_obj(alias, Alias) of
        {ok, #{aliases:=Aliases}} -> Aliases;
        {error, _} -> []
    end.


%% @doc Get's de pid for an object
-spec get_pid(string()|binary()) ->
    {ok, pid()} | {error, not_found|term()}.

get_pid(UserObjId) ->
    case resolve(UserObjId) of
        {ok, _Class, _ObjId, Pid} ->
            {ok, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets a full object
-spec get_obj(string()|binary()) ->
    {ok, nkdomain:obj()} | {error, term()}.
 
get_obj(UserObjId) ->
    case resolve(UserObjId) of
        {ok, {Class, ObjId, _Pid}} ->
            case nkdomain_obj2:get_obj(Class, ObjId) of
                {ok, Obj} ->
                    {ok, Class, ObjId, Obj};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Removes an object
%% Most objects can also be removed loading a remove => true field
-spec remove(string()|binary()) ->
    ok | {error, term()}.

remove(UserObjId) ->
    case resolve(UserObjId) of
        {ok, {Class, ObjId, _Pid}} ->
            nkdomain_obj2:remove_obj(Class, ObjId);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets an object's roles
-spec get_roles(string()|binary()) ->
    {ok, [nkrole:role()]} | {error, term()}.

get_roles(UserObjId) ->
    nkdomain_role:get_roles(UserObjId).


%% @doc Gets object direct roles
-spec get_role_objs(nkrole:role(), string()|binary()) ->
    {ok, [nkrole:role_spec()]} | {error, term()}.

get_role_objs(Role, UserObjId) ->
    nkdomain_role:get_role_objs(Role, UserObjId).


%% @doc Gets an object's roles iterating at all levels
-spec find_role_objs(nkrole:role(), string()|binary()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

find_role_objs(Role, UserObjId) ->
    nkdomain_role:find_role_objs(Role, UserObjId).


%% @doc Check if Id has Role over Target
-spec has_role(string()|binary(), nkrole:role(), string()|binary()) ->
    {ok, boolean()} | {error, term()}.

has_role(Id, Role, Target) ->
    nkdomain_role:has_role(Id, Role, Target).


%% @doc Resolves any id or alias
-spec resolve(binary()|string()) ->
    {ok, {class(), obj_id(), pid()}} | {error, term()}.

resolve(UserObjId) ->
    UserObjId1 = nklib_util:to_binary(UserObjId),
    case get_aliases(UserObjId1) of
        [] ->
            nkdomain_util:resolve(UserObjId1);
        [UserObjId2] -> 
            nkdomain_util:resolve(UserObjId2);
        _ ->
            {error, multiple_aliases}
    end.


%% @doc Resolves any id or alias, allowing multiple alias
-spec multi_resolve(binary()|string()) ->
    [{class(), obj_id(), pid()}].

multi_resolve(UserObjId) ->
    UserObjId1 = nklib_util:to_binary(UserObjId),
    case get_aliases(UserObjId1) of
        [] ->
            case nkdomain_util:resolve(UserObjId1) of
                {ok, Data} -> [Data];
                {error, _} -> []
            end;
        UserObjIdList ->
            lists:foldl(
                fun(Id, Acc) ->
                    case nkdomain_util:resolve(Id) of
                        {ok, {Class, ObjId, Pid}} -> 
                            [{Class, ObjId, Pid}|Acc];
                        {error, _} -> 
                            Acc
                    end
                end,
                [],
                UserObjIdList)
    end.



%% @doc Registers a callback that will be called any time and object is updated
%% as Mod:Fun(ObjId, Spec|removed)
-spec register_update_callback(nkdomain:class(), atom(), atom()) ->
    ok.

register_update_callback(Class, Mod, Fun) when is_atom(Class) ->
    code:ensure_loaded(Mod),
    ok = nkdomain_app:put({update, Class}, {Mod, Fun}).



