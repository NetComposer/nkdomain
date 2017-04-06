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


-export([find_loaded/1, find/1, find/2, load/1, load/2, load/3, create/3]).
-export_type([obj_id/0, name/0, obj/0, path/0, type/0, id/0, class/0, history/0, history_op/0]).

%%-include_lib("nklib/include/nklib.hrl").

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: binary().

-type name() :: binary().

-type path() :: [binary()].

-type id() :: obj_id() | path().

-type type() :: binary().

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


-type create_opts() ::
    #{
        obj_id => obj_id(),
        register => nklib:link(),
        %%  events => [nkservice_events:type()],
        enabled => boolean()                        % Start disabled
    }.

-type load_opts() ::
    #{
        register => nklib:link()
    }.


%% ===================================================================
%% Public
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds and loaded object from UUID or Path
-spec find_loaded(id()) ->
    {ok, type(), domain:obj_id(), path(), pid()} |
    {error, object_not_loaded|term()}.

find_loaded(IdOrPath) ->
    case nkdomain_obj_lib:find_loaded(IdOrPath) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        not_found ->
            {error, object_not_loaded}
    end.


%% @doc
find(IdOrPath) ->
    find(root, IdOrPath).


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), obj_id()|path()) ->
    {ok, type(), domain:obj_id(), path(), pid()|undefined} |
    {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkdomain_obj_lib:find(Srv, IdOrPath) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
load(IdOrPath) ->
    load(root, IdOrPath, #{}).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), obj_id()|path()) ->
    {ok, type(), obj_id(), path(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, IdOrPath) ->
    load(Srv, IdOrPath, #{}).


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), obj_id()|path(), load_opts()) ->
    {ok, type(), obj_id(), path(), pid()} |
    {error, obj_not_found|term()}.

load(Srv, IdOrPath, Meta) ->
    case nkdomain_obj_lib:load(Srv, IdOrPath, Meta) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates a new object
-spec create(nkservice:id(), map(), create_opts()) ->
    {ok, type(), obj_id(), path(), pid()}.

create(Srv, Obj, Meta) ->
    case nkdomain_obj_lib:create(Srv, Obj, Meta) of
        #obj_id_ext{type=Type, obj_id=ObjId, path=Path, pid=Pid} ->
            {ok, Type, ObjId, Path, Pid};
        {error, Error} ->
            {error, Error}
    end.

