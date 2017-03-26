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

%% @doc Domain Object

-module(nkdomain_domain_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/4, get_types/2, get_all_types/2, get_childs/3, get_all_childs/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:name(), nkdomain:id(), binary()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, Name, Parent, Desc) ->
    Opts = #{parent=>Parent, name=>Name},
    Base = #{description=>Desc},
    case nkdomain_obj_lib:make_obj(Srv, ?DOMAIN_DOMAIN, Base, Opts) of
        {ok, Obj} ->
            case nkdomain:create(Srv, Obj, #{}) of
                {ok, ?DOMAIN_DOMAIN, ObjId, Path, Pid} ->
                    {ok, ObjId, Path, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_types(SrvId, Id) ->
    case nkdomain:find(SrvId, Id) of
        {ok, _Type, ObjId, _Path, _Pid} ->
            SrvId:object_store_find_types(SrvId, ObjId);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_all_types(SrvId, Id) ->
    case nkdomain:find(SrvId, Id) of
        {ok, _Type, _ObjId, Path, _Pid} ->
            SrvId:object_store_find_all_types(SrvId, Path);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_childs(SrvId, Id, Spec) ->
    case nkdomain:find(SrvId, Id) of
        {ok, _Type, ObjId, _Path, _Pid} ->
            SrvId:object_store_find_childs(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_all_childs(SrvId, Id, Spec) ->
    case nkdomain:find(SrvId, Id) of
        {ok, _Type, _ObjId, Path, _Pid} ->
            SrvId:object_store_find_all_childs(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.









%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_DOMAIN
    }.


%% @private
object_mapping() ->
    #{}.


%% @private
object_syntax(_Mode) ->
    #{}.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_domain_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_domain_obj_api:cmd(Sub, Cmd, Data, State).


