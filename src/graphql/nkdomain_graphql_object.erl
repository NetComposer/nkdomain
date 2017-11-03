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
-module(nkdomain_graphql_object).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([execute/4]).

%% Assume we are given a map(). Look up the field in the map. If not
%% present, return the value null.
execute(Ctx, #{type:=Type}=Obj, Field, Args) ->
    case common_field(Field, Obj) of
        {ok, Res} ->
            {ok, Res};
        unknown ->
            case nkdomain_lib:get_module(Type) of
                undefined ->
                    {error, unknown_type};
                Module ->
                    Module:execute(Ctx, Obj, Field, Args)
            end
    end.


%% @%% @private GraphQL execute
common_field(Field, Obj) ->
    case Field of
        <<"id">> -> {ok, maps:get(obj_id, Obj)};
        <<"vsn">> -> {ok, maps:get(vsn, Obj, null)};
        <<"type">> -> {ok, maps:get(type, Obj)};
        <<"path">> -> {ok, maps:get(path, Obj)};
        <<"objName">> -> {ok, maps:get(obj_name, Obj)};
        <<"domainId">> -> {ok, maps:get(domain_id, Obj)};
        <<"domain">> -> get_obj(maps:get(domain_id, Obj));
        <<"parentId">> -> {ok, maps:get(parent_id, Obj)};
        <<"parent">> -> get_obj(maps:get(parent_id, Obj));
        <<"srvId">> -> {ok, maps:get(srv_id, Obj, null)};
        <<"subtype">> -> {ok, maps:get(subtype, Obj, [])};
        <<"createdBy">> -> get_obj(maps:get(created_by, Obj));
        <<"createdTime">> -> {ok, maps:get(created_time, Obj, null)};
        <<"updatedBy">> -> get_obj(maps:get(updated_by, Obj, null));
        <<"updatedTime">> -> {ok, maps:get(updated_time, Obj, null)};
        <<"enabled">> -> {ok, maps:get(enabled, Obj, true)};
        <<"active">> -> {ok, maps:get(active, Obj, null)};
        <<"expiresTime">> -> {ok, maps:get(expires_time, Obj, null)};
        <<"destroyed">> -> {ok, maps:get(destroyed, Obj, false)};
        <<"destroyedTime">> -> {ok, maps:get(destroyed_time, Obj, null)};
        <<"destroyedCode">> -> {ok, maps:get(destroyed_code, Obj, null)};
        <<"destroyedReason">> -> {ok, maps:get(destroyed_reason, Obj, null)};
        <<"name">> -> {ok, maps:get(name, Obj, null)};
        <<"description">> -> {ok, maps:get(description, Obj, null)};
        <<"tags">> -> {ok, maps:get(tags, Obj, [])};
        <<"aliases">> -> {ok, maps:get(aliases, Obj, [])};
        <<"iconId">> -> get_obj(maps:get(icon_id, Obj, null));
        <<"nextStatusTime">> -> {ok, maps:get(next_status_time, Obj, null)};
        _ -> unknown
    end.


get_obj(null) ->
    {ok, null};
get_obj(DomainId) ->
    nkdomain:get_obj(DomainId).