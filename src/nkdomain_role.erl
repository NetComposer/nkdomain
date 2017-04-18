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

%% @doc Utility functions to manage roles
-module(nkdomain_role).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_roles/1, get_role_objs/2, find_role_objs/2, has_role/3, set_role/3]).
-export([stop/1, get_rolemap/1, resolve_roles/1]).
-export([get_role_op/1]).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Gets an object's roles
-spec get_roles(nkdomain:user_obj_id()) ->
    {ok, [nkrole:role()]} | {error, term()}.

get_roles(Id) ->
    case get_role_op(Id) of
        {ok, ObjUserId, Opts} -> 
            nkrole:get_roles(ObjUserId, Opts);
        {error, Error} -> 
            {error, Error}
    end.


%% @doc Gets an object's direct roles
-spec get_role_objs(nkrole:role(), nkdomain:user_obj_id()) ->
    {ok, [nkrole:role_spec()]} | {error, term()}.

get_role_objs(Role, Id) ->
    Role1 = to_binrole(Role),
    case get_role_op(Id) of
        {ok, ObjUserId, Opts} -> 
            nkrole:get_role_objs(Role1, ObjUserId, Opts);
        {error, Error} -> 
            {error, Error}
    end.


%% @doc Gets an object's roles iterating at all levels
-spec find_role_objs(nkrole:role(), nkdomain:user_obj_id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

find_role_objs(Role, Id) ->
    Role1 = to_binrole(Role),
    case get_role_op(Id) of
        {ok, ObjUserId, Opts} -> 
            nkrole:find_role_objs(Role1, ObjUserId, Opts);
        {error, Error} -> 
            {error, Error}
    end.


%% @doc Check if ObjId has Role over Target
-spec has_role(nkdomain:user_obj_id(), nkrole:role(), nkdomain:obj_id()) ->
    {ok, boolean()} | {error, term()}.

has_role(Id, Role, Target) ->
    Role1 = to_binrole(Role),
    case nkdomain:resolve(Id) of
        {ok, {Class, ObjId, _Pid}} ->
            {ok, ObjUserId1} = nkdomain_util:make_user_id({Class, ObjId}),
            case get_role_op(Target) of
                {ok, TargetObjId, Opts} ->
                    nkrole:has_role(ObjUserId1, Role1, TargetObjId, Opts);
                {error, Error} -> 
                    {error, Error}
            end;
        {error, not_found} ->
            {ok, false};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets an object's roles iterating at all levels
-spec set_role(nkrole:role(), nkdomain:user_obj_id(), [nkrole:role_spec()]) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

set_role(Role, Id, RoleSpecs) ->
    Role1 = to_binrole(Role),
    case get_role_op(Id) of
        {ok, ObjUserId, Opts} -> 
            nkrole:set_role(Role1, ObjUserId, RoleSpecs, Opts);
        {error, Error} -> 
            {error, Error}
    end.


%% @ooc Stops a role proxy
-spec stop(nkdomain:obj_user_id()) ->
    ok | {error, term()}.

stop(Id) ->
    case get_role_op(Id) of
        {ok, ObjUserId, _} -> 
            nkrole:stop(ObjUserId);
        {error, Error} -> 
            {error, Error}
    end.




%% ===================================================================
%% Private
%% ===================================================================


%% @private Gets role's user id and call options
-spec get_role_op(nkdomain:user_obj_id()|{nkdomain:class(), nkdomain:obj_id()}) ->
    {ok, nkdomain:user_obj_id(), nkrole:opts()} | {error, term()}.

get_role_op(Term) ->
    case nkdomain_util:make_user_id(Term) of
        {ok, ObjUserId} ->
            Opts =  #{
                timeout => 180000,
                proxy_timeout => nkdomain_app:get(role_proxy_timeout),
                get_rolemap_fun => fun get_rolemap/1
            },
            {ok, ObjUserId, Opts};
        {error, Error} ->
            {error, Error}
    end.


%% @private
%% Called from nkrole_cache to get roles
get_rolemap(ObjUserId) when is_binary(ObjUserId) ->
    case nkdomain:resolve(ObjUserId) of
        {ok, {Class, ObjId, _Pid}} ->
            nkdomain_orig_obj:do_call(Class, ObjId, get_rolemap, #{});
        {error, Error} ->
            {error, Error}
    end.


%% @private 
%% Called from config to expand all roles to their cannonical form and 
%% expand aliases
-spec resolve_roles(nkrole:rolemap()) ->
    nkrole:rolemap().

resolve_roles(RoleMap) ->
    resolve_roles(maps:to_list(RoleMap), #{}).


%% @private
resolve_roles([], Acc) ->
    Acc;

resolve_roles([{Role, RoleSpec}|Rest], Acc) ->
    RoleSpec1 = nklib_util:filtermap(
        fun do_resolve_role/1,
        RoleSpec),
    resolve_roles(Rest, maps:put(Role, RoleSpec1, Acc)).


%% @private
do_resolve_role(Term) when is_map(Term) ->
    [{SubRole, SubObj}] = maps:to_list(Term),
    case do_resolve_role(SubObj) of
        {true, SubObj1} -> 
            {true, maps:put(SubRole, SubObj1, #{})};
        false -> 
            false
    end;

do_resolve_role(Term) ->
    case nkdomain_util:get_parts(Term) of
        {ok, {alias, AliasId}} ->
            case nkdomain:get_aliases(AliasId) of
                [] ->
                    lager:notice("Could not resolve alias '~s'", [AliasId]),
                    false;
                [UserObjId] ->
                    lager:info("Resolving alias '~s' to '~s'", [AliasId, UserObjId]),
                    {true, UserObjId};
                List ->
                    lager:notice("Could not resolve multiple alias '~s': ~p", 
                                 [AliasId, List]),
                    false
            end;
        {ok, {Class, ObjId}} -> 
            {ok, UserObjId} = nkdomain_util:make_user_id({Class, ObjId}),
            {true, UserObjId};
        {error, Error} ->
            lager:warning("Error loading role ~p: ~p", [Term, Error]),
            false
    end.


%% @private
to_binrole(Role) when is_list(Role) -> list_to_binary(Role);
to_binrole(Role) -> Role.


% update_rolemap(NewRoleMap, OldRoleMap, Replace) ->
%     do_update_rolemap(maps:to_list(NewRoleMap), OldRoleMap, Replace, []).


% %% @private
% do_update_rolemap([], _OldRoleMap, _Rplace, Acc) ->
%     maps:from_list(Acc);

% do_update_rolemap([{Role, RoleSpecs}|Rest], OldRoleMap, Replace, Acc) ->
%     RoleSpecs1 = case Replace of
%         true ->
%             RoleSpecs;
%         false ->
%             OldRoleSpecs = maps:get(Role, OldRoleMap, []),
%             lists:usort(RoleSpecs++OldRoleSpecs)
%     end,
%     do_update_rolemap(Rest, OldRoleMap, Replace, [{Role, RoleSpecs1}|Acc]).


















