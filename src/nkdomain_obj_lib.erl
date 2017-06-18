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



-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%% ===================================================================
%% Public
%% ===================================================================



%% ===================================================================
%% Private
%% ===================================================================

%%%% @private
%%reserve(ObjId) ->
%%    reserve(ObjId, 5).
%%
%%
%%%% @private
%%reserve(_ObjId, 0) ->
%%    {error, could_not_reserve};
%%
%%reserve(ObjId, Tries) ->
%%    case nkdist_reg:reserve(nkdomain, ObjId) of
%%        ok ->
%%            ok;
%%        {error, _} ->
%%            lager:info("NkDOMAIN: Could not reserve ~s, retrying", [ObjId]),
%%            timer:sleep(1000),
%%            reserve(ObjId, Tries-1)
%%    end.
%%
%%
%%%% @private
%%unreserve(ObjId) ->
%%    lager:error("UNReserved ~p (~p)", [ObjId, self()]),
%%    nkdist_reg:unreserve(nkdomain, ObjId).


%%%% @private
%%register(Type, ObjId, Path) ->
%%    do_register(Type, ObjId, Path, #{sync=>true}).
%%
%%
%%%% @private
%%register(Type, ObjId, Path, Pid) ->
%%    do_register(Type, ObjId, Path, #{sync=>true, replace_pid=>Pid}).
%%
%%
%%%% @private
%%do_register(Type, ObjId, Path, Opts) ->
%%    case nkdist_reg:register(proc, nkdomain, ObjId, Opts#{meta=>{Type, path, Path}}) of
%%        ok ->
%%            nkdist_reg:register(reg, nkdomain, Path, Opts#{meta=>{Type, obj_id, ObjId}});
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%%% @private Links a child to its parent
%%link_to_parent(Parent, Type, Name, ChildId) ->
%%    % Parent will receive {received_link, {nkdomain_child, ChildId}} (does nothing)
%%    % If parent dies, child receives {sent_link_down, {nkdomain_child, ChildId}}
%%    % Id child dies, parent receives {received_link_down, {nkdomain_child, ChildId}}
%%    ok = nkdist_reg:link(nkdomain, Parent, {nkdomain_child, Type, Name, ChildId}).
%%
%%
%%%% @private Links a child to its parent
%%%% Destination will receive {received_link, {Type, Tag}}
%%%% On failure, orig receive {sent_link_down, {Type, Tag}},
%%%% destination receives {received_link_down, {Type, Tag}}
%%
%%link_to_obj(Type, OrigPid, DestPid, Tag) when is_pid(OrigPid) ->
%%    nkdist_reg:link_pid(OrigPid, DestPid, {Type, Tag});
%%
%%link_to_obj(Type, OrigId, DestPid, Tag) ->
%%    case nkdomain_lib:find_loaded(Srv, OrigId) of
%%        #obj_id_ext{pid=OrigPid} ->
%%            link_to_obj(Type, OrigPid, DestPid, Tag);
%%        not_found ->
%%            {error, destination_not_found}
%%    end.
%%
%%
%%%% @private Removes a previous link
%%%% Destination will receive {removed_link, {Type, Tag}}
%%
%%unlink_to_obj(Type, OrigPid, DestPid, Tag) when is_pid(OrigPid) ->
%%    nkdist_reg:unlink_pid(OrigPid, DestPid, {Type, Tag});
%%
%%unlink_to_obj(Type, OrigId, DestPid, Tag) ->
%%    case nkdomain_lib:find_loaded(Srv, OrigId) of
%%        #obj_id_ext{pid=OrigPid} ->
%%            unlink_to_obj(Type, OrigPid, DestPid, Tag);
%%        not_found ->
%%            {error, destination_not_found}
%%    end.




%%%% @private
%%to_bin(T) -> nklib_util:to_binary(T).