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

%% @doc Utility to monitor a number of objects
-module(nkdomain_monitor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([new/2, get_obj/2, get_obj_ids/1, get_obj_values/1, get_disabled/1]).
-export([update_obj/3, load_obj/3, add_obj/3, rm_obj/2]).
-export([reload_disabled/1, down_obj/2]).
-export_type([monitor/0]).

-include_lib("nkdomain/include/nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type obj() :: term().

-record(obj_monitor, {
    srv_id :: nkservice:id(),
    regtag :: term(),
    objs = #{} :: #{nkdomain:obj_id() => {obj(), pid()}},
    disabled = #{} :: #{ConvId::nkdomain:obj_id() => nkutil:timestamp()},
    pids = #{} :: #{pid() => {ConvId::nkdomain:obj_id(), reference()}}
}).

-type monitor() :: #obj_monitor{}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Creates a monitor
-spec new(nkservice:id(), term()) ->
    monitor().

new(SrvId, RegTag) ->
    #obj_monitor{srv_id=SrvId, regtag=RegTag}.


%% @doc Get all objects
-spec get_obj(nkdomain:id(), monitor()) ->
    {enabled, obj(), pid()} | {disabled, obj(), nklib_util:timestamp()} | not_found.

get_obj(Id, #obj_monitor{objs=Objs, disabled=Disabled}) ->
    case maps:find(Id, Objs) of
        {ok, {Obj, Pid}} when is_pid(Pid) ->
            {enabled, Obj, Pid};
        {ok, {Obj, undefined}} ->
            Time = maps:get(Id, Disabled),
            {disabled, Obj, Time};
        error ->
            not_found
    end.


%% @doc Get all objects
-spec get_obj_ids(monitor()) ->
    [nkdomain:obj_id()].

get_obj_ids(#obj_monitor{objs=Objs}) ->
    maps:keys(Objs).


%% @doc Get all objects
-spec get_obj_values(monitor()) ->
    [{enabled|disabled, nkdomain:obj()}].

get_obj_values(#obj_monitor{objs=Objs}) ->
    lists:map(
        fun({Obj, Pid}) ->
            {
                case is_pid(Pid) of true -> enabled; false -> disabled end,
                Obj
            }
        end,
        maps:values(Objs)).


%% @doc Updates an object
-spec update_obj(nkdomain:id(), obj(), monitor()) ->
    {ok, monitor()} | not_found.

update_obj(Id, Obj, #obj_monitor{objs=Objs}=Monitor) ->
    case maps:find(Id, Objs) of
        {ok, {_, Pid}} ->
            Objs2 = maps:put(Id, {Obj, Pid}, Objs),
            {ok, Monitor#obj_monitor{objs=Objs2}};
        error ->
            not_found
    end.


%% @doc Get disabled objects
-spec get_disabled(monitor()) ->
    [{nkdomain:obj_id(), nklib_util:timestamp()}].

get_disabled(#obj_monitor{disabled=Disabled}) ->
    maps:to_list(Disabled).


%% @doc Adds a new object, or, if it not found, is added as 'disabled'
-spec load_obj(nkobject:id(), obj(), monitor()) ->
    {enabled, nkdomain:obj_id(), pid(), monitor()} | {disabled, monitor()} | {error, Error}
    when Error :: member_already_present | term().

load_obj(Id, Obj, Monitor) ->
    case add_obj(Id, Obj, Monitor) of
        {ok, ObjId, Pid, Monitor2} ->
            {enabled, ObjId, Pid, Monitor2};
        {error, object_not_found} ->
            {disabled, do_add_disabled(nklib_util:to_binary(Id), Obj, Monitor)};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Adds a new object
-spec add_obj(nkobject:id(), obj(), monitor()) ->
    {ok, nkdomain:obj_id(), pid(), monitor()} |
    {error, Error} when Error :: object_not_found | member_already_present | term().

add_obj(Id, Obj, #obj_monitor{srv_id=SrvId, regtag=RegTag}=Monitor) ->
    case get_obj(Id, Monitor) of
        {enabled, _, _}  ->
            {error, object_already_exists};
        _ ->
            case nkdomain_lib:load(SrvId, Id, #{register=>{RegTag, self()}}) of
                #obj_id_ext{obj_id=ObjId, pid=Pid} ->
                    case do_add_enabled(ObjId, Obj, Pid, Monitor) of
                        {ok, Monitor2} ->
                            {ok, ObjId, Pid, Monitor2};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc Removes a new object
-spec rm_obj(nkobject:id(), monitor()) ->
    {ok, monitor()} | {error, member_not_found|object_not_found|term()}.

rm_obj(Id, #obj_monitor{srv_id=SrvId, objs=Objs}=Monitor) ->
    Id2 = nklib_util:to_binary(Id),
    case maps:find(Id2, Objs) of
        {ok, _} ->
            {ok, do_remove(Id2, Monitor)};
        error ->
            case nkdomain_lib:find(SrvId, Id2) of
                {ok, _Type, ObjId, _Pid} ->
                    case maps:find(ObjId, Objs) of
                        {ok, _} ->
                            {ok, do_remove(ObjId, Monitor)};
                        error ->
                            {error, object_not_found}
                    end;
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc Notifies a process down
-spec down_obj(pid(), monitor()) ->
    {ok, nkdomain:obj_id(), obj(), monitor()} | not_found.

down_obj(Pid, #obj_monitor{pids=Pids, objs=Objs}=Monitor) ->
    case maps:find(Pid, Pids) of
        {ok, {ObjId, _Ref}} ->
            {Obj, Pid} = maps:get(ObjId, Objs),
            {ok, ObjId, Obj, do_add_disabled(ObjId, Obj, Monitor)};
        error ->
            not_found
    end.


%% @doc Tries to reload disabled objects
-spec reload_disabled(monitor()) ->
    {[nkdomain:obj_id()], monitor()}.

reload_disabled(#obj_monitor{disabled=Disabled}=Monitor) ->
    reload_disabled(maps:to_list(Disabled), [], Monitor).


%% @private
reload_disabled([], Acc, Monitor) ->
    {Acc, Monitor};

reload_disabled([{ObjId, _Time}|Rest], Acc, #obj_monitor{objs=Objs}=Monitor) ->
    {Obj, _} = maps:get(ObjId, Objs),
    case add_obj(ObjId, Obj, Monitor) of
        {ok, ObjId, Monitor2} ->
            reload_disabled(Rest, [ObjId|Acc], Monitor2);
        {error, _Error} ->
            reload_disabled(Rest, Acc, Monitor)
    end.


%% ===================================================================
%% Public
%% ===================================================================

%% @private
do_add_enabled(ObjId, Obj, Pid, Monitor) ->
    #obj_monitor{objs=Objs, pids=Pids, disabled=Disabled} = Monitor,
    case maps:find(ObjId, Objs) of
        {ok, {Obj, Pid}} ->
            {error, member_already_present};
        _ ->
            % Not found or disabled or different pid
            Objs2 = Objs#{ObjId => {Obj, Pid}},
            Pids2 = case maps:is_key(Pid, Pids) of
                true ->
                    Pids;
                false ->
                    Ref = monitor(process, Pid),
                    Pids#{Pid => {ObjId, Ref}}
            end,
            Disabled2 = maps:remove(ObjId, Disabled),
            {ok, Monitor#obj_monitor{objs=Objs2, pids=Pids2, disabled=Disabled2}}
    end.


%% @private
do_add_disabled(ObjId, Obj, Monitor) ->
    #obj_monitor{objs=Objs, pids=Pids, disabled=Disabled} = Monitor,
    Objs2 = Objs#{ObjId => {Obj, undefined}},
    Pids2 = case maps:find(ObjId, Objs) of
        {ok, {_, undefined}} ->
            Pids;
        {ok, {_, Pid}} ->
            {ObjId, Ref} = maps:get(Pid, Pids),
            demonitor(Ref, [flush]),
            maps:remove(Pid, Pids);
        error ->
            Pids
    end,
    Disabled2 = case maps:is_key(ObjId, Disabled) of
        true ->
            Disabled;
        false ->
            Disabled#{ObjId => nklib_util:timestamp()}
    end,
    Monitor#obj_monitor{objs=Objs2, pids=Pids2, disabled=Disabled2}.


%% @private
do_remove(ObjId, Monitor) ->
    #obj_monitor{objs=Objs, pids=Pids, disabled=Disabled, regtag=RegTag} = Monitor,
    Pids2 = case maps:find(ObjId, Objs) of
        {ok, {_, undefined}} ->
            Pids;
        {ok, {_, Pid}} ->
            {ObjId, Ref} = maps:get(Pid, Pids),
            demonitor(Ref, [flush]),
            nkdomain_obj:unregister(Pid, {RegTag, self()}),
            maps:remove(Pid, Pids);
        error ->
            Pids
    end,
    Objs2 = maps:remove(ObjId, Objs),
    Disabled2 = maps:remove(ObjId, Disabled),
    Monitor#obj_monitor{objs=Objs2, pids=Pids2, disabled=Disabled2}.
