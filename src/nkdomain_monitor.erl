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

-export([new/2, get_obj/2, get_objs/1, get_disabled/1, add_obj/2, rm_obj/2]).
-export([reload_disabled/1, down_obj/2]).
-export_type([monitor/0]).

-include_lib("nkdomain/include/nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-record(obj_monitor, {
    srv_id :: nkservice:id(),
    module :: module(),
    objs = #{} :: #{nkdomain:obj_id() => pid()},
    disabled = #{} :: #{ConvId::nkdomain:obj_id() => nkutil:timestamp()},
    pids = #{} :: #{pid() => {ConvId::nkdomain:obj_id(), reference()}}
}).

-type monitor() :: #obj_monitor{}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Creates a monitor
-spec new(nkservice:id(), atom()) ->
    monitor().

new(SrvId, Module) ->
    #obj_monitor{srv_id=SrvId, module=Module}.


%% @doc Get all objects
-spec get_obj(nkdomain:id(), monitor()) ->
    {enabled, pid()} | {disabled, nklib_util:timestamp()} | not_found.

get_obj(Id, #obj_monitor{objs=Objs, disabled=Disabled}) ->
    case maps:find(Id, Objs) of
        {ok, Pid} when is_pid(Pid) ->
            {enabled, Pid};
        {ok, undefined} ->
            Time = maps:get(Id, Disabled),
            {disabled, Time};
        error ->
            not_found
    end.


%% @doc Get all objects
-spec get_objs(monitor()) ->
    [nkdomain:obj_id()].

get_objs(#obj_monitor{objs=Objs}) ->
    maps:keys(Objs).


%% @doc Get disabled objects
-spec get_disabled(monitor()) ->
    [{nkdomain:obj_id(), nklib_util:timestamp()}].

get_disabled(#obj_monitor{disabled=Disabled}) ->
    maps:to_list(Disabled).


%% @doc Adds a new object
-spec add_obj(nkobject:id(), monitor()) ->
    {enabled, monitor()} | {disabled, monitor()} | {error, term()}.

add_obj(Id, #obj_monitor{srv_id=SrvId, module=Module}=Monitor) ->
    case nkdomain_obj_lib:find(SrvId, Id) of
        #obj_id_ext{obj_id=ObjId, pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:register(Pid, {Module, self()}),
            {enabled, do_add_enabled(ObjId, Pid, Monitor)};
        #obj_id_ext{obj_id=ObjId} ->
            case nkdomain_obj_lib:load(SrvId, ObjId, #{register=>{Module, self()}}) of
                #obj_id_ext{obj_id=ObjId, pid=Pid} ->
                    {enabled, do_add_enabled(ObjId, Pid, Monitor)};
                {error, object_not_found} ->
                    {disabled, do_add_disabled(ObjId, Monitor)};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc Removes a new object
-spec rm_obj(nkobject:id(), monitor()) ->
    {ok, monitor()} | {error, term()}.

rm_obj(Id, #obj_monitor{srv_id=SrvId, objs=Objs}=Monitor) ->
    case maps:find(Id, Objs) of
        {ok, _} ->
            {ok, do_remove(Id, Monitor)};
        error ->
            case nkdomain_obj_lib:find(SrvId, Id) of
                #obj_id_ext{obj_id=ObjId} ->
                    case maps:find(ObjId, Objs) of
                        {ok, _} ->
                            {ok, do_remove(Id, Monitor)};
                        error ->
                            {error, member_not_found}
                    end;
                {error, Error} when Error==object_not_found; Error==path_not_found ->
                    {error, object_not_found};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc Notifies a process down
-spec down_obj(pid(), monitor()) ->
    {ok, nkdomain:obj_id(), monitor()} | not_found.

down_obj(Pid, #obj_monitor{pids=Pids}=Monitor) ->
    case maps:find(Pid, Pids) of
        {ok, {ObjId, _Ref}} ->
            {disabled, ObjId, do_add_disabled(ObjId, Monitor)};
        error ->
            not_found
    end.


%% @doc Tries to reload disabled objects
-spec reload_disabled(monitor()) ->
    monitor().

reload_disabled(#obj_monitor{disabled=Disabled}=Monitor) ->
    lists:foldl(
        fun({ObjId, true}, Acc) ->
            case add_obj(ObjId, Acc) of
                {enabled, Acc2} -> Acc2;
                {disabled, Acc2} -> Acc2;
                {error, _Error} -> Acc
            end
        end,
        Monitor,
        maps:to_list(Disabled)).






%% ===================================================================
%% Public
%% ===================================================================

%% @private
do_add_enabled(ObjId, Pid, Monitor) ->
    #obj_monitor{objs=Objs, pids=Pids, disabled=Disabled} = Monitor,
    Objs2 = Objs#{ObjId => Pid},
    Pids2 = case maps:is_key(Pid, Pids) of
        true ->
            Pids;
        false ->
            Ref = monitor(process, Pid),
            Pids#{Pid => {ObjId, Ref}}
    end,
    Disabled2 = maps:remove(ObjId, Disabled),
    Monitor#obj_monitor{objs=Objs2, pids=Pids2, disabled=Disabled2}.


%% @private
do_add_disabled(ObjId, Monitor) ->
    #obj_monitor{objs=Objs, pids=Pids, disabled=Disabled} = Monitor,
    Objs2 = Objs#{ObjId => undefined},
    Pids2 = case maps:find(ObjId, Objs) of
        {ok, undefined} ->
            Pids;
        {ok, Pid} ->
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
    #obj_monitor{objs=Objs, pids=Pids, disabled=Disabled, module=Module} = Monitor,
    Pids2 = case maps:find(ObjId, Objs) of
        {ok, undefined} ->
            Pids;
        {ok, Pid} ->
            {ObjId, Ref} = maps:get(Pid, Pids),
            demonitor(Ref, [flush]),
            nkdomain_obj:unregister(Pid, {Module, self()}),
            maps:remove(Pid, Pids);
        error ->
            Pids
    end,
    Objs2 = maps:remove(ObjId, Objs),
    Disabled2 = maps:remove(ObjId, Disabled),
    Monitor#obj_monitor{objs=Objs2, pids=Pids2, disabled=Disabled2}.
