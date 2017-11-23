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


-module(nkdomain_obj_roles).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([get_roles/1, save_roles/2, add_roles/4, remove_roles/4]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @private
get_roles(#obj_state{obj=Obj}) ->
    lists:foldl(
        fun(#{role:=R, direct:=D, indirect:=I}, Acc) ->
            I2 = [{R2, O2} || #{role:=R2, obj_id:=O2} <- I],
            Acc#{R => D++I2}
        end,
        #{},
        maps:get(roles, Obj, [])).


%% @private
save_roles(Roles, #obj_state{obj=Obj} = State) ->
    RoleList = lists:foldl(
        fun
            ({_Role, []}, Acc) ->
                Acc;
            ({Role, RoleSpec}, Acc) ->
                D = [RS || RS <- RoleSpec, is_binary(RS)],
                I = [#{role=>R2, obj_id=>O2} || {R2, O2} <- RoleSpec],
                [#{role=>Role, direct=>D, indirect=>I}|Acc]
        end,
        [],
        maps:to_list(Roles)),
    case maps:get(roles, Obj, []) of
        RoleList ->
            State;
        _ ->
            Obj2 = ?ADD_TO_OBJ(roles, RoleList, Obj),
            State#obj_state{obj=Obj2, is_dirty=true}
    end.


%% @private
add_roles([], _Role, RoleData, State) ->
    {RoleData, State};

add_roles([RoleSpec|Rest], Role, Acc, State) ->
    RoleSpec2 = case RoleSpec of
        {R, O} -> {to_bin(R), to_bin(O)};
        O -> to_bin(O)
    end,
    case lists:member(RoleSpec2, Acc) of
        true ->
            add_roles(Rest, Role, Acc, State);
        false ->
            Acc2 = [RoleSpec2|Acc],
            State2 = do_event({added_role, Role, RoleSpec2}, State),
            add_roles(Rest, Role, Acc2, State2)
    end.


%% @private
remove_roles([], _Role, RoleData, State) ->
    {RoleData, State};

remove_roles([RoleSpec|Rest], Role, Acc, State) ->
    RoleSpec2 = case RoleSpec of
        {R, O} -> {to_bin(R), to_bin(O)};
        O -> to_bin(O)
    end,
    case lists:member(RoleSpec2, Acc) of
        false ->
            remove_roles(Rest, Role, Acc, State);
        true ->
            Acc2 = Acc -- [RoleSpec2],
            State2 = do_event({removed_role, Role, RoleSpec2}, State),
            remove_roles(Rest, Role, Acc2, State2)
    end.


%% @private
do_event(Event, State) ->
    ?DEBUG("sending 'event': ~p", [Event], State),
    nkdomain_obj_util:event(Event, State).



%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).


