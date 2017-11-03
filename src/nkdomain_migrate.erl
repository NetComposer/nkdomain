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
-module(nkdomain_migrate).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').


-export([import_7_to_8/1, print/1]).
-export([import_8_to_9/1]).
-include("nkdomain.hrl").


%% ===================================================================
%% 7 to 8
%% ===================================================================


%% @doc
import_7_to_8(Path) ->
    Fun = fun(Obj) ->
        case Obj of
            #{?DOMAIN_USER := User} ->
                User2 = import_7_to_8_user(User),
                {upgrade, Obj#{?DOMAIN_USER:=User2}};
            #{<<"conversation">> := #{<<"push_app_id">>:=AppId}=Conv} ->
                Conv2 = maps:remove(<<"push_app_id">>, Conv),
                {upgrade, Obj#{<<"conversation">>:=Conv2#{<<"push_srv_id">>=>AppId}}};
            _ ->
                {upgrade, Obj}
        end
    end,
    nkdomain_store_search:import_objects(<<"nkobjects_v7">>, Path, Fun).


%% @private
import_7_to_8_user(User) ->
    Push = lists:map(
        fun(P) ->
            case maps:take(<<"app_id">>, P) of
                {<<"sphera_collab">>, P2} ->
                    P2#{<<"srv_id">> => <<"sphera_telemed">>};
                {Key, P2} ->
                    P2#{<<"srv_id">> => Key};
                error ->
                    P
            end
        end,
        maps:get(<<"push">>, User, [])),
    Status = lists:map(
        fun(S) ->
            case maps:take(<<"app_id">>, S) of
                {<<"sphera_collab">>, S2} ->
                    S2#{<<"srv_id">> => <<"sphera_telemed">>};
                {Key, S2} ->
                    S2#{<<"srv_id">> => Key};
                error ->
                    S
            end
        end,
        maps:get(<<"status">>, User, [])),
    User#{<<"push">>=>Push, <<"status">>:=Status}.


import_8_to_9(Path) ->
    Fun = fun(Obj) ->
        case Obj of
            #{<<"path">>:=<<"/sipstorm_c4">>} ->
                {upgrade, Obj};
            #{<<"path">>:=<<"/sphera">>} ->
                {upgrade, Obj};
            _ ->
                Obj2 = maps:remove(<<"srv_id">>, Obj),
                {upgrade, Obj2}
        end
    end,
    nkdomain_store_search:import_objects(<<"nkobjects_v8">>, Path, Fun).









%% ===================================================================
%% Util
%% ===================================================================


%% @private
print(Obj) ->
    io:format("\n~s\n", [nklib_json:encode_pretty(Obj)]).