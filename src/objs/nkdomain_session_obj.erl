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

%% @doc User Session Object

-module(nkdomain_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_check_active/2]).

-include("nkdomain.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_id => binary(),
        referred_id => binary(),
        pid => pid(),
        local => binary(),
        remote => binary()
    }.


%% ===================================================================
%% Public
%% ===================================================================

-spec create(nkservice:id(), nkdomain:obj_id(), create_opts()) ->
    {ok, SessId::nkdomain:obj_id(), pid()} | {error, term()}.

create(SrvId, Domain, Opts) ->
    Make1 = [
        {active, true},
        case Opts of
            #{session_id:=SessId} -> {obj_id, SessId};
            _ -> []
        end,
        case Opts of
            #{referred_id:=ReferId} -> {referred_id, ReferId};
            _ -> []
        end,
        {type_obj, maps:with([local, remote], Opts)}
    ],
    Make2 = maps:from_list(lists:flatten(Make1)),
    case nkdomain_obj_lib:make_obj(SrvId, Domain, ?DOMAIN_SESSION, Make2) of
        {ok, Obj} ->
            CreateList = [
%%                {remove_after_stop, true},
                case Opts of
                    #{pid:=Pid} -> {register, {?MODULE, Pid}};
                    _ -> []
                end
            ],
            Create = maps:from_list(lists:flatten(CreateList)),
            case nkdomain_obj_lib:create(SrvId, Obj, Create) of
                #obj_id_ext{type = ?DOMAIN_SESSION, obj_id=ObjId, pid=ObjPid} ->
                    {ok, ObjId, ObjPid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_SESSION,
        remove_after_stop => true
    }.


%% @private
object_mapping() ->
    #{
        local => #{type => keyword},
        remote => #{type => keyword}
    }.


%% @private
object_syntax(_Mode) ->
    #{
        local => binary,
        remote => binary
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_session_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_session_obj_api:cmd(Sub, Cmd, Data, State).


%% ===================================================================
%% nkdomain callbacks
%% ===================================================================

%% @private
object_check_active(SrvId, ObjId) ->
    case nkdomain_obj_lib:find_loaded(ObjId) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            true;
        not_found ->
            lager:notice("Removing active object ~s", [ObjId]),
            nkdomain:archive(SrvId, ObjId, object_clean_process),
            false
    end.

