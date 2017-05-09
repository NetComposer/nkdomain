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
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_event/2]).
-export([object_check_active/2]).

-include("nkdomain.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_id => binary(),
        api_server_pid => pid(),
        local => binary(),
        remote => binary()
    }.


%% ===================================================================
%% Public
%% ===================================================================

-spec create(nkservice:id(), nkdomain:obj_id(), create_opts()) ->
    {ok, SessId::nkdomain:obj_id(), pid()} | {error, term()}.

create(SrvId, UserId, Opts) ->
    Make1 = [
        {active, true},
        {created_by, UserId},
        case Opts of
            #{session_id:=SessId} -> {obj_id, SessId};
            _ -> []
        end,
        {type_obj, maps:with([local, remote], Opts)},
        case Opts of
            #{api_server_pid:=Pid1} -> {usage_link, {Pid1, nkdomain_session_api}};
            _ -> []
        end
    ],
    Make2 = maps:from_list(lists:flatten(Make1)),
    case nkdomain_obj_lib:make_and_create(SrvId, UserId, ?DOMAIN_SESSION, Make2) of
        {ok, ObjId, _Path, ObjPid} ->
            case Opts of
                #{api_server_pid:=Pid2} ->
                    % Register to keep the user awake
                    ok = nkdomain_obj:link(UserId, usage, ObjPid, ?MODULE),
                    %% TODO if the session is moved, it will fail
                    ok = nkapi_server:register(Pid2, {?MODULE, ObjPid});
                _ ->
                    ok
            end,
            {ok, ObjId, ObjPid};
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
object_event({enabled, false}, Session) ->
    nkdomain_obj:unload(self(), session_is_disabled),
    {ok, Session};

object_event(_Event, Session) ->
    {ok, Session}.



%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Sub, Cmd, ?DOMAIN_SESSION, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Req, State) ->
    nkdomain_obj_api:api(Sub, Cmd, Req, ?DOMAIN_SESSION, State).




%% ===================================================================
%% nkdomain callbacks
%% ===================================================================

%% @private
object_check_active(SrvId, ObjId) ->
    case nkdomain_obj_lib:find_loaded(ObjId) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            true;
        not_found ->
            lager:notice("NkDOMAIN: removing stalle active object ~s", [ObjId]),
            nkdomain:archive(SrvId, ObjId, object_clean_process),
            false
    end.

