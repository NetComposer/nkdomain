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
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_event/2]).
-export([object_admin_tree/3]).
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
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(SrvId, UserId, Opts) ->
    Obj1 = #{
        type => ?DOMAIN_SESSION,
        active => true,
        created_by => UserId,
        parent_id => UserId,
        ?DOMAIN_SESSION => maps:with([local, remote], Opts)
    },
    Obj2 = case Opts of
        #{session_id:=SessId} ->
            Obj1#{obj_id => SessId};
        _ ->
            Obj1
    end,
    Opts2 = case Opts of
        #{api_server_pid:=Pid1} ->
            #{usage_link => {Pid1, nkdomain_session_api}};
            _ ->
                #{}
    end,
    case nkdomain_obj_lib:make_and_create(SrvId, <<>>, Obj2, Opts2) of
        {ok, Reply, Pid} ->
            case Opts of
                #{api_server_pid:=Pid2} ->
                    % Register to keep the user awake
                    ok = nkdomain_obj:link(UserId, usage, Pid, ?MODULE),
                    %% TODO if the session is moved, it will fail
                    ok = nkapi_server:register(Pid2, {?MODULE, Pid});
                _ ->
                    ok
            end,
            {ok, Reply, Pid};
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
object_parse(_SrvId, _Mode, _Obj) ->
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


%% @doc
%% @doc
object_admin_tree(sessions, List, #{types:=Types}=State) ->
    Num = maps:get(?DOMAIN_SESSION, Types),
    Item = nkadmin_util:menu_item(domain_tree_sessions_login, {menuBadge, Num}, State),
    {ok, [{Item, 500}|List]};

object_admin_tree(_Category, _Data, _State) ->
    ok.



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

