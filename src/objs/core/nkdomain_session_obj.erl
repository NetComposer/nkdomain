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

-export([start/4]).
-export([object_info/0, object_es_mapping/0, object_parse/3,
         object_init/1, object_stop/2, object_event/2]).
-export([object_admin_info/0]).
-export([object_check_active/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_id => binary(),
        api_server_pid => pid(),
        login_meta => map(),
        local => binary(),
        remote => binary()
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Creates a new session
-spec start(nkservice:id(), nkdomain:id(), nkdomain:id(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(SrvId, DomainId, UserId, Opts) ->
        Obj1 = #{
            type => ?DOMAIN_SESSION,
            domain_id => DomainId,
            parent_id => UserId,
            created_by => UserId,
            active => true,
            ?DOMAIN_SESSION => maps:with([local, remote, login_meta], Opts)
        },
        Obj2 = case Opts of
            #{session_id:=SessId} ->
                Obj1#{obj_id => SessId};
            _ ->
                Obj1
        end,
        #{api_server_pid:=ApiPid} = Opts,
        case nkdomain_obj_make:create(SrvId, Obj2, #{meta=>#{api_server_pid=>ApiPid}}) of
            {ok, #obj_id_ext{obj_id=SessId2, pid=Pid}, _} ->
                {ok, SessId2, Pid};
            {error, Error} ->
                {error, Error}
        end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_info() ->
    #{
        type => ?DOMAIN_SESSION,
        stop_after_disabled => true,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 1000,
        tree_id => <<"domain_tree_sessions_sessions">>
    }.


%% @private
object_es_mapping() ->
    #{
        local => #{type => keyword},
        remote => #{type => keyword},
        login_meta => #{enabled => false}
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        local => binary,
        remote => binary,
        login_meta => any
    }.


%% @private
object_init(#?STATE{srv_id=SrvId, id=Id, obj=Obj, meta=#{api_server_pid:=ApiPid}}=State) ->
    %% TODO Link again if moved process
    #obj_id_ext{obj_id=SessId} = Id,
    #{created_by:=UserId} = Obj,
    ok = nkdomain_user_obj:register_session(SrvId, UserId, ?DOMAIN_SESSION, SessId, #{}),
    {ok, nkdomain_obj_util:link_to_api_server(?MODULE, ApiPid, State)};

object_init(State) ->
    ?LLOG(warning, "started without meta", [], State),
    {ok, State}.



%% @private
object_stop(_Reason, #?STATE{meta=#{api_server_pid:=ApiPid}}=State) ->
    % When the session stops, we stop the WS
    nkapi_server:stop(ApiPid, nkdomain_session_stop),
    {ok, State};

object_stop(_Reason, State) ->
    {ok, State}.



%% @private
object_event({enabled, false}, State) ->
    nkdomain:unload(any, self(), session_is_disabled),
    {ok, State};

object_event(_Event, State) ->
    {ok, State}.





%% ===================================================================
%% nkdomain callbacks
%% ===================================================================

%% @private
object_check_active(SrvId, ObjId) ->
    case nkdomain_lib:find_loaded(SrvId, ObjId) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            true;
        not_found ->
            lager:notice("NkDOMAIN: removing stalle active object ~s", [ObjId]),
            SrvId:object_db_delete(SrvId, ObjId),
            false
    end.

