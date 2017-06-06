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
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3,
         object_event/2]).
-export([object_admin_info/0]).
-export([object_check_active/2]).

-include("nkdomain.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_id => binary(),
        domain_id => nkdomain:obj_id(),
        api_server_pid => pid(),
        login_meta => map(),
        local => binary(),
        remote => binary()
    }.


%% ===================================================================
%% Public
%% ===================================================================

-spec create(nkservice:id(), nkdomain:obj_id(), create_opts()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(SrvId, UserId, Opts) ->
    Opts2 = Opts#{user_id=>UserId},
    Obj1 = #{
        type => ?DOMAIN_SESSION,
        active => true,
        created_by => UserId,
        parent_id => UserId,
        ?DOMAIN_SESSION => maps:with([local, remote, domain_id, user_id, login_meta], Opts2)
    },
    Obj2 = case Opts of
        #{session_id:=SessId} ->
            Obj1#{obj_id => SessId};
        _ ->
            Obj1
    end,
    case nkdomain_obj_lib:make_and_create(SrvId, <<>>, Obj2, Opts2) of
        {ok, #{obj_id:=SessId2}=Reply, Pid} ->
            % Register to keep the user awake
            ok = nkdomain_obj:link(UserId, usage, Pid, {?MODULE, SessId2}),
            case Opts2 of
                #{api_server_pid:=ApiPid} ->
                    ok = nkdomain_obj:register(Pid, usage, {?MODULE, api_server, ApiPid}),
                    ok = nkapi_server:register(ApiPid, {nkdomain_stop, ?MODULE, Pid});
                _ ->
                    ok
            end,
            {ok, Reply, Pid};
        {error, Error} ->
            {error, Error}
    end.


%%%% @doc
%%-spec get_user(nkdomain:obj_id()|pid()) ->
%%    {ok, #{user_id=>binary(), domain_id=>binary()}}.
%%
%%get_user(SessId) ->
%%    nkdomain_obj:sync_op(SessId, {?MODULE, get_user}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_SESSION,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 1000,
        tree_id => <<"domain_tree_sessions_session">>
    }.




%% @private
object_mapping() ->
    #{
        local => #{type => keyword},
        remote => #{type => keyword},
        domain_id => #{type => keyword},
        user_id => #{type => keyword},
        login_meta => #{enabled => false}
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        local => binary,
        remote => binary,
        domain_id => binary,
        user_id => binary,
        login_meta => any
    }.


%% @private
object_event({enabled, false}, Session) ->
    nkdomain_obj:unload(self(), session_is_disabled),
    {ok, Session};

object_event(_Event, Session) ->
    {ok, Session}.



%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_SESSION, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(Cmd, Req, State) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_SESSION, Req, State).


%%%% @private
%%object_sync_op({?MODULE, get_user}, _From, #?NKOBJ{parent_id=UserId, obj=Obj}=Session) ->
%%    #{?DOMAIN_SESSION:=#{domain_id:=DomainId, login_meta:=Meta}} = Obj,
%%    {reply, {ok, #{user_id=>UserId, domain_id=>DomainId, login_meta=>Meta}}, Session};
%%
%%object_sync_op(_Op, _From, _Session) ->
%%    continue.


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

