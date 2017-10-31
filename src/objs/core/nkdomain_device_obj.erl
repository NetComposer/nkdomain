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

%% @doc Device Object

-module(nkdomain_device_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-export([create/2, attach_session/3, get_users/1, find_sso/1]).
-export([object_info/0, object_admin_info/0, object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export([object_es_mapping/0, object_parse/2]).
-export([object_init/1, object_sync_op/3, object_link_down/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Device "++Txt, Args)).



%% ===================================================================
%% Types
%% ===================================================================



-type create_opts() ::
#{
    obj_id => binary(),
    created_by => nkdomain:obj_id(),
    sso_device_ids => [binary()]
}.



%% ===================================================================
%% Public
%% ===================================================================

-spec create(nkdomain:id(), create_opts()) ->
    {ok, DeviceIdId::nkdomain:obj_id(), pid()} | {error, term()}.

create(Domain, Opts) ->
    DeviceObj = maps:with([sso_device_ids], Opts),
    Obj = #{
        type => ?DOMAIN_DEVICE,
        domain_id => Domain,
        created_by => maps:get(created_by, Opts, <<"admin">>),
        ?DOMAIN_DEVICE => DeviceObj
    },
    case nkdomain_obj_make:create(Obj) of
        {ok, #obj_id_ext{obj_id=DeviceId, pid=MsgPid}, []} ->
            {ok, DeviceId, MsgPid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Attaches an user and session to the device
%% The device will be loaded as long as the session is
%% You can use get_users/1 to get all registered users
%% You can use find_sso/1 to find if any of my sso_device_ids have any registered user currently.
-spec attach_session(nkdomain:id(), nkdomain:id(), nkdomain:obj_id()) ->
    ok | {error, term()}.

attach_session(DeviceId, User, SessId) ->
    case nkdomain_lib:load(SessId) of
        #obj_id_ext{pid=Pid} ->
            case nkdomain_lib:find(User) of
                #obj_id_ext{obj_id=UserId, type=?DOMAIN_USER} ->
                    nkdomain_obj:sync_op(DeviceId, {?MODULE, attach_session, UserId, SessId, Pid});
                {error, object_not_found} ->
                    {error, user_not_found};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, invalid_session};
        {error, Error} ->
            {error, Error}
    end.

%% @private
-spec find_sso(nkdomain:id()) ->
    {ok, [UserId::nkdomain:obj_id()]} | {error, term()}.

find_sso(DeviceId) ->
    nkdomain_obj:sync_op(DeviceId, {?MODULE, find_sso}).


%% @private
-spec get_users(nkdomain:id()) ->
    {ok, [UserId::nkdomain:obj_id()]} | {error, term()}.

get_users(DeviceId) ->
    nkdomain_obj:sync_op(DeviceId, {?MODULE, get_users}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(user_session, {
    session_id :: nkdomain:obj_id(),
    user_id :: nkdomain:user_id()
}).


-record(session, {
   user_sessions = [] :: [#user_session{}]
}).



%% @private
object_info() ->
    #{
        type => ?DOMAIN_DEVICE
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 100
    }.


%% @private
object_es_mapping() ->
    #{
        sso_device_ids => #{type => keyword}
    }.


%% @private
object_parse(_Type, _Obj) ->
    #{
        sso_device_ids => {list, binary}
    }.


% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_device_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_send_event(Event, State) ->
    nkdomain_device_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_device_obj_cmd:cmd(Cmd, Req).


%% @private
object_init(State) ->
    Session = #session{},
    {ok, State#obj_state{session=Session}}.


%% @private
object_sync_op({?MODULE, attach_session, UserId, SessId, Pid}, _From, State) ->
    % We monitor the session and are alive while the session is
    State2 = nkdomain_obj:links_add(usage, {session, SessId, Pid}, State),
    #obj_state{session=Session} = State2,
    #session{user_sessions=UserSessions} = Session,
    UserSession = #user_session{session_id=SessId, user_id=UserId},
    UserSessions2 = lists:keystore(SessId, #user_session.session_id, UserSessions, UserSession),
    Session2 = Session#session{user_sessions=UserSessions2},
    State3 = State2#obj_state{session=Session2},
    ?LLOG(notice, "attached user ~s (~s)", [UserId, SessId], State3),
    {reply, ok, State3};

object_sync_op({?MODULE, get_users}, _From, #obj_state{session=#session{user_sessions=UserSessions}}=State) ->
    Users = [UserId || #user_session{user_id=UserId} <- UserSessions],
    {reply, {ok, Users}, State};


object_sync_op({?MODULE, find_sso}, _From, #obj_state{obj=Obj}=State) ->
    #{?DOMAIN_DEVICE:=DeviceObj} = Obj,
    SsoDevices = maps:get(sso_device_ids, DeviceObj, []),
    UserIds = lists:foldl(
        fun(DeviceId, Acc) ->
            case get_users(DeviceId) of
                {ok, Users} ->
                    lists:usort(Acc++Users);
                _ ->
                    Acc
            end
        end,
        [],
        SsoDevices),
    {reply, {ok, UserIds}, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_link_down({usage, {session, SessId, _Pid}}, State) ->
    #obj_state{session=#session{user_sessions=UserSessions}=Session} = State,
    UserSessions2 = lists:keydelete(SessId, #user_session.session_id, UserSessions),
    Session2 = Session#session{user_sessions=UserSessions2},
    State2 = State#obj_state{session=Session2},
    ?LLOG(notice, "detached sessions ~s", [SessId], State),
    {ok, State2};

object_link_down(_Link, _State) ->
    continue.

%% ===================================================================
%% Internal
%% ===================================================================
