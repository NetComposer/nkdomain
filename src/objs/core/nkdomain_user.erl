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

%% @doc User Object

%% Session registrations
%% ---------------------
%%
%% - Sessions can register with the user object calling register_session():
%%      - SessId
%%      - Type
%%      - Domain path (used for presence)
%%      - Opts (initial presence, presence_fun, status_fun)
%% - The server keeps track of sessions (usually as childs).
%%
%%
%% Presence management
%% -------------------
%%
%% - Each session can define its current session_presence(), at load or later on calling update_presence()
%% - At any moment, the current user_presence() can be calculated calling presence_fun(), using
%%   session_presence() from all sessions with this Type and Domain
%% - If a session goes down, user_presence() is updated (removing this session_presence() from the list)
%% - Each time presence changes (because of session registrations, sessions down or update_presence()),
%%   the new user_presence() is calculated and a presence_update event is launched
%% - Current user_presence() can be get calling get_presence() or get_name/2
%%
%%
%% Status management
%% -----------------
%%
%% - An user can have an status(), associated to any service and domain path
%% - Status can be updated with set_status() and get with get_status() and get_name/2
%% - Event status_updated is launched any time it changes
%%
%%
%% Push sending
%% ------------
%%
%% - An user must first store a push device, with add_push_device()
%%    - push_device_id()
%%    - Domain path: not used yet
%%    - nkservice:id(): service that must implement object_send_push/3
%%    - push_device_data()
%%
%% - Then it can use send_push() to send a push_msg() to all registered devices with that service,
%%   and will call SrvId:object_send_push/3 for each
%%
%%
%% Notification process
%% --------------------
%%
%% If we want to generate a notification directed to this user:
%%
%% - First generate a token, child of this user, and info from add_token_notification()
%% - When the token is loaded, the user is also loaded as father
%% - The user detects the token, having the info, and writes down the notification, with
%%   the domain path and the session type
%% - If a session has registered that type with a notify fun:
%%      - we find all user sessions with that type that registered that domain path, and call notify_fun()
%%      - if any is available, that's it
%%      - if none is available, we may send a wakeup push
%% - The notify remains stored (and the user loaded) until the toke is destroyed
%% - At that moment, the unload of the token is detected, and the notification is deleted
%% - If a session has registered that type with a notify fun:
%%      - we find all user sessions with that type that registered that domain path, and call notify_fun()
%%      - if any is available, that's it
%%      - if none is available, we may send a wakeup removal push
%% - Any session can tell the user to "launch" again all notifications, for this specific session
%%

-module(nkdomain_user).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, auth/2, make_token/4, get_name/1, get_info/2]).
-export([user_pass/1]).
-export([get_sessions/1, get_sessions/2, get_presence/2, get_presence/3, update_presence/3]).
-export([register_session/5, unregister_session/2, launch_session_notifications/2, set_status/4, get_status/3]).
-export([add_token_notification/4, remove_token_notification/3]).
-export([add_push_device/5, remove_push_device/2, send_push/3, remove_all_push_devices/1, remove_push_devices/2,
         get_push_devices/2]).
-export([sync_op/2, async_op/2]).

-export_type([events/0, push_msg/0, push_device_id/0, push_device_data/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).

-define(INVALID_PASSWORD_TIME, 500).
-define(USER_PASS_ITERS, 10).
-define(USER_MOD, nkdomain_user_obj).


%-define(MAX_EXPIRE, 60).    % Secs


%% ===================================================================
%% Types
%% ===================================================================

-type auth_opts() ::
    #{
        force_auth => boolean(),
        password => binary(),
        sso_device_id => binary()
    }.

-type sess_opts() ::
    #{
        notify_fun => notify_fun(),
        presence_fun => presence_fun(),
        presence => session_presence(),             % Used when calling presence_fun
        term() => term()
    }.

-type notify_msg() ::
    {token_created, nkdomain:obj_id(), nkdomain_token_obj:token_data()} |
    {token_removed, nkdomain:obj_id(), Reason::term()}.


-type notify_fun() ::
    fun((Pid::pid(), notify_msg()) -> ok).


-type session_presence() :: term().
-type user_presence() :: term().

-type presence_fun() ::
    fun((UserId::binary(), [session_presence()]) -> {ok, user_presence()}).

-type user_status() :: map().

-type push_msg() ::
    #{
        type => simple,
        title => binary(),
        body =>binary()
    }.

-type push_device_id() :: binary().

-type push_device_data() ::
    #{
        push_id => binary(),
        platform_id => binary(),
        platform_version => binary(),
        base_url => binary()
    }.

-type notification_opts() ::
    #{
        srv_id => nkservice:id(),   % For push sending
        wakeup_push => push_msg()
    }.


-type events() ::
    {session_started, nkdomain:type(), nkdomain:obj_id()} |
    {session_stopped, nkdomain:type(), nkdomain:obj_id()} |
    {status_updated, SrvId::nkservice:id(), Path::nkdomain:path(), user_status()} |
    {presence_updated, nkdomain:type(), Path::nkdomain:path(), user_presence()}.


-type create_opts() ::
    #{
        parent_id => nkdomain:id(),
        created_by => nkdomain:id(),
        ttl => integer(),
        tags => [binary()],
        name => binary(),
        surname => binary(),
        email => binary()

    }.


%% ===================================================================
%% API
%% ===================================================================


%% @doc
-spec create(nkdomain:id(), create_opts()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

create(Domain, Opts) ->
    Base = maps:with([parent_id, created_by, ttl, tags], Opts),
    User = maps:with([name, surname, email], Opts),
    Obj = Base#{
        type => ?DOMAIN_USER,
        domain_id => Domain,
        ?DOMAIN_USER => User
    },
    case nkdomain_user_obj:check_email(Obj) of
        {ok, Obj2} ->
            case nkdomain_obj_make:create(Obj2) of
                {ok, #obj_id_ext{obj_id=UserId, pid=Pid}, _} ->
                    {ok, UserId, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc
-spec auth(User::binary(), auth_opts()) ->
    {ok, UserId::nkdomain:obj_id(), DomainId::nkdomain:obj_id()} |
    {error, user_not_found|term()}.

auth(User, #{force_auth:=true}) ->
    case sync_op(User, {dont_check}) of
        {ok, {true, UserId, DomainId}} ->
            {ok, UserId, DomainId};
        {ok, false} ->
            timer:sleep(?INVALID_PASSWORD_TIME),
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end;

auth(User, #{password:=Pass}) ->
    Pass2 = user_pass(Pass),
    case sync_op(User, {check_pass, Pass2}) of
        {ok, {true, UserId, DomainId}} ->
            {ok, UserId, DomainId};
        {ok, false} ->
            timer:sleep(?INVALID_PASSWORD_TIME),
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end;

auth(User, #{sso_device_id:=DeviceId}) ->
    case sync_op(User, {check_device, DeviceId}) of
        {ok, {true, UserId, DomainId}} ->
            {ok, UserId, DomainId};
        {ok, false} ->
            timer:sleep(?INVALID_PASSWORD_TIME),
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end;

auth(User, _Data) ->
    auth(User, #{password=><<>>}).


%% @doc Generates a password from an user password or hash
-spec user_pass(string()|binary()) ->
    binary().

user_pass(Pass) ->
    Pass2 = nklib_util:to_binary(Pass),
    case binary:split(Pass2, <<"!">>, [global]) of
        [<<"NKD">>, <<>>, P, <<>>] when byte_size(P) > 10 ->
            Pass2;
        _ ->
            Salt = <<"netcomposer">>,
            Iters = ?USER_PASS_ITERS,
            {ok, Pbkdf2} = pbkdf2:pbkdf2(sha, Pass2, Salt, Iters),
            Hash = nklib_util:lhash(Pbkdf2),
            <<"NKD!!", Hash/binary, "!">>
    end.


%% @doc
-spec make_token(nkdomain:id(), nkdomain:id(), #{ttl=>integer()}, map()) ->
    {ok, nkdomain:obj_id(), integer()} | {error, term()}.

make_token(DomainId, UserId, TokenOpts, TokenData) ->
    TokenOpts2 = TokenOpts#{
        parent_id => UserId,
        created_by => UserId,
        subtype => ?DOMAIN_USER
    },
    case nkdomain_token_obj:create(DomainId, TokenOpts2, TokenData) of
        {ok, TokenId, _Pid, TTL} ->
            {ok, TokenId, TTL};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_name(nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_name(Id) ->
    get_info(Id, #{}).


%% @doc
-type get_name_opts() ::
    #{
        domain_id => nkdomain:id(),
        srv_id => nksevice:id() | binary(),
        session_types => [nkdomain:type()]
    }.

-spec get_info(nkdomain:id(), get_name_opts()) ->
    {ok, map()} | {error, term()}.

get_info(Id, Opts) ->
    sync_op(Id, {get_info, Opts}).


%% @doc
-spec register_session(nkdomain:id(), nkdomain:id(), nkdomain:type(),
                       nkdomain:obj_id(), sess_opts()) ->
                          ok | {error, term()}.

register_session(Id, Domain, Type, SessId, Opts) ->
    case nkdomain_db:find(Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, path=DomainPath} ->
            sync_op(Id, {register_session, DomainPath, Type, SessId, Opts, self()});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
unregister_session(Id, SessId) ->
    async_op(Id, {unregister_session, SessId}).


%% @doc
-spec get_sessions(nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_sessions(UserId) ->
    sync_op(UserId, get_sessions).


%% @doc
-spec get_sessions(nkdomain:id(), nkdomain:type()) ->
    {ok, [{nkdomain:obj_id(), Meta::map(), pid()}]} | {error, term()}.

get_sessions(UserId, Type) ->
    sync_op(UserId, {get_sessions, nklib_util:to_binary(Type)}).


%% @doc
-spec get_presence(nkdomain:id(), nkdomain:type()) ->
    {ok, user_presence()} | {error, term()}.

get_presence(Id, Type) ->
    sync_op(Id, {get_presence, Type}).


%% @doc
-spec get_presence(nkdomain:id(), nkdomain:type(), nkdomain:id()) ->
    {ok, user_presence()} | {error, term()}.

get_presence(Id, Type, Domain) ->
    case nkdomain_db:find(Domain) of
        #obj_id_ext{path=DomainPath} ->
            sync_op(Id, {get_presence, Type, DomainPath});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec update_presence(nkdomain:id(), nkdomain:id(), session_presence()) ->
    ok | {error, term()}.

update_presence(Id, SessId, Presence) ->
    async_op(Id, {update_presence, SessId, Presence}).


%% @doc See above
-spec set_status(nkdomain:id(), nkservice:id()|binary(), nkdomain:id(), user_status()) ->
    ok | {error, term()}.

set_status(Id, Srv, Domain, Status) when is_map(Status) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            {error, invalid_service};
        SrvId ->
            case nkdomain_db:find(Domain) of
                #obj_id_ext{path=DomainPath} ->
                    async_op(Id, {set_status, SrvId, DomainPath, Status});
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
-spec get_status(nkdomain:id(), nkservice:id()|binary(), nkdomain:id()) ->
    {ok, user_status()} | {error, term()}.

get_status(Id, Srv, Domain) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            {error, invalid_service};
        SrvId ->
            case nkdomain_db:find(Domain) of
                #obj_id_ext{path=DomainPath} ->
                    sync_op(Id, {get_status, SrvId, DomainPath});
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc See above
-spec add_token_notification(nkdomain:id(), nkdomain:type(), notification_opts(), nkdomain_token_obj:token_data()) ->
    {ok, UserId::nkdomain:obj_id(), nkdomain_token_obj:token_data()} | {error, term()}.

add_token_notification(Id, Type, Opts, Token) ->
    sync_op(Id, {add_notification_op, Type, Opts, Token}).


%% @doc
-spec remove_token_notification(nkdomain:obj_id(), map(), term()) ->
    ok | {error, term()}.

remove_token_notification(Id, TokenId, Reason) ->
    async_op(Id, {remove_notification, TokenId, Reason}).


%% @doc Force sends of all pending notifications for this session
launch_session_notifications(Id, SessId) ->
    async_op(Id, {launch_session_notifications, SessId}).


%% @doc Registers a push device
-spec add_push_device(nkdomain:id(), nkdomain:id(), nkservice:id()|binary(), push_device_id(), push_device_data()) ->
    ok | {error, term()}.

add_push_device(Id, Domain, Srv, DeviceId, PushData) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            {error, invalid_service};
        SrvId ->
            case nkdomain_db:find(Domain) of
                #obj_id_ext{path=DomainPath} ->
                    async_op(Id, {add_push_device, DomainPath, SrvId, DeviceId, PushData});
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
remove_push_device(Id, DeviceId) ->
    async_op(Id, {remove_push_device, DeviceId}).


%% @doc
remove_all_push_devices(Id) ->
    async_op(Id, {remove_push_devices}).


%% @doc
remove_push_devices(Id, SrvId) ->
    async_op(Id, {remove_push_devices, SrvId}).


%% @doc
get_push_devices(Id, SrvId) ->
    sync_op(Id, {get_push_devices, SrvId}).


%% @doc
-spec send_push(nkdomain:obj_id(), nkservice:id()|binary(), push_msg()) ->
    ok | {error, term()}.

send_push(Id, Srv, Push) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            ok;
        SrvId ->
            async_op(Id, {send_push, SrvId, Push})
    end.


%%%% @private
%%find_childs(User) ->
%%    case nkdomain_db:find(User) of
%%        #obj_id_ext{obj_id=UserId} ->
%%            Spec = #{
%%                filters => #{
%%                    parent_id => UserId
%%                },
%%                fields => [<<"path">>]
%%            },
%%            nkdomain:search(Spec);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @private
sync_op(User, Op) ->
    nkdomain_obj:sync_op(User, {nkdomain_user_obj, Op}).


%% @private
async_op(User, Op) ->
    nkdomain_obj:async_op(User, {nkdomain_user_obj, Op}).
