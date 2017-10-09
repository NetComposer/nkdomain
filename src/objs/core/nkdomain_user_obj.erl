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
%% - Any session can tell the user to "launch" again all notificationa, for this specific session
%%

-module(nkdomain_user_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/1, auth/2, make_token/4, get_name/1, get_info/2]).
-export([find_childs/1]).
-export([object_info/0, object_admin_info/0, object_create/1, object_update/1, object_es_mapping/0, object_es_unparse/2,
         object_parse/2, object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export([object_init/1, object_save/1, object_event/2,
         object_sync_op/3, object_async_op/2, object_link_down/2, object_handle_info/2]).
-export([fun_user_pass/1, user_pass/1]).
-export([get_sessions/1, get_sessions/2, get_presence/3, update_presence/3]).
-export([register_session/5, unregister_session/2, launch_session_notifications/2, set_status/4, get_status/3]).
-export([add_token_notification/4, remove_token_notification/3]).
-export([add_push_device/5, remove_push_device/2, send_push/3, remove_all_push_devices/1]).

-export_type([events/0, push_msg/0, push_device_id/0, push_device_data/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).

-define(INVALID_PASSWORD_TIME, 500).
-define(USER_PASS_ITERS, 10).

%-define(MAX_EXPIRE, 60).    % Secs

%% ===================================================================
%% Types
%% ===================================================================

-type auth_opts() :: #{password=>binary()}.

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
        wakeup_push => term()
    }.


-type events() ::
    {session_started, nkdomain:type(), nkdomain:obj_id()} |
    {session_stopped, nkdomain:type(), nkdomain:obj_id()} |
    {status_updated, SrvId::nkservice:id(), Path::nkdomain:path(), user_status()} |
    {presence_updated, nkdomain:type(), Path::nkdomain:path(), user_presence()}.


%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(map()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

create(Obj) ->
    object_create(Obj).


%% @doc
-spec auth(User::binary(), auth_opts()) ->
    {ok, UserId::nkdomain:obj_id(), DomainId::nkdomain:obj_id()} |
    {error, user_not_found|term()}.

auth(UserId, #{password:=Pass}) ->
    Pass2 = user_pass(Pass),
    case nkdomain_obj:sync_op(UserId, {?MODULE, check_pass, Pass2}) of
        {ok, {true, ObjId, DomainId}} ->
            {ok, ObjId, DomainId};
        {ok, false} ->
            timer:sleep(?INVALID_PASSWORD_TIME),
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
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
    nkdomain_obj:sync_op(Id, {?MODULE, get_info, Opts}).


%% @doc
-spec register_session(nkdomain:id(), nkdomain:id(), nkdomain:type(),
                       nkdomain:obj_id(), sess_opts()) ->
                          ok | {error, term()}.

register_session(Id, Domain, Type, SessId, Opts) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, path=DomainPath} ->
            nkdomain_obj:sync_op(Id, {?MODULE, register_session, DomainPath, Type, SessId, Opts, self()});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
unregister_session(Id, SessId) ->
    nkdomain_obj:async_op(Id, {?MODULE, unregister_session, SessId}).


%% @doc
-spec get_sessions(nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_sessions(UserId) ->
    nkdomain_obj:sync_op(UserId, {?MODULE, get_sessions}).


%% @doc
-spec get_sessions(nkdomain:id(), nkdomain:type()) ->
    {ok, [{nkdomain:obj_id(), Meta::map(), pid()}]} | {error, term()}.

get_sessions(UserId, Type) ->
    nkdomain_obj:sync_op(UserId, {?MODULE, get_sessions, nklib_util:to_binary(Type)}).


%% @doc
-spec get_presence(nkdomain:id(), nkdomain:type(), nkdomain:id()) ->
    {ok, user_presence()} | {error, term()}.

get_presence(Id, Type, Domain) ->
    case nkdomain_lib:find(Domain) of
        #obj_id_ext{path=DomainPath} ->
            nkdomain_obj:sync_op(Id, {?MODULE, get_presence, Type, DomainPath});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec update_presence(nkdomain:id(), nkdomain:id(), session_presence()) ->
    ok | {error, term()}.

update_presence(Id, SessId, Presence) ->
    nkdomain_obj:async_op(Id, {?MODULE, update_presence, SessId, Presence}).


%% @doc See above
-spec set_status(nkdomain:id(), nkservice:id()|binary(), nkdomain:id(), user_status()) ->
    ok | {error, term()}.

set_status(Id, Srv, Domain, Status) when is_map(Status) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            {error, invalid_service};
        SrvId ->
            case nkdomain_lib:find(Domain) of
                #obj_id_ext{path=DomainPath} ->
                    nkdomain_obj:async_op(Id, {?MODULE, set_status, SrvId, DomainPath, Status});
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
            case nkdomain_lib:find(Domain) of
                #obj_id_ext{path=DomainPath} ->
                    nkdomain_obj:sync_op(Id, {?MODULE, get_status, SrvId, DomainPath});
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc See above
-spec add_token_notification(nkdomain:id(), nkdomain:type(), notification_opts(), nkdomain_token_obj:token_data()) ->
    {ok, UserId::nkdomain:obj_id(), nkdomain_token_obj:token_data()} | {error, term()}.

add_token_notification(Id, Type, Opts, Token) ->
    nkdomain_obj:sync_op(Id, {?MODULE, add_notification_op, Type, Opts, Token}).


%% @doc
-spec remove_token_notification(nkdomain:obj_id(), map(), term()) ->
    ok | {error, term()}.

remove_token_notification(Id, TokenId, Reason) ->
    nkdomain_obj:async_op(Id, {?MODULE, remove_notification, TokenId, Reason}).


%% @doc Force sends of all pending notifications for this session
launch_session_notifications(Id, SessId) ->
    nkdomain_obj:async_op(Id, {?MODULE, launch_session_notifications, SessId}).


%% @doc Registers a push device
-spec add_push_device(nkdomain:id(), nkdomain:id(), nkservice:id()|binary(), push_device_id(), push_device_data()) ->
    ok | {error, term()}.

add_push_device(Id, Domain, Srv, DeviceId, PushData) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            {error, invalid_service};
        SrvId ->
            case nkdomain_lib:find(Domain) of
                #obj_id_ext{path=DomainPath} ->
                    nkdomain_obj:async_op(Id, {?MODULE, add_push_device, DomainPath, SrvId, DeviceId, PushData});
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
remove_push_device(Id, DeviceId) ->
    nkdomain_obj:async_op(Id, {?MODULE, remove_push_device, DeviceId}).


%% @doc
remove_all_push_devices(Id) ->
    nkdomain_obj:async_op(Id, {?MODULE, remove_push_devices}).


%% @doc
-spec send_push(nkdomain:obj_id(), nkservice:id()|binary(), push_msg()) ->
    ok | {error, term()}.

send_push(Id, Srv, Push) ->
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            ok;
        SrvId ->
            nkdomain_obj:async_op(Id, {?MODULE, send_push, SrvId, Push})
    end.


%% @private
find_childs(User) ->
    case nkdomain_lib:find(User) of
        #obj_id_ext{obj_id=UserId} ->
            Spec = #{
                filters => #{
                    parent_id => UserId
                },
                fields => [<<"path">>]
            },
            nkdomain:search(Spec);
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(user_session, {
    id :: nkdomain:obj_id(),
    type :: nkdomain:type(),
    domain_path :: nkdomain:path(),
    presence :: session_presence(),
    opts = #{} :: sess_opts(),
    pid :: pid()
}).

-record(notify_token, {
    token_id :: binary(),
    domain_path :: nkdomain:path(),
    session_type :: binary(),
    data :: map()
}).

-record(push_device, {
    device_id :: binary(),
    domain_path :: nkdomain:path(),
    srv_id :: nkservice:id(),
    push_data :: push_device_data(),
    updated_time :: binary()
}).

-record(status, {
    id :: {nkservice:id(), nkdomain:path()},
    user_status :: user_status(),
    updated_time :: binary()
}).

-record(session, {
    user_sessions = [] :: [#user_session{}],
    user_session_notify = #{} :: #{nkdomain:type() => notify_fun()},
    user_session_presence = #{} :: #{nkdomain:type() => presence_fun()},
    notify_tokens = [] :: [#notify_token{}],
    push_devices = [] :: [#push_device{}],
    statuses = [] :: [#status{}],
    meta = #{}          % For future use
}).



%% @private
object_info() ->
    #{
        type => ?DOMAIN_USER,
        default_ttl => 5*60*1000
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 100,
        type_view_mod => nkdomain_user_obj_type_view,
        obj_view_mod => nkdomain_user_obj_view
    }.


%% @doc
object_create(Obj) ->
    nkdomain_obj_make:create(Obj#{type=>?DOMAIN_USER}).


%% @private
object_es_mapping() ->
    #{
        name => #{type => text},
        surname => #{type => text},
        name_sort => #{type => keyword},
        surname_sort =>  #{type => keyword},
        fullname_norm => #{type => keyword},
        email => #{type => keyword},
        password => #{type => keyword},
        phone_t => #{type => keyword},
        address_t => #{type => text},
        push => #{
            type => object,
            dynamic => false,
            properties => #{
                domain_path => #{type => keyword},
                srv_id => #{type => keyword},
                device_id => #{type => keyword},
                push_data => #{enabled => false},
                updated_time => #{type => date}
            }
        },
        status => #{
            type => object,
            dynamic => false,
            properties => #{
                domain_path => #{type => keyword},
                srv_id => #{type => keyword},
                user_status => #{enabled => false},
                updated_time => #{type => date}
            }
        },
        name_sort => #{type => keyword}
    }.


%% @private
object_es_unparse(Obj, Base) ->
    User = maps:get(?DOMAIN_USER, Obj),
    Name = maps:get(name, User, <<>>),
    SurName = maps:get(surname, User, <<>>),
    FullName = <<Name/binary, " ", SurName/binary>>,
    UserKeys = maps:keys(object_es_mapping()),
    UserMap = maps:with(UserKeys, User),
    UserMap2 = UserMap#{
        name_sort => nkdomain_store_es_util:normalize(Name),
        surname_sort => nkdomain_store_es_util:normalize(SurName),
        fullname_norm =>  nkdomain_store_es_util:normalize_multi(FullName)
    },
    Base#{
        ?DOMAIN_USER => UserMap2
    }.


%% @private
object_parse(update, _Obj) ->
    #{
        name => binary,
        surname => binary,
        password => fun ?MODULE:fun_user_pass/1,
        email => lower,
        phone_t => binary,
        address_t => binary,
        push => {list,
            #{
                domain_path => binary,
                srv_id => binary,
                device_id => binary,
                push_data => map,
                updated_time => integer,
                '__mandatory' => [srv_id, device_id, push_data, updated_time],
                '__defaults' => #{domain_path => <<>>}
                % add domain_path when all objects are updated
                %'__mandatory' => [domain_path, app_id, device_id, push_data, updated_time]
             }
        },
        status => {list,
             #{
                 domain_path => binary,
                 srv_id => binary,
                 user_status => map,
                 updated_time => integer,
                 '__mandatory' => [srv_id, user_status, updated_time],
                 '__defaults' => #{domain_path => <<>>}
                 % add domain_path when all objects are updated
                 % '__mandatory' => [domain_path, app_id, user_status, updated_time]
             }
        }
    };

object_parse(_Mode, Obj) ->
    Base = object_parse(update, Obj),
    Base#{'__mandatory' => [name, surname]}.


%% @doc
object_update(Obj) ->
    #{?DOMAIN_USER:=#{name:=Name, surname:=SurName}} = Obj,
    FullName = <<Name/binary, " ", SurName/binary>>,
    Obj2 = Obj#{name=>FullName},
    check_email(Obj2).


% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_user_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_send_event(Event, State) ->
    nkdomain_user_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_user_obj_cmd:cmd(Cmd, Req).



%% @private
%% We initialize soon in case of early terminate
object_init(#obj_state{obj=#{?DOMAIN_USER:=User}}=State) ->
    Push = maps:get(push, User, []),
    PushDevices = do_load_push(Push, []),
    Status = maps:get(status, User, []),
    Statuses = do_load_status(Status, []),
    Session = #session{push_devices=PushDevices, statuses=Statuses},
    {ok, State#obj_state{session=Session}}.



%% @private Prepare the object for saving
object_save(#obj_state{obj=Obj, session=Session}=State) ->
    #session{push_devices=Devices, statuses=Statuses} = Session,
    Push = do_save_push(Devices, []),
    Status = do_save_status(Statuses, []),
    #{?DOMAIN_USER:=User} = Obj,
    User2 = User#{push=>Push, status=>Status},
    Obj2 = ?ADD_TO_OBJ(?DOMAIN_USER, User2, Obj),
    {ok, State#obj_state{obj = Obj2}}.


%% @private
%% We detect the loading and unloading of token objects to see if they are notifications
object_event(Event, State) ->
    case Event of
        {child_loaded, ?DOMAIN_TOKEN, TokenId, Pid} ->
            nkdomain_obj:async_op(self(), {?MODULE, loaded_token, TokenId, Pid});
        {child_unloaded, ?DOMAIN_TOKEN, TokenId} ->
            nkdomain_obj:async_op(self(), {?MODULE, unloaded_token, TokenId});
        _ ->
            ok
    end,
    {ok, State}.


% @private
object_sync_op({?MODULE, check_pass, _Pass}, _From, #obj_state{is_enabled=false}=State) ->
    {reply, {error, object_is_disabled}, State};

object_sync_op({?MODULE, check_pass, Pass}, _From, #obj_state{id=Id, obj=Obj}=State) ->
    case Obj of
        #{domain_id:=DomainId, ?DOMAIN_USER:=#{password:=Pass}} ->
            #obj_id_ext{obj_id=ObjId} = Id,
            {reply, {ok, {true, ObjId, DomainId}}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

object_sync_op({?MODULE, get_info, Opts}, _From, #obj_state{obj=Obj}=State) ->
    Base = nkdomain_obj_util:get_obj_name(State),
    #{name:=UserName, surname:=UserSurName} = User = maps:get(?DOMAIN_USER, Obj),
    Data = Base#{
        name => UserName,
        surname => UserSurName,
        fullname => maps:get(name, Obj, UserName),
        email => maps:get(email, User, <<>>),
        phone_t => maps:get(phone_t, User, <<>>),
        address_t => maps:get(address_t, User, <<>>),
        icon_id => maps:get(icon_id, Obj, <<>>)
    },
    Path = case Opts of
        #{domain_id:=Domain} ->
            case nkdomain_util:is_path(Domain) of
                {true, DP} ->
                    DP;
                {false, _} ->
                    case nkdomain:find(Domain) of
                        #obj_id_ext{path=DP} -> DP;
                        _ -> undefined
                    end
            end;
        _ ->
            undefined
    end,
    Data2 = case Path /= undefined andalso Opts of
        #{srv_id:=SrvId} ->
            SrvId2 = nkdomain_obj_util:get_existing_srv_id(SrvId),
            case do_get_status(SrvId2, Path, State) of
                {ok, Status} ->
                    Data#{status=>Status};
                {error, _} ->
                    Data
            end;
        _ ->
            Data
    end,
    Data3 = case Path /= undefined andalso Opts of
        #{session_types:=Types} ->
            Presence = lists:foldl(
                fun(Type, Acc) ->
                    case do_get_presence(Type, Path, State) of
                        {ok, UserPres} ->
                            Acc#{Type => UserPres};
                        {error, _} ->
                            Acc
                    end
                end,
                #{},
                Types),
            Data2#{presence => Presence};
        _ ->
            Data2
    end,
    {reply, {ok, Data3}, State};

object_sync_op({?MODULE, register_session, DomainPath, Type, SessId, Opts, Pid}, _From, State) ->
    case find_session(SessId, State) of
        {ok, _} ->
            State2 = rm_session(SessId, State),
            {reply, ok, add_session(DomainPath, Type, SessId, Opts, Pid, State2)};
        not_found ->
            State2 = add_session(DomainPath, Type, SessId, Opts, Pid, State),
            {reply, ok, State2}
    end;

object_sync_op({?MODULE, get_sessions}, _From, #obj_state{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions),
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, get_sessions, Type}, _From, #obj_state{session=Session}=State) ->
    #session{user_sessions=UserSessions1} = Session,
    UserSessions2 = [US || #user_session{type=T}=US <- UserSessions1, T==Type],
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions2),
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, add_notification_op, SessType, Opts, Token}, _From, State) ->
    #obj_state{id=#obj_id_ext{srv_id=SrvId, obj_id=UserId}} = State,
    UserData1 = maps:get(?DOMAIN_USER, Token, #{}),
    UserNotification1 = #{
        <<"user_id">> => UserId,
        <<"srv_id">> => SrvId,
        <<"session_type">> => SessType
    },
    UserNotification2 = case Opts of
        #{wakeup_push:=Push} ->
            UserNotification1#{<<"wakeup_push">> => Push};
        _ ->
            UserNotification1
    end,
    UserData2 = UserData1#{<<"notification">> => UserNotification2},
    {reply, {ok, UserId, Token#{?DOMAIN_USER=>UserData2}}, State};

object_sync_op({?MODULE, get_status, SrvId, DomainPath}, _From, State) ->
    Reply = do_get_status(SrvId, DomainPath, State),
    {reply, Reply, State};

object_sync_op({?MODULE, get_presence, Type, DomainPath}, _From, State) ->
    Reply = do_get_presence(Type, DomainPath, State),
    {reply, Reply, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, unregister_session, SessId}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{domain_path=Path, type=Type}} ->
            State2 = rm_session(SessId, State),
            State3 = do_update_presence(Type, Path, State2),
            {noreply, State3};
        not_found ->
            {noreply, State}
    end;

object_async_op({?MODULE, update_presence, SessId, Presence}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{domain_path=Path, type=Type}=UserSession} ->
            UserSession2 = UserSession#user_session{presence=Presence},
            State2 = store_session(UserSession2, #{}, State),
            State3 = do_update_presence(Type, Path, State2),
            {noreply, State3};
        not_found ->
            {noreply, State}
    end;

object_async_op({?MODULE, launch_session_notifications, _SessId}=Msg, State) ->
    % Reply to client first
    erlang:send_after(1000, self(), Msg),
    {noreply, State};

object_async_op({?MODULE, set_status, SrvId, DomainPath, UserStatus}, State) ->
    State2 = do_set_status(SrvId, DomainPath, UserStatus, State),
    {noreply, State2};

object_async_op({?MODULE, loaded_token, TokenId, Pid}, State) ->
    case nkdomain_token_obj:get_token_data(Pid) of
        {ok, #{domain_id:=DomainId, data:=Data}} ->
            case Data of
                #{?DOMAIN_USER:=#{<<"notification">>:=Notification}} ->
                    ?LLOG(notice, "detected notification token ~s", [TokenId], State),
                    #{
                        <<"session_type">> := SessType
                    } = Notification,
                    case nkdomain_lib:find(DomainId) of
                        #obj_id_ext{path=DomainPath} ->
                            Notify = #notify_token{
                                token_id = TokenId,
                                domain_path = DomainPath,
                                session_type = SessType,
                                data = Data
                            },
                            #obj_state{session=Session1} = State,
                            #session{notify_tokens=Msgs} = Session1,
                            Session2 = Session1#session{notify_tokens=[Notify|Msgs]},
                            State2 = State#obj_state{session=Session2},
                            notify_token_sessions(Notify, created, State2),
                            {noreply, State2};
                        {error, _} ->
                            ?LLOG(warning, "received invalid token", [], State),
                            {noreply, State}
                    end;
                _ ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;

object_async_op({?MODULE, unloaded_token, TokenId}, State) ->
    ?LLOG(notice, "unloaded notification token ~s", [TokenId], State),
    State2 = do_remove_notification(TokenId, timeout, State),
    {noreply, State2};

object_async_op({?MODULE, remove_notification, TokenId, Reason}, State) ->
    State2 = do_remove_notification(TokenId, Reason, State),
    {noreply, State2};

object_async_op({?MODULE, add_push_device, DomainPath, SrvId, DeviceId, PushData}, State) ->
    State2 = add_push(DomainPath, SrvId, DeviceId, PushData, State),
    {noreply, State2};

object_async_op({?MODULE, send_push, SrvId, Push}, State) ->
    do_send_push(SrvId, Push, State),
    {noreply, State};

object_async_op({?MODULE, remove_push_device, DeviceId}, State) ->
    State2 = remove_push(DeviceId, State),
    {noreply, State2};

object_async_op({?MODULE, remove_push_devices}, State) ->
    State2 = do_remove_all_push_devices(State),
    {noreply, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({?MODULE, expired_notify, NotifyId}, State) ->
    lager:warning("NKLOG Expired ~p", [NotifyId]),
    State2 = do_remove_notification(NotifyId, timeout, State),
    {noreply, State2};

object_handle_info({?MODULE, launch_session_notifications, SessId}, #obj_state{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    State2 = case lists:keyfind(SessId, #user_session.id, UserSessions) of
        #user_session{} = UserSession ->
            do_launch_session_tokens(UserSession, State);
        false ->
            State
    end,
    {noreply, State2};

object_handle_info(_Info, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, session, SessId, _Pid}}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{domain_path=Path, type=Type}} ->
            State2 = do_event({session_stopped, Type, SessId}, State),
            ?DEBUG("registered session down: ~s", [SessId], State2),
            State3 = rm_session(SessId, State2),
            State4 = do_update_presence(Type, Path, State3),
            {ok, State4};
        not_found ->
            {ok, State}
    end;

object_link_down(_Link, State) ->
    {ok, State}.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
fun_user_pass(Pass) ->
    {ok, user_pass(Pass)}.


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


%% @private
check_email(#{?DOMAIN_USER:=#{email:=Email}}=Obj) ->
    Email2 = nklib_util:to_lower(Email),
    Aliases = maps:get(aliases, Obj, []),
    case lists:member(Email2, Aliases) of
        true ->
            {ok, Obj};
        false ->
            Spec = #{
                size => 0,
                filters => #{type=>?DOMAIN_USER, << ?DOMAIN_USER/binary, ".email">> => Email2}
            },
            case nkdomain:search(Spec) of
                {ok, 0, _, _} ->
                    {ok, Obj#{aliases=>Email2}};
                {ok, _, _, _} ->
                    {error, {email_duplicated, Email2}};
                {error, Error} ->
                    {error, Error}
            end
    end;

check_email(Obj) ->
    {ok, Obj}.


%% @private
do_update_presence(Type, DomainPath, State) ->
    case do_get_presence(Type, DomainPath, State) of
        {ok, UserPresence} ->
            do_event({presence_updated, Type, DomainPath, UserPresence}, State);
        {error, _} ->
            State
    end.


%% @private Gets presence status from all sessions with Domain and Type and call presence_fun() to get current
%% presence status
do_get_presence(DomainPath, Type, State) ->
    #obj_state{id=Id, session=Session} = State,
    #obj_id_ext{obj_id=UserId} =Id,
    #session{user_sessions=UserSessions, user_session_presence=PresFuns} = Session,
    case maps:find(Type, PresFuns) of
        {ok, Fun} ->
            PresenceList = [
                P || #user_session{type=T, domain_path=D, presence=P}
                     <- UserSessions, T==Type andalso D==DomainPath
            ],
            {ok, Presence} = Fun(UserId, PresenceList),
            {ok, Presence};
        error ->
            {error, unknown_presence}
    end.


%% @private
find_session(SessId, #obj_state{session=Session}) ->
    #session{user_sessions=UserSessions} = Session,
    case lists:keyfind(SessId, #user_session.id, UserSessions) of
        #user_session{} = UserSession ->
            {ok, UserSession};
        false ->
            not_found
    end.


%% @private
add_session(DomainPath, Type, SessId, Opts, Pid, State) ->
    UserSession = #user_session{
        id = SessId,
        domain_path = DomainPath,
        type = Type,
        presence = maps:get(presence, Opts, #{}),
        opts = maps:without([notify_fun, presence_fun, presence], Opts),
        pid = Pid
    },
    State2 = nkdomain_obj:links_add(usage, {?MODULE, session, SessId, Pid}, State),
    State3 = do_event({session_started, Type, SessId}, State2),
    State4 = store_session(UserSession, Opts, State3),
    do_update_presence(Type, DomainPath, State4).


%% @private
rm_session(SessId, #obj_state{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    case lists:keytake(SessId, #user_session.id, UserSessions) of
        {value, #user_session{pid=Pid}, UserSessions2} ->
            State2 = nkdomain_obj:links_remove(usage, {?MODULE, session, SessId, Pid}, State),
            Session2 = Session#session{user_sessions=UserSessions2},
            State2#obj_state{session=Session2};
        false ->
            State
    end.


%% @private
store_session(#user_session{id=SessId, type=Type}=UserSession, Opts, #obj_state{session=Session}=State) ->
    #session{
        user_sessions = UserSessions,
        user_session_notify = Notify,
        user_session_presence = Presence
    } = Session,
    UserSessions2 = lists:keystore(SessId, #user_session.id, UserSessions, UserSession),
    Notify2 = case maps:get(notify_fun, Opts, undefined) of
        Fun1 when is_function(Fun1, 2) ->
            Notify#{Type => Fun1};
        _ ->
            Notify
    end,
    Presence2 = case maps:get(presence_fun, Opts, undefined) of
        Fun2 when is_function(Fun2, 2) ->
            Presence#{Type => Fun2};
        _ ->
            Presence
    end,
    Session2 = Session#session{
            user_sessions = UserSessions2,
            user_session_notify = Notify2,
            user_session_presence = Presence2
    },
    State#obj_state{session=Session2}.


%% @private
export_session(#user_session{id=Id, type=Type, opts=Opts, presence=Presence, pid=Pid}) ->
    #{
        session_id => Id,
        type => Type,
        presence => Presence,
        opts => Opts,
        pid => Pid
    }.


%% @doc
do_remove_notification(TokenId, Reason, #obj_state{session=Session}=State) ->
    #session{notify_tokens=Msgs1} = Session,
    case lists:keytake(TokenId, #notify_token.token_id, Msgs1) of
        {value, #notify_token{}=Notify, Msgs2} ->
            notify_token_sessions(Notify, {removed, Reason}, State),
            Session2 = Session#session{notify_tokens=Msgs2},
            State#obj_state{session=Session2};
        false ->
            State
    end.


%% @private
notify_token_sessions(Notify, Op, State) ->
    #notify_token{domain_path=DomainPath, session_type=Type, token_id=TokenId, data=Data} = Notify,
    #obj_state{session=#session{user_sessions=UserSessions, user_session_notify=NotifyFuns}} = State,
    case maps:find(Type, NotifyFuns) of
        {ok, Fun} ->
            ?DEBUG("notify sessions ~s ~s", [DomainPath, Type], State),
            Num = lists:foldl(
                fun(#user_session{pid=Pid, type=T, domain_path=D}, Acc) ->
                    case T==Type andalso D==DomainPath of
                        true ->
                            case Op of
                                created->
                                    Fun(Pid, {token_created, TokenId, Data});
                                {removed, Reason} ->
                                    Fun(Pid, {token_removed, TokenId, Reason})
                            end,
                            Acc+1;
                        false ->
                            Acc
                    end
                end,
                0,
                UserSessions),
            case Op of
                _ when Num > 0 ->
                    ok;
                created ->
                    ?LLOG(notice, "no ~p session found: send wakeup", [Data], State),
                    ok;
%%                    Push = #{
%%                        type => simple,
%%                        title => <<"New user notification">>,
%%                        body => <<"Wake up">>
%%                    },
%%                    AppId = <<"user_notifications">>,
%%                    send_push(any, self(), AppId, Push);
                {removed, Reason} ->
                    ?LLOG(notice, "~s session notification removed: ~p", [Type, Reason]),
                    Push = #{
                        type => remove
                    },
                    AppId = <<"user_notifications">>,
                    send_push(self(), AppId, Push)
            end;
        error ->
            ok
    end.


%% @private
do_launch_session_tokens(#user_session{domain_path=DomainPath, type=Type, pid=Pid}, State) ->
    #obj_state{session=#session{user_session_notify=NotifyFuns}} = State,
    case maps:find(Type, NotifyFuns) of
        {ok, Fun} ->
            #obj_state{session=#session{notify_tokens=Msgs}} = State,
            lists:foreach(
                fun(#notify_token{token_id=Id, data=Data, session_type=T, domain_path=D}) ->
                    case T==Type andalso D==DomainPath of
                        true ->
                            Fun(Pid, {token_created, Id, Data});
                        false ->
                            ok
                    end
                end,
                Msgs);
        error ->
            ok
    end,
    State.


%% @private
do_load_push([], Acc) ->
    Acc;

do_load_push([Push|Rest], Acc) ->
    #{
        domain_path := DomainPath,
        srv_id := Srv,
        device_id := DeviceId,
        push_data := Data,
        updated_time := Time
    } = Push,
    PushDevice = #push_device{
        domain_path = DomainPath,
        srv_id = nkdomain_obj_util:get_srv_id(Srv),
        device_id = DeviceId,
        push_data = Data,
        updated_time = Time
    },
    Acc2 = lists:keystore(DeviceId, #push_device.device_id, Acc, PushDevice),
    do_load_push(Rest, Acc2).


%% @private
do_save_push([], Acc) ->
    Acc;

do_save_push([PushDevice|Rest], Acc) ->
    #push_device{
        device_id = DeviceId,
        srv_id = SrvId,
        domain_path = DomainPath,
        push_data = Data,
        updated_time = Time
    } = PushDevice,
    Push = #{
        device_id => DeviceId,
        srv_id => SrvId,
        domain_path => DomainPath,
        push_data => Data,
        updated_time => Time
    },
    do_save_push(Rest, [Push|Acc]).



%% @private
add_push(DomainPath, SrvId, DeviceId, PushData, State) ->
    #obj_state{session=Session} = State,
    #session{push_devices=PushDevices} = Session,
    Now =nkdomain_util:timestamp(),
    PushDevice = case lists:keyfind(DeviceId, #push_device.device_id, PushDevices) of
        #push_device{push_data=PushData} = PD ->
            PD#push_device{
                updated_time=Now
            };
        _ ->
            #push_device{
                domain_path = DomainPath,
                srv_id = SrvId,
                device_id = DeviceId,
                push_data = PushData,
                updated_time = Now
            }
    end,
    PushDevices2 = lists:keystore(DeviceId, #push_device.device_id, PushDevices, PushDevice),
    Session2 = Session#session{push_devices = PushDevices2},
    State#obj_state{session=Session2, is_dirty=true}.


%% @private
remove_push(DeviceId, State) ->
    #obj_state{session=Session} = State,
    #session{push_devices=PushDevices1} = Session,
    PushDevices2 = lists:keydelete(DeviceId, #push_device.device_id, PushDevices1),
    Session2 = Session#session{push_devices = PushDevices2},
    State#obj_state{session=Session2, is_dirty=true}.


%% @private
do_remove_all_push_devices(State) ->
    #obj_state{session=Session} = State,
    Session2 = Session#session{push_devices=[]},
    State#obj_state{session=Session2, is_dirty=true}.


%% @doc
do_send_push(SrvId, Push, State) ->
    Devices = find_push_devices(SrvId, State),
    lists:foreach(
        fun(#push_device{device_id=DeviceId, push_data=PushDevice}) ->
            ?LLOG(notice, "sending PUSH to device ~s (~s): ~p (~p)",
                          [DeviceId, SrvId, Push, PushDevice], State),
            SrvId:object_send_push(DeviceId, PushDevice, Push)
        end,
        Devices).


%% @private
find_push_devices(SrvId, State) ->
    #obj_state{session=Session} = State,
    #session{push_devices=Devices} = Session,
    [Device || #push_device{srv_id=S}=Device <- Devices, S==SrvId].


%% @private
do_get_status(SrvId, Path, State) ->
    #obj_state{session=#session{statuses=Statuses}} = State,
    Id = {SrvId, Path},
    case lists:keyfind(Id, #status.id, Statuses) of
        #status{user_status=UserStatus} ->
            {ok, UserStatus};
        false ->
            {ok, #{}}
    end.


%% @private
do_load_status([], Acc) ->
    Acc;

do_load_status([Status|Rest], Acc) ->
    #{
        domain_path := DomainPath,
        srv_id := Srv,
        user_status := UserStatus,
        updated_time := Time
    } = Status,
    SrvId = nkdomain_obj_util:get_srv_id(Srv),
    Id = {SrvId, DomainPath},
    Status2 = #status{
        id = Id,
        user_status = UserStatus,
        updated_time= Time
    },
    Acc2 = lists:keystore(Id, #status.id, Acc, Status2),
    do_load_status(Rest, Acc2).


%% @private
do_save_status([], Acc) ->
    Acc;

do_save_status([Status|Rest], Acc) ->
    #status{
        id = {SrvId, Path},
        user_status = UserStatus,
        updated_time= Time
    } = Status,
    Status2 = #{
        srv_id => SrvId,
        domain_path => Path,
        user_status => UserStatus,
        updated_time => Time
    },
    do_save_status(Rest, [Status2|Acc]).


%% @private
do_set_status(SrvId, Path, UserStatus, #obj_state{session=Session}=State) ->
    #session{statuses=Statuses} = Session,
    Id = {SrvId, Path},
    Now = nkdomain_util:timestamp(),
    Status = #status{
        id = Id,
        user_status = UserStatus,
        updated_time= Now
    },
    Statuses2 = lists:keystore(Id, #status.id, Statuses, Status),
    Session2 = Session#session{statuses=Statuses2},
    State2 = do_event({status_updated, SrvId, Path, UserStatus}, State),
    State2#obj_state{session=Session2, is_dirty=true}.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).
