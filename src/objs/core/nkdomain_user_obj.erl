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

-module(nkdomain_user_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, auth/3, make_token/5, get_name/2]).
-export([find_childs/2]).
-export([object_info/0, object_admin_info/0, object_create/2, object_es_mapping/0, object_es_unparse/3,
         object_parse/3, object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export([object_init/1, object_save/1, object_event/2,
         object_sync_op/3, object_async_op/2, object_link_down/2, object_handle_info/2]).

-export([fun_user_pass/1, user_pass/1]).
-export([get_sessions/2, get_sessions/3]).
-export([register_session/6, unregister_session/3, launch_session_notifications/3, set_status/5, get_status/4]).
-export([add_notification_op/5, remove_notification/4]).
-export([add_push_device/6, remove_push_device/3, send_push/4, remove_push_devices/2]).

-export_type([events/0, push_msg/0, push_app_id/0, push_device_id/0, push_device/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).

-define(INVALID_PASSWORD_TIME, 500).

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
    {token_created, nkdomain:obj_id(), map()} |
    {token_removed, nkdomain:obj_id(), Reason::term()}.


-type notify_fun() ::
    fun((Pid::pid(), notify_msg()) -> ok).


-type session_presence() :: term().
-type user_presence() :: term().

-type presence_fun() :: fun((UserId::binary(), [session_presence()]) -> {ok, user_presence()}).

-type push_msg() ::
    #{
        type => simple,
        title => binary(),
        body =>binary()
    }.

-type push_app_id() :: binary().

-type push_device_id() :: binary().

-type push_device() ::
    #{
        push_id => binary(),
        platform_id => binary(),
        platform_version => binary(),
        base_url => binary()
    }.


-type events() ::
    {session_started, nkdomain:type(), nkdomain:obj_id()} |
    {session_stopped, nkdomain:type(), nkdomain:obj_id()} |
    {status_updated, Domain::nkdomain:path(), AppId::binary(), Status::map()} |
    {presence_updated, Domain::nkdomain:path(), nkdomain:type(), user_presence()}.


%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(nkservice:id(), map()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

create(SrvId, Obj) ->
    object_create(SrvId, Obj).


%% @doc
-spec auth(nkservice:id(), User::binary(), auth_opts()) ->
    {ok, UserId::nkdomain:obj_id(), DomainId::nkdomain:obj_id()} |
    {error, user_not_found|term()}.

auth(SrvId, UserId, #{password:=Pass}) ->
    Pass2 = user_pass(Pass),
    case nkdomain_obj:sync_op(SrvId, UserId, {?MODULE, check_pass, Pass2}) of
        {ok, {true, ObjId, DomainId}} ->
            {ok, ObjId, DomainId};
        {ok, false} ->
            timer:sleep(?INVALID_PASSWORD_TIME),
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end.

%% @doc
-spec make_token(nkservice:id(), nkdomain:id(), nkdomain:id(), #{ttl=>integer()}, map()) ->
    {ok, nkdomain:obj_id(), integer()} | {error, term()}.

make_token(SrvId, DomainId, UserId, TokenOpts, TokenData) ->
    case nkdomain_token_obj:create(SrvId, DomainId, UserId, UserId, ?DOMAIN_USER, TokenOpts, TokenData) of
        {ok, TokenId, _Pid, TTL, _Unknown} ->
            {ok, TokenId, TTL};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_sessions(nkservice:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_sessions(SrvId, UserId) ->
    nkdomain_obj:sync_op(SrvId, UserId, {?MODULE, get_sessions}).


%% @doc
-spec get_sessions(nkservice:id(), nkdomain:id(), nkdomain:type()) ->
    {ok, [{nkomain:obj_id(), Meta::map(), pid()}]} | {error, term()}.

get_sessions(SrvId, UserId, Type) ->
    nkdomain_obj:sync_op(SrvId, UserId, {?MODULE, get_sessions, nklib_util:to_binary(Type)}).


%% @doc
-spec get_name(nkservice:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_name(Srv, Id) ->
    nkdomain_obj:sync_op(Srv, Id, {?MODULE, get_name}).


%% @doc
-spec register_session(nkservice:id(), nkdomain:id(), nkdomain:id(), nkdomain:type(),
                       nkdomain:obj_id(), sess_opts()) ->
    ok | {error, term()}.

register_session(SrvId, Id, Domain, Type, SessId, Opts) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{type=?DOMAIN_DOMAIN, path=DomainPath} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, register_session, DomainPath, Type, SessId, Opts, self()});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
unregister_session(SrvId, Id, SessId) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, unregister_session, SessId}).


%% @doc
launch_session_notifications(SrvId, Id, SessId) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, launch_session_notifications, SessId}).


%% @doc Statuses currently indexed by AppId
set_status(SrvId, Id, Domain, AppId, Status) when is_map(Status) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{path=DomainPath} ->
            nkdomain_obj:async_op(SrvId, Id, {?MODULE, set_status, DomainPath, AppId, Status});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get_status(SrvId, Id, Domain, AppId) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{path=DomainPath} ->
            nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_status, DomainPath, AppId});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Adds notification info to a token
%% To generate a notification, make a token child of this user with the information this function returns
%% See nkchat_session_obj:send_invitation() for a sample
%% If the user object dies, the token will reload it automatically

-spec add_notification_op(nkservice:id(), nkdomain:id(), nkdomain:type(), #{}, map()) ->
    {ok, UserId::nkdomain:obj_id(), map()} | {error, term()}.

add_notification_op(SrvId, Id, SessType, Opts, Base) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, add_notification_op, SessType, Opts, Base}).


%% @doc
-spec remove_notification(nkservice:id(), nkdomain:obj_id(), map(), term()) ->
    ok | {error, term()}.

remove_notification(SrvId, Id, TokenId, Reason) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, remove_notification, TokenId, Reason}).


%% @doc
add_push_device(SrvId, Id, Domain, AppId, DeviceId, PushData) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{path=DomainPath} ->
            nkdomain_obj:async_op(SrvId, Id, {?MODULE, add_push_device, DomainPath, AppId, DeviceId, PushData});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
remove_push_device(SrvId, Id, DeviceId) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, remove_push_device, DeviceId}).


%% @doc
remove_push_devices(SrvId, Id) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, remove_push_devices}).


%% @doc
-spec send_push(nkservice:id(), nkdomain:obj_id(), push_app_id(), push_msg()) ->
    ok | {error, term()}.

send_push(SrvId, Id, AppId, Push) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, send_push, AppId, Push}).


%% @private
find_childs(SrvId, User) ->
    case nkdomain_lib:find(SrvId, User) of
        #obj_id_ext{obj_id=UserId} ->
            Spec = #{
                filters => #{
                    parent_id => UserId
                },
                fields => [<<"path">>]
            },
            nkdomain:search(SrvId, Spec);
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
    app_id :: binary(),
    push_data :: push_device(),
    updated_time :: binary()
}).

-record(status, {
    domain_path :: nkdomain:path(),
    app_id :: binary(),
    user_status :: map(),
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
object_create(SrvId, Obj) ->
    case check_email(SrvId, Obj) of
        {ok, Obj2} ->
            nkdomain_obj_make:create(SrvId, Obj2#{type=>?DOMAIN_USER});
        {error, Error} ->
            {error, Error}
    end.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
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
                app_id => #{type => keyword},
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
                app_id => #{type => keyword},
                user_status => #{enabled => false},
                updated_time => #{type => date}
            }
        },
        name_sort => #{type => keyword}
    }.


%% @private
object_es_unparse(_SrvId, Obj, Base) ->
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
object_parse(_SrvId, update, _Obj) ->
    #{
        vsn => binary,
        name => binary,
        surname => binary,
        password => fun ?MODULE:fun_user_pass/1,
        email => lower,
        phone_t => binary,
        address_t => binary,
        push => {list,
            #{
                domain_path => binary,
                app_id => binary,
                device_id => binary,
                push_data => map,
                updated_time => integer,
                '__mandatory' => [domain_path, app_id, device_id, push_data, updated_time]
             }
        },
        status => {list,
             #{
                 domain_path => binary,
                 app_id => binary,
                 user_status => map,
                 updated_time => integer,
                 '__mandatory' => [domain_path, app_id, user_status, updated_time]
             }
        },
        '__defaults' => #{vsn => <<"1">>}
    };

object_parse(SrvId, Mode, Obj) ->
    {BaseSyn, Opts} = SrvId:object_syntax(SrvId, Mode),
    Syntax1 = object_parse(SrvId, update, Obj),
    Syntax2 = Syntax1#{'__mandatory' => [name, surname]},
    case nklib_syntax:parse(Obj, BaseSyn#{?DOMAIN_USER=>Syntax2}, Opts) of
        {ok, Obj2, Unknown} ->
            #{?DOMAIN_USER:=#{name:=Name, surname:=SurName}} = Obj2,
            FullName = <<Name/binary, " ", SurName/binary>>,
            Obj3 = Obj2#{name=>FullName},
            {ok, Obj3, Unknown};
        {error, Error} ->
            {error, Error}
    end.


% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_user_obj_syntax:api(Cmd, Syntax).


%% @private
object_send_event(Event, State) ->
    nkdomain_user_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_user_obj_api:cmd(Cmd, Req).



%% @private
%% We initialize soon in case of early terminate
object_init(#?STATE{obj=#{?DOMAIN_USER:=User}}=State) ->
    Push = maps:get(push, User, []),
    PushDevices = do_load_push(Push, []),
    Status = maps:get(status, User, []),
    Statuses = do_load_status(Status, []),
    Session = #session{push_devices=PushDevices, statuses=Statuses},
    {ok, State#?STATE{session=Session}}.



%% @private Prepare the object for saving
object_save(#?STATE{obj=Obj, session=Session}=State) ->
    #session{push_devices=Devices, statuses=Statuses} = Session,
    Push = do_save_push(Devices, []),
    Status = do_save_status(Statuses, []),
    #{?DOMAIN_USER:=User} = Obj,
    User2 = User#{push=>Push, status=>Status},
    Obj2 = ?ADD_TO_OBJ(?DOMAIN_USER, User2, Obj),
    {ok, State#?STATE{obj = Obj2}}.


%% @private
%% We detect the loading and unloading of token objects to see if they are notifications
object_event(Event, State) ->
    case Event of
        {child_loaded, ?DOMAIN_TOKEN, TokenId, Pid} ->
            nkdomain_obj:async_op(any, self(), {?MODULE, loaded_token, TokenId, Pid});
        {child_unloaded, ?DOMAIN_TOKEN, TokenId} ->
            nkdomain_obj:async_op(any, self(), {?MODULE, unloaded_token, TokenId});
        _ ->
            ok
    end,
    {ok, State}.


% @private
object_sync_op({?MODULE, check_pass, _Pass}, _From, #?STATE{is_enabled=false}=State) ->
    {reply, {error, object_is_disabled}, State};

object_sync_op({?MODULE, check_pass, Pass}, _From, #?STATE{id=Id, obj=Obj}=State) ->
    case Obj of
        #{domain_id:=DomainId, ?DOMAIN_USER:=#{password:=Pass}} ->
            #obj_id_ext{obj_id=ObjId} = Id,
            {reply, {ok, {true, ObjId, DomainId}}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

object_sync_op({?MODULE, get_name}, _From, #?STATE{obj=Obj}=State) ->
    Base = nkdomain_obj_util:get_obj_name(State),
    #{name:=UserName, surname:=UserSurName} = User = maps:get(?DOMAIN_USER, Obj),
    Data = Base#{
        name => UserName,
        surname => UserSurName,
        fullname => maps:get(name, Obj, <<>>),
        email => maps:get(email, User, <<>>),
        phone_t => maps:get(phone_t, User, <<>>),
        address_t => maps:get(address_t, User, <<>>),
        icon_id => maps:get(icon_id, Obj, <<>>)
    },
    {reply, {ok, Data}, State};

object_sync_op({?MODULE, register_session, DomainPath, Type, SessId, Opts, Pid}, _From, State) ->
    case find_session(SessId, State) of
        {ok, _} ->
            State2 = rm_session(SessId, State),
            {reply, ok, add_session(DomainPath, Type, SessId, Opts, Pid, State2)};
        not_found ->
            State2 = add_session(DomainPath, Type, SessId, Opts, Pid, State),
            {reply, ok, State2}
    end;

object_sync_op({?MODULE, get_sessions}, _From, #?STATE{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions),
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, get_sessions, Type}, _From, #?STATE{session=Session}=State) ->
    #session{user_sessions=UserSessions1} = Session,
    UserSessions2 = [US || #user_session{type=T}=US <- UserSessions1, T==Type],
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions2),
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, add_notification_op, SessType, _Opts, Base}, _From, State) ->
    #?STATE{srv_id=SrvId, id=#obj_id_ext{obj_id=UserId}} = State,
    UserData1 = maps:get(?DOMAIN_USER, Base, #{}),
    UserData2 = UserData1#{
        <<"notification">> => #{
            <<"user_id">> => UserId,
            <<"srv_id">> => SrvId,
            <<"session_type">> => SessType
        }
    },
    {reply, {ok, UserId, Base#{?DOMAIN_USER=>UserData2}}, State};

object_sync_op({?MODULE, get_status, DomainPath, AppId}, _From, State) ->
    #?STATE{session=#session{statuses=Statuses}} = State,
    Reply = case lists:keyfind(AppId, #status.app_id, Statuses) of
        #status{domain_path=DomainPath, user_status=UserStatus} ->
            {ok, UserStatus};
        #status{} ->
            {error, domain_invalid};
        false ->
            {ok, #{}}
    end,
    {reply, Reply, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, unregister_session, SessId}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{domain_path=Path, type=Type}} ->
            State2 = rm_session(SessId, State),
            State3 = update_presence(Path, Type, State2),
            {noreply, State3};
        not_found ->
            {noreply, State}
    end;

object_async_op({?MODULE, launch_session_notifications, SessId}, #?STATE{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    State2 = case lists:keyfind(SessId, #user_session.id, UserSessions) of
        #user_session{} = UserSession ->
            do_launch_session_tokens(UserSession, State);
        false ->
            State
    end,
    {noreply, State2};

object_async_op({?MODULE, set_status, DomainPath, AppId, UserStatus}, State) ->
    State2 = do_set_status(DomainPath, AppId, UserStatus, State),
    {noreply, State2};

object_async_op({?MODULE, loaded_token, TokenId, Pid}, #?STATE{srv_id=SrvId}=State) ->
    case nkdomain_token_obj:get_token_data(any, Pid) of
        {ok, #{domain_id:=DomainId, data:=Data}} ->
            case Data of
                #{?DOMAIN_USER:=#{<<"notification">>:=Notification}} ->
                    ?LLOG(notice, "detected notification token ~s", [TokenId], State),
                    #{
                        <<"session_type">> := SessType
                    } = Notification,
                    case nkdomain_lib:find(SrvId, DomainId) of
                        #obj_id_ext{path=DomainPath} ->
                            Notify = #notify_token{
                                token_id = TokenId,
                                domain_path = DomainPath,
                                session_type = SessType,
                                data = Data
                            },
                            #?STATE{session=Session1} = State,
                            #session{notify_tokens=Msgs} = Session1,
                            Session2 = Session1#session{notify_tokens=[Notify|Msgs]},
                            % Add usage so that the object is not unloaded
                            State2 = State#?STATE{session=Session2},
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

object_async_op({?MODULE, add_push_device, DomainPath, AppId, DeviceId, PushData}, State) ->
    State2 = add_push(DomainPath, AppId, DeviceId, PushData, State),
    {noreply, State2};

object_async_op({?MODULE, send_push, AppId, Push}, State) ->
    send_push(AppId, Push, State),
    {noreply, State};

object_async_op({?MODULE, remove_push_device, DeviceId}, State) ->
    State2 = remove_push(DeviceId, State),
    {noreply, State2};

object_async_op({?MODULE, remove_push_devices}, State) ->
    State2 = remove_push_devices(State),
    {noreply, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({?MODULE, expired_notify, NotifyId}, State) ->
    lager:warning("NKLOG Expired ~p", [NotifyId]),
    State2 = do_remove_notification(NotifyId, timeout, State),
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
            State4 = update_presence(Path, Type, State3),
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
            Iters = nkdomain_app:get(user_password_pbkdf2_iters),
            {ok, Pbkdf2} = pbkdf2:pbkdf2(sha, Pass2, Salt, Iters),
            Hash = nklib_util:lhash(Pbkdf2),
            <<"NKD!!", Hash/binary, "!">>
    end.


%% @private
check_email(SrvId, #{?DOMAIN_USER:=#{email:=Email}}=Obj) ->
    Email2 = nklib_util:to_lower(Email),
    Spec = #{
        size => 0,
        filters => #{type=>?DOMAIN_USER, << ?DOMAIN_USER/binary, ".email">> => Email2}
    },
    case nkdomain:search(SrvId, Spec) of
        {ok, 0, _, _} ->
            {ok, Obj#{aliases=>Email2}};
        {ok, _, _, _} ->
            {error, {email_duplicated, Email2}};
        {error, Error} ->
            {error, Error}
    end;

check_email(_SrvId, Obj) ->
    {ok, Obj}.


%% @private
update_presence(DomainPath, Type, State) ->
    #?STATE{id=Id, session=Session} = State,
    #obj_id_ext{obj_id=UserId} =Id,
    #session{user_sessions=UserSessions, user_session_presence=PresFuns} = Session,
    case maps:find(Type, PresFuns) of
        {ok, Fun} ->
            Presence = [
                P || #user_session{type=T, domain_path=D, presence=P}
                      <- UserSessions, T==Type andalso D==DomainPath
            ],
            {ok, Event} = Fun(UserId, Presence),
            do_event({presence_updated, DomainPath, Type, Event}, State);
        error ->
            State
    end.


%% @private
find_session(SessId, #?STATE{session=Session}) ->
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
    update_presence(DomainPath, Type, State4).


%% @private
rm_session(SessId, #?STATE{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    case lists:keytake(SessId, #user_session.id, UserSessions) of
        {value, #user_session{pid=Pid}, UserSessions2} ->
            State2 = nkdomain_obj:links_remove(usage, {?MODULE, session, SessId, Pid}, State),
            Session2 = Session#session{user_sessions=UserSessions2},
            State2#?STATE{session=Session2};
        false ->
            State
    end.


%% @private
store_session(#user_session{id=SessId, type=Type}=UserSession, Opts, #?STATE{session=Session}=State) ->
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
    State#?STATE{session=Session2}.


%% @private
export_session(#user_session{id=Id, type=Type, opts=Opts, pid=Pid}) ->
    #{
        session_id => Id,
        type => Type,
        opts => Opts,
        pid => Pid
    }.


%% @doc
do_remove_notification(TokenId, Reason, #?STATE{session=Session}=State) ->
    #session{notify_tokens=Msgs1} = Session,
    case lists:keytake(TokenId, #notify_token.token_id, Msgs1) of
        {value, #notify_token{}=Notify, Msgs2} ->
            notify_token_sessions(Notify, {removed, Reason}, State),
            Session2 = Session#session{notify_tokens=Msgs2},
            State#?STATE{session=Session2};
        false ->
            State
    end.


%% @private
notify_token_sessions(#notify_token{domain_path=DomainPath, session_type=Type, token_id=TokenId, data=Data}, Op, State) ->
    #?STATE{session=#session{user_sessions=UserSessions, user_session_notify=NotFuns}} = State,
    case maps:find(Type, NotFuns) of
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
                    ?LLOG(notice, "no ~p session found: send wakeup", [Type], State),
                    Push = #{
                        type => simple,
                        title => <<"New user notification">>,
                        body => <<"Wake up">>
                    },
                    AppId = <<"user_notifications">>,
                    send_push(any, self(), AppId, Push);
                {removed, Reason} ->
                    ?LLOG(notice, "~s session notification removed: ~p", [Type, Reason]),
                    Push = #{
                        type => remove
                    },
                    AppId = <<"user_notifications">>,
                    send_push(any, self(), AppId, Push)
            end;
        error ->
            ok
    end.



%% @private
do_launch_session_tokens(#user_session{domain_path=DomainPath, type=Type, pid=Pid}, State) ->
    #?STATE{session=#session{user_session_notify=NotFuns}} = State,
    case maps:find(Type, NotFuns) of
        {ok, Fun} ->
            #?STATE{session=#session{notify_tokens=Msgs}} = State,
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
        app_id := AppId,
        device_id := DeviceId,
        push_data := Data,
        updated_time := Time
    } = Push,
    PushDevice = #push_device{
        domain_path = DomainPath,
        app_id = AppId,
        device_id = DeviceId,
        push_data = Data,
        updated_time = Time
    },
    do_load_push(Rest, [PushDevice|Acc]).


%% @private
do_save_push([], Acc) ->
    Acc;

do_save_push([PushDevice|Rest], Acc) ->
    #push_device{
        device_id = DeviceId,
        app_id = AppId,
        domain_path = DomainPath,
        push_data = Data,
        updated_time = Time
    } = PushDevice,
    Push = #{
        device_id => DeviceId,
        app_id => AppId,
        domain_path => DomainPath,
        push_data => Data,
        updated_time => Time
    },
    do_save_push(Rest, [Push|Acc]).



%% @private
add_push(DomainPath, AppId, DeviceId, PushData, State) ->
    #?STATE{session=Session} = State,
    #session{push_devices=PushDevices1} = Session,
    Now =nkdomain_util:timestamp(),
    PushDevice = case lists:keyfind(DeviceId, #push_device.device_id, PushDevices1) of
        #push_device{push_data=PushData} = PD ->
            PD#push_device{
                updated_time=Now
            };
        _ ->
            #push_device{
                domain_path = DomainPath,
                app_id = AppId,
                device_id = DeviceId,
                push_data = PushData,
                updated_time = Now
            }
    end,
    PushDevices2 = lists:keystore(DeviceId, #push_device.device_id, PushDevices1, PushDevice),
    Session2 = Session#session{push_devices = PushDevices2},
    State#?STATE{session=Session2, is_dirty=true}.


%% @private
remove_push(DeviceId, State) ->
    #?STATE{session=Session} = State,
    #session{push_devices=PushDevices1} = Session,
    PushDevices2 = lists:keydelete(DeviceId, #push_device.device_id, PushDevices1),
    Session2 = Session#session{push_devices = PushDevices2},
    State#?STATE{session=Session2, is_dirty=true}.


%% @private
remove_push_devices(State) ->
    #?STATE{session=Session} = State,
    Session2 = Session#session{push_devices=[]},
    State#?STATE{session=Session2, is_dirty=true}.


%% @doc
send_push(AppId, Push, #?STATE{srv_id=SrvId}=State) ->
    Devices = find_push_devices(AppId, State),
    lists:foreach(
        fun(#push_device{device_id=DeviceId, push_data=PushDevice}) ->
            ?LLOG(notice, "sending PUSH to ~s device ~s: ~p (~p)",
                          [AppId, DeviceId, Push, PushDevice], State),
            SrvId:object_send_push(SrvId, AppId, DeviceId, PushDevice, Push)
        end,
        Devices).


%% @private
find_push_devices(AppId, State) ->
    #?STATE{session=Session} = State,
    #session{push_devices=Devices} = Session,
    [Device || #push_device{app_id=A}=Device <- Devices, A==AppId].


%% @private
do_load_status([], Acc) ->
    Acc;

do_load_status([Status|Rest], Acc) ->
    #{
        domain_path := DomainPath,
        app_id := AppId,
        user_status := UserStatus,
        updated_time := Time
    } = Status,
    Status2 = #status{
        domain_path = DomainPath,
        app_id = AppId,
        user_status = UserStatus,
        updated_time= Time
    },
    do_load_status(Rest, [Status2|Acc]).


%% @private
do_save_status([], Acc) ->
    Acc;

do_save_status([Status|Rest], Acc) ->
    #status{
        app_id = AppId,
        domain_path = DomainPath,
        user_status = UserStatus,
        updated_time= Time
    } = Status,
    Status2 = #{
        app_id => AppId,
        domain_path => DomainPath,
        user_status => UserStatus,
        updated_time => Time
    },
    do_save_status(Rest, [Status2|Acc]).



%% @private
do_set_status(DomainPath, AppId, UserStatus, #?STATE{session=Session}=State) ->
    #session{statuses=Statuses} = Session,
    Now = nkdomain_util:timestamp(),
    Status = #status{
        app_id = AppId,
        domain_path = DomainPath,
        user_status = UserStatus,
        updated_time= Now
    },
    Statuses2 = lists:keystore(AppId, #status.app_id, Statuses, Status),
    Session2 = Session#session{statuses=Statuses2},
    State2 = do_event({status_updated, DomainPath, AppId, UserStatus}, State),
    State2#?STATE{session=Session2, is_dirty=true}.


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).
