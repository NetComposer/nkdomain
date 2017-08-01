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
-export([register_session/6, unregister_session/3, update_status/4]).
-export([add_notification_op/5]).
-export([add_push_device/6, remove_push_device/3]).

-export_type([events/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).

-define(INVALID_PASSWORD_TIME, 500).

-define(MAX_EXPIRE, 60).    % Secs

%% ===================================================================
%% Types
%% ===================================================================

-type events() ::
    {login, SessId::binary(), Meta::map()}.

-type auth_opts() :: #{password=>binary()}.

-type sess_opts() ::
    #{
        notify_fun => notify_fun(),
        term() => term()
    }.

-type notify_opts() ::
    #{
    }.

-type notify_fun() ::
    fun((SessId::nkdomain:obj_id(), Pid::pid(), TokenId::nkdomain:obj_id(), Data::map(), created|removed) -> ok).

-type push_data() ::
    #{
        push_id => binary(),
        platform_id => binary(),
        platform_version => binary()
    }.

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
        {ok, TokenId, TTL, _Unknown} ->
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

register_session(SrvId, Id, DomainId, Type, SessId, Opts) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, register_session, DomainId, Type, SessId, Opts, self()}).


%% @doc
unregister_session(SrvId, Id, SessId) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, unregister_session, SessId}).


%% @doc
update_status(SrvId, Id, SessId, Status) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, update_status, SessId, Status}).


%% @doc Adds notification info to a token
%% To generate a notification, make a token child of this user with the information this function returns
%% See nkchat_session_obj:send_invitation() for a sample
%% If the user object dies, the token will reload it automatically

-spec add_notification_op(nkservice:id(), nkdomain:id(), nkdomain:type(), notify_opts(), map()) ->
    {ok, UserId::nkdomain:obj_id(), map()} | {error, term()}.

add_notification_op(SrvId, Id, SessType, Opts, Base) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, add_notification_op, SessType, Opts, Base}).


%%%% @doc
%%-spec remove_notification(nkservice:id(), nkdomain:obj_id(), nkdomain:obj_id()) ->
%%    ok | {error, term()}.
%%
%%remove_notification(SrvId, Id, NotifyId) ->
%%    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, remove_notification, NotifyId}).


%% @doc
add_push_device(SrvId, Id, DomainId, Type, DeviceId, PushData) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, add_push_device, DomainId, Type, DeviceId, PushData}).


%% @doc
remove_push_device(SrvId, Id, DeviceId) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, remove_push_device, DeviceId}).


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
    domain_id :: nkdomain:obj_id(),
    opts = #{} :: sess_opts(),
    status = <<>> :: binary(),
    status_time = 0 :: nkdomain:timestamp(),
    notify_fun :: fun(),
    pid :: pid()
}).

-record(notify_msg, {
    token_id :: binary(),
    domain_id :: nkdomain:obj_id(),
    session_type :: binary(),
    data :: map()
    %created_time :: integer(),
    %expires_time :: binary(),
    %timer :: reference()
}).

-record(push_device, {
    session_type :: binary(),
    domain_id :: nkdomain:obj_id(),
    device_id :: binary(),
    push_data :: push_data(),
    updated_time :: binary()
}).


-record(session, {
    user_sessions = [] :: [#user_session{}],
    notify_msgs = [] :: [#notify_msg{}],
    push_devices = [] :: [#push_device{}]
}).



%% @private
object_info() ->
    #{
        type => ?DOMAIN_USER
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
        name => #{type => text},
        name_norm => #{type => text},
        name_sort => #{type => keyword},
        surname => #{type => text},
        surname_norm =>  #{type => text},
        surname_sort =>  #{type => keyword},
        email => #{type => keyword},
        password => #{type => keyword},
        phone_t => #{type => keyword},
        address_t => #{type => text},
        push => #{
            type => object,
            dynamic => false,
            properties => #{
                domain_id => #{type => keyword},
                session_type => #{type => keyword},
                device_id => #{type => keyword},
                push_data => #{enabled => false},
                updated_time => #{type => date}
            }
        }
    }.


%% @private
object_es_unparse(_SrvId, Obj, Base) ->
    FullName = maps:get(name, Obj),
    User = maps:get(?DOMAIN_USER, Obj),
    Name = maps:get(name, User, <<>>),
    NameNorm = nkdomain_store_es_util:normalize(Name),
    nklib_parse:normalize(Name),
    SurName = maps:get(surname, User, <<>>),
    SurNameNorm = nkdomain_store_es_util:normalize(SurName),
    UserKeys = maps:keys(object_es_mapping()),
    UserMap = maps:with(UserKeys, User),
    UserMap2 = UserMap#{
        name_norm => NameNorm,
        name_sort => NameNorm,
        surname_norm => SurNameNorm,
        surname_sort => SurNameNorm
    },
    FullNameNorm = nkdomain_store_es_util:normalize(FullName),
    Base#{
        name => FullName,
        name_norm => FullNameNorm,
        ?DOMAIN_USER => UserMap2
    }.


%% @private
object_parse(_SrvId, update, _Obj) ->
    #{
        name => binary,
        surname => binary,
        password => fun ?MODULE:fun_user_pass/1,
        email => lower,
        phone_t => binary,
        address_t => binary,
        push => {list,
            #{
                domain_id => binary,
                session_type => binary,
                device_id => binary,
                push_data => map,
                updated_time => integer,
                '__mandatory' => [domain_id, session_type, device_id, push_data, updated_time]
             }
        }
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
    ObjData = #session{},
    State2 = set_session(ObjData, State),
    Push = maps:get(push, User, []),
    State3 = load_push(Push, State2),
    {ok, State3}.


%% @private Prepare the object for saving
object_save(#?STATE{obj=Obj, session=Session}=State) ->
    #session{notify_msgs=_Msgs, push_devices=Devices} = Session,
    Push = lists:map(
        fun({DeviceId, PushDevice}) ->
            #push_device{
                domain_id = DomainId,
                session_type = Type,
                device_id = DeviceId,
                push_data = Data,
                updated_time = Time
            } = PushDevice,
            #{
                domain_id => DomainId,
                session_type => Type,
                device_id => DeviceId,
                push_data => Data,
                updated_time => Time
            }
        end,
        Devices),
    #{?DOMAIN_USER:=User} = Obj,
    User2 = User#{push=>Push},
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
        email => maps:get(email, User, <<>>),
        phone_t => maps:get(phone_t, User, <<>>),
        address_t => maps:get(address_t, User, <<>>)
    },
    {reply, {ok, Data}, State};

object_sync_op({?MODULE, register_session, DomainId, Type, SessId, Opts, Pid}, _From, State) ->
    case find_session(SessId, State) of
        {ok, _} ->
            State2 = rm_session(SessId, State),
            {reply, ok, add_session(DomainId, Type, SessId, Opts, Pid, State2)};
        not_found ->
            {reply, ok, add_session(DomainId, Type, SessId, Opts, Pid, State)}
    end;

object_sync_op({?MODULE, get_sessions}, _From, State) ->
    #session{user_sessions=UserSessions} = get_obj_session(State),
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions),
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, get_sessions, Type}, _From, State) ->
    #session{user_sessions=UserSessions1} = get_obj_session(State),
    UserSessions2 = [US || #user_session{type=T}=US <- UserSessions1, T==Type],
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions2),
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, add_notification_op, SessType, _Opts, Base}, _From, State) ->
    #?STATE{id=#obj_id_ext{obj_id=UserId}} = State,
    UserData1 = maps:get(?DOMAIN_USER, Base, #{}),
    UserData2 = UserData1#{
        <<"notification">> => #{
            <<"user_id">> => UserId,
            <<"session_type">> => SessType
        }
    },
    {reply, {ok, UserId, Base#{?DOMAIN_USER=>UserData2}}, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, unregister_session, SessId}, State) ->
    State2 = rm_session(SessId, State),
    {noreply, State2};

object_async_op({?MODULE, update_status, SessId, Status}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{type=Type}=UserSession} ->
            Now = nkdomain_util:timestamp(),
            UserSession2 = UserSession#user_session{status=Status, status_time=Now},
            State2 = nkdomain_obj_util:event({session_status_updated, Type, SessId, Status}, State),
            State3 = store_session(UserSession2, State2),
            {noreply, State3};
        not_found ->
            {noreply, State}
    end;

object_async_op({?MODULE, loaded_token, TokenId, Pid}, State) ->
    case nkdomain_token_obj:get_token_data(any, Pid) of
        {ok, #{domain_id:=DomainId, data:=Data}} ->
            case Data of
                #{?DOMAIN_USER:=#{<<"notification">>:=Notification}} ->
                    ?LLOG(notice, "detected notification token ~s", [TokenId], State),
                    #{
                        <<"session_type">> := SessType
                    } = Notification,
                    Notify = #notify_msg{
                        token_id = TokenId,
                        domain_id = DomainId,
                        session_type = SessType,
                        data = Data
                    },
                    #?STATE{session=Session1} = State,
                    #session{notify_msgs=Msgs} = Session1,
                    Session2 = Session1#session{notify_msgs=[Notify|Msgs]},
                    % Add usage so that the object is not unloaded
                    State2 = State#?STATE{session=Session2},
                    notify_sessions(Notify, created, State2),
                    {noreply, State2};
                _ ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;

object_async_op({?MODULE, unloaded_token, TokenId}, State) ->
    ?LLOG(notice, "unloaded notification token ~s", [TokenId], State),
    State2 = remove_notification(TokenId, State),
    {noreply, State2};

object_async_op({?MODULE, add_push_device, DomainId, Type, DeviceId, PushData}, State) ->
    State2 = add_push(DomainId, Type, DeviceId, PushData, State),
    {noreply, State2};

object_async_op({?MODULE, remove_push_device, DeviceId}, State) ->
    State2 = remove_push(DeviceId, State),
    {noreply, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({?MODULE, expired_notify, NotifyId}, State) ->
    lager:warning("NKLOG Expired ~p", [NotifyId]),
    State2 = remove_notification(NotifyId, State),
    {noreply, State2};

object_handle_info(_Info, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, session, SessId, _Pid}}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{type=Type}} ->
            State2 = nkdomain_obj_util:event({session_stopped, Type, SessId}, State),
            ?DEBUG("registered session down: ~s", [SessId], State2),
            {ok, rm_session(SessId, State2)};
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
find_session(SessId, State) ->
    #session{user_sessions=UserSessions} = get_obj_session(State),
    case lists:keyfind(SessId, #user_session.id, UserSessions) of
        #user_session{} = UserSession ->
            {ok, UserSession};
        false ->
            not_found
    end.


%% @private
add_session(DomainId, Type, SessId, Opts, Pid, State) ->
    Fun = maps:get(notify_fun, Opts, none),
    UserSessionTuple = #user_session{
        id = SessId,
        domain_id = DomainId,
        type = Type,
        opts = Opts,
        notify_fun = Fun,
        pid = Pid
    },
    UserSession = UserSessionTuple,
    State2 = nkdomain_obj:links_add(usage, {?MODULE, session, SessId, Pid}, State),
    State3 = nkdomain_obj_util:event({session_started, Type, SessId}, State2),
    State4 = store_session(UserSession, State3),
    notify_session(DomainId, Type, SessId, Pid, Fun, State4).


%% @private
rm_session(SessId, State) ->
    #session{user_sessions=UserSessions} = Session = get_obj_session(State),
    case lists:keytake(SessId, #user_session.id, UserSessions) of
        {value, #user_session{pid=Pid}, UserSessions2} ->
            State2 = nkdomain_obj:links_remove(usage, {?MODULE, session, SessId, Pid}, State),
            Session2 = Session#session{user_sessions=UserSessions2},
            set_session(Session2, State2);
        error ->
            State
    end.


%% @private
store_session(#user_session{id=SessId}=UserSession, State) ->
    #session{user_sessions=UserSessions} = Session = get_obj_session(State),
    UserSessions2 = lists:keystore(SessId, #user_session.id, UserSessions, UserSession),
    Session2 = Session#session{user_sessions=UserSessions2},
    set_session(Session2, State).


%% @private
export_session(#user_session{id=Id, type=Type, status=Status, opts=Opts, status_time=Time, pid=Pid}) ->
    #{
        session_id => Id,
        type => Type,
        status => Status,
        status_time => Time,
        opts => Opts,
        pid => Pid
    }.


%% @doc
remove_notification(TokenId, #?STATE{session=Session}=State) ->
    #session{notify_msgs=Msgs1} = Session,
    case lists:keytake(TokenId, #notify_msg.token_id, Msgs1) of
        {value, #notify_msg{}=Notify, Msgs2} ->
            notify_sessions(Notify, removed, State),
            Session2 = Session#session{notify_msgs=Msgs2},
            State#?STATE{session=Session2};
        false ->
            State
    end.


%% @private
notify_sessions(#notify_msg{domain_id=DomainId, session_type=Type, token_id=TokenId, data=Data}, Op, State) ->
    #session{user_sessions=UserSessions} = get_obj_session(State),
    Num = lists:foldl(
        fun(#user_session{id=SessId, pid=Pid, notify_fun=Fun, type=T, domain_id=D}, Acc) ->
            case T==Type andalso D==DomainId andalso is_function(Fun, 5) of
                true ->
                    Fun(SessId, Pid, TokenId, Data, Op),
                    Acc+1;
                false ->
                    Acc
            end
        end,
        0,
        UserSessions),
    case Num > 0 of
        true ->
            ok;
        false when Op==created ->
            lager:error("NKLOG SEND PUSH ~p", [Data]),
            send_push(DomainId, Type, Data, State);
        false when Op==removed ->
            lager:error("NKLOG SEND PUSH REMOVED ~p", [Data])
    end.


%% @private
notify_session(DomainId, Type, SessId, Pid, Fun, State) when is_function(Fun, 5) ->
    #?STATE{session=#session{notify_msgs=Msgs}} = State,
    lists:foreach(
        fun(#notify_msg{token_id=Id, data=Data, session_type=T, domain_id=D}) ->
            case T==Type andalso D==DomainId of
                true ->
                    Fun(SessId, Pid, Id, Data, created);
                false ->
                    ok
            end
        end,
        Msgs),
    State;

notify_session(_DomainId, _Type, _SessId, _Pid, _Fun, State) ->
    State.


%% @private
load_push(Push, #?STATE{session=Session}=State) ->
    PushDevices = do_load_push(Push, []),
    Session2 = Session#session{push_devices=PushDevices},
    State#?STATE{session=Session2}.


%% @private
do_load_push([], Acc) ->
    Acc;

do_load_push([Push|Rest], Acc) ->
    #{
        domain_id := DomainId,
        session_type := Type,
        device_id := DeviceId,
        push_data := Data,
        updated_time := Time
    } = Push,
    PushDevice = #push_device{
        domain_id = DomainId,
        session_type = Type,
        device_id = DeviceId,
        push_data = Data,
        updated_time = Time
    },
    do_load_push(Rest, [PushDevice|Acc]).



%% @private
add_push(DomainId, Type, DeviceId, PushData, State) ->
    #?STATE{session=Session} = State,
    #session{push_devices=PushDevices1} = Session,
    PushDevice = #push_device{
        domain_id = DomainId,
        session_type = Type,
        device_id = DeviceId,
        push_data = PushData,
        updated_time = nkdomain_util:timestamp()
    },
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


%% @doc
send_push(DomainId, Type, Push, #?STATE{session=Session}) ->
    #session{push_devices=Devices} = Session,
    lists:foreach(
        fun(#push_device{domain_id=D, session_type=T, device_id=Device, push_data=PushData}) ->
            case D==DomainId andalso T==Type of
                true ->
                    lager:warning("NKLOG Sending PUSH to ~s:~s device ~s: ~p (~p)",
                                  [DomainId, Type, Device, Push, PushData]);
                false ->
                    ok
            end
        end,
        Devices).



%% @private
get_obj_session(State) ->
    nkdomain_obj_util:get_obj_session(State).


%% @private
set_session(Data, State) ->
    nkdomain_obj_util:set_obj_session(Data, State).
