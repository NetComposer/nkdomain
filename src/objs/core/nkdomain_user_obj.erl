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

-export([object_execute/5, object_schema/1, object_query/3, object_mutation/3]).
-export([object_info/0, object_admin_info/0, object_create/1, object_update/1, object_es_mapping/0, object_es_unparse/2,

         object_parse/2, object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export([object_init/1, object_save/1, object_event/2,
         object_sync_op/3, object_async_op/2, object_link_down/2, object_handle_info/2]).
-export([fun_user_pass/1]).


-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).

-define(USER_PASS_ITERS, 10).

%-define(MAX_EXPIRE, 60).    % Secs


%% ===================================================================
%% API
%% ===================================================================




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(user_session, {
    id :: nkdomain:obj_id(),
    type :: nkdomain:type(),
    domain_path :: nkdomain:path(),
    presence :: nkdomain_user:session_presence(),
    opts = #{} :: nkdomain_user:sess_opts(),
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
    push_data :: nkdomain_user:push_device_data(),
    updated_time :: binary()
}).

-record(status, {
    id :: {nkservice:id(), nkdomain:path()},
    user_status :: nkdomain_user:user_status(),
    updated_time :: binary()
}).

-record(session, {
    user_sessions = [] :: [#user_session{}],
    user_session_notify = #{} :: #{nkdomain:type() => nkdomain_user:notify_fun()},
    user_session_presence = #{} :: #{nkdomain:type() => nkdomain_user:presence_fun()},
    notify_tokens = [] :: [#notify_token{}],
    push_devices = [] :: [#push_device{}],
    statuses = [] :: [#status{}],
    meta = #{}          % For future use
}).



%% @private
object_info() ->
    #{
        type => ?DOMAIN_USER,
        schema_type => 'User',
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
    case check_email(Obj) of
        {ok, Obj2} ->
            nkdomain_obj_make:create(Obj2#{type=>?DOMAIN_USER});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
object_schema(Type) ->
    nkdomain_user_obj_schema:object_schema(Type).


%% @doc
object_execute(Field, ObjIdExt, #{?DOMAIN_USER:=User}, Args, _Ctx) ->
    nkdomain_user_obj_schema:object_execute(Field, ObjIdExt, User, Args).


%% @doc
object_query(QueryName, Params, Ctx) ->
    nkdomain_user_obj_schema:object_query(QueryName, Params, Ctx).


%% @doc
object_mutation(MutationName, Params, Ctx) ->
    nkdomain_user_obj_schema:object_mutation(MutationName, Params, Ctx).


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
            nkdomain_user:async_op(self(), {loaded_token, TokenId, Pid});
        {child_unloaded, ?DOMAIN_TOKEN, TokenId} ->
            nkdomain_user:async_op(self(), {unloaded_token, TokenId});
        _ ->
            ok
    end,
    {ok, State}.


% @private
object_sync_op({?MODULE, Op}, From, State) ->
    sync_op(Op, From, State);

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, Op}, State) ->
    async_op(Op, State);

object_async_op(_Op, _State) ->
    continue.


%% @private
%%object_handle_info({?MODULE, expired_notify, NotifyId}, State) ->
%%    lager:warning("NKLOG Expired ~p", [NotifyId]),
%%    State2 = do_remove_notification(NotifyId, timeout, State),
%%    {noreply, State2};

object_handle_info({?MODULE, {launch_session_notifications, SessId}}, #obj_state{session=Session}=State) ->
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

% @private
sync_op({check_pass, _Pass}, _From, #obj_state{is_enabled=false}=State) ->
    {reply, {error, object_is_disabled}, State};

sync_op({check_pass, Pass}, _From, #obj_state{id=Id, obj=Obj}=State) ->
    case Obj of
        #{domain_id:=DomainId, ?DOMAIN_USER:=#{password:=Pass}} ->
            #obj_id_ext{obj_id=UserId} = Id,
            {reply, {ok, {true, UserId, DomainId}}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

sync_op({check_device, _Pass}, _From, #obj_state{is_enabled=false}=State) ->
    {reply, {error, object_is_disabled}, State};

sync_op({check_device, DeviceId}, _From, #obj_state{id=Id, obj=Obj}=State) ->
    #obj_id_ext{obj_id=UserId} = Id,
    case nkdomain_device_obj:find_sso(DeviceId) of
        {ok, UserId} ->
            #{domain_id:=DomainId} = Obj,
            {reply, {ok, {true, UserId, DomainId}}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

sync_op({get_info, Opts}, _From, #obj_state{obj=Obj}=State) ->
    Base = nkdomain_obj_util:get_obj_name(State),
    #{name:=UserName, surname:=UserSurName} = User = maps:get(?DOMAIN_USER, Obj),
    Data = Base#{
        name => UserName,
        surname => UserSurName,
        fullname => <<UserName/binary, " ", UserSurName/binary>>,
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
                    case nkdomain_lib:find(Domain) of
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
                            Acc#{Type => #{status => <<"offline">>}}
                    end
                end,
                #{},
                Types),
            Data2#{presence => Presence};
        _ ->
            Data2
    end,
    {reply, {ok, Data3}, State};

sync_op({register_session, DomainPath, Type, SessId, Opts, Pid}, _From, State) ->
    case find_session(SessId, State) of
        {ok, _} ->
            State2 = rm_session(SessId, State),
            {reply, ok, add_session(DomainPath, Type, SessId, Opts, Pid, State2)};
        not_found ->
            State2 = add_session(DomainPath, Type, SessId, Opts, Pid, State),
            {reply, ok, State2}
    end;

sync_op({get_sessions}, _From, #obj_state{session=Session}=State) ->
    #session{user_sessions=UserSessions} = Session,
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions),
    {reply, {ok, Reply}, State};

sync_op({get_sessions, Type}, _From, #obj_state{session=Session}=State) ->
    #session{user_sessions=UserSessions1} = Session,
    UserSessions2 = [US || #user_session{type=T}=US <- UserSessions1, T==Type],
    Reply = lists:map(
        fun(UserSession) -> export_session(UserSession) end,
        UserSessions2),
    {reply, {ok, Reply}, State};

sync_op({add_notification_op, SessType, Opts, Token}, _From, State) ->
    #obj_state{id=#obj_id_ext{obj_id=UserId}} = State,
    UserData1 = maps:get(?DOMAIN_USER, Token, #{}),
    UserNotification1 = #{
        <<"user_id">> => UserId,
        <<"session_type">> => SessType
    },
    UserNotification2 = case Opts of
        #{srv_id:=SrvId, wakeup_push:=Push} ->
            UserNotification1#{<<"srv_id">> => SrvId, <<"wakeup_push">> => Push};
        _ ->
            UserNotification1
    end,
    UserData2 = UserData1#{<<"notification">> => UserNotification2},
    {reply, {ok, UserId, Token#{?DOMAIN_USER=>UserData2}}, State};

sync_op({get_status, SrvId, DomainPath}, _From, State) ->
    Reply = do_get_status(SrvId, DomainPath, State),
    {reply, Reply, State};

sync_op({get_presence, Type, DomainPath}, _From, State) ->
    Reply = do_get_presence(Type, DomainPath, State),
    {reply, Reply, State};

sync_op(_Op, _From, _State) ->
    continue.


%% @private
async_op({unregister_session, SessId}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{domain_path=Path, type=Type}} ->
            State2 = rm_session(SessId, State),
            State3 = do_update_presence(Type, Path, State2),
            {noreply, State3};
        not_found ->
            {noreply, State}
    end;

async_op({update_presence, SessId, Presence}, State) ->
    case find_session(SessId, State) of
        {ok, #user_session{domain_path=Path, type=Type}=UserSession} ->
            UserSession2 = UserSession#user_session{presence=Presence},
            State2 = store_session(UserSession2, #{}, State),
            State3 = do_update_presence(Type, Path, State2),
            {noreply, State3};
        not_found ->
            {noreply, State}
    end;

async_op({launch_session_notifications, _SessId}=Msg, State) ->
    % Reply to client first
    erlang:send_after(1000, self(), {?MODULE, Msg}),
    {noreply, State};

async_op({set_status, SrvId, DomainPath, UserStatus}, State) ->
    State2 = do_set_status(SrvId, DomainPath, UserStatus, State),
    {noreply_and_save, State2};

async_op({loaded_token, TokenId, Pid}, State) ->
    case nkdomain_token_obj:get_token_data(Pid) of
        {ok, #{domain_id:=DomainId, data:=Data}} ->
            case Data of
                #{?DOMAIN_USER:=#{<<"notification">>:=Notification}} ->
                    ?LLOG(info, "detected notification token ~s", [TokenId], State),
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

async_op({unloaded_token, TokenId}, State) ->
    ?LLOG(info, "unloaded notification token ~s", [TokenId], State),
    State2 = do_remove_notification(TokenId, timeout, State),
    {noreply, State2};

async_op({remove_notification, TokenId, Reason}, State) ->
    State2 = do_remove_notification(TokenId, Reason, State),
    {noreply, State2};

async_op({add_push_device, DomainPath, SrvId, DeviceId, PushData}, State) ->
    State2 = add_push(DomainPath, SrvId, DeviceId, PushData, State),
    {noreply, State2};

async_op({send_push, SrvId, Push}, State) ->
    do_send_push(SrvId, Push, State),
    {noreply, State};

async_op({remove_push_device, DeviceId}, State) ->
    State2 = remove_push(DeviceId, State),
    {noreply, State2};

async_op({remove_push_devices}, State) ->
    State2 = do_remove_all_push_devices(State),
    {noreply, State2};

async_op(_Op, _State) ->
    continue.



%% @private
fun_user_pass(Pass) ->
    {ok, nkdomain_user:user_pass(Pass)}.


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
do_update_presence(Type, Path, State) ->
    case do_get_presence(Type, Path, State) of
        {ok, UserPresence} ->
            do_event({presence_updated, Type, Path, UserPresence}, State);
        {error, _Error} ->
            State
    end.


%% @private Gets presence status from all sessions with Domain and Type and call presence_fun() to get current
%% presence status
do_get_presence(Type, Path, State) ->
    #obj_state{id=Id, session=Session} = State,
    #obj_id_ext{obj_id=UserId} =Id,
    #session{user_sessions=UserSessions, user_session_presence=PresFuns} = Session,
    case maps:find(Type, PresFuns) of
        {ok, Fun} ->
            PresenceList = [
                P || #user_session{type=T, domain_path=D, presence=P}
                     <- UserSessions, T==Type andalso D==Path
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
                    notify_wakeup_push(Data, State);
                {removed, Reason} ->
                    ?LLOG(notice, "~s session notification removed: ~p", [Type, Reason])
            end;
        error when Op==created ->
            notify_wakeup_push(Data, State);
        error ->
            ok
    end.

%% @private
notify_wakeup_push(#{?DOMAIN_USER:=#{<<"notification">>:=#{<<"srv_id">>:=Srv, <<"wakeup_push">>:=Push}}}, State) ->
    ?LLOG(notice, "no session found: send wakeup (~p)", [Push], State),
    case nkdomain_obj_util:get_existing_srv_id(Srv) of
        undefined ->
            ?LLOG(notice, "could not send push: unknown service '~s'", [Srv], State);
        SrvId ->
            do_send_push(SrvId, Push, State)
    end;

notify_wakeup_push(S, State) ->
    lager:error("NKLOG NO WAKE ~p", [S]),
    ?LLOG(notice, "no session found (and no wakeup)", [], State).



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
            ?LLOG(info, "sending PUSH (~s): ~p", [SrvId, Push]),
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
