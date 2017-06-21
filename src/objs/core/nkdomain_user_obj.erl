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

-export([create/2, auth/3, make_token/5, check_token/2, get_name/2]).
-export([object_info/0, object_admin_info/0, object_create/2, object_es_mapping/0, object_es_unparse/3,
         object_parse/3, object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export([object_init/1, object_sync_op/3, object_async_op/2, object_link_down/2]).
-export([fun_user_pass/1, user_pass/1]).
-export([get_sessions/2, get_sessions/3]).
-export([register_session/5, unregister_session/3]).

-export_type([events/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).

-define(INVALID_PASSWORD_TIME, 500).

%% ===================================================================
%% Types
%% ===================================================================

-type events() ::
    {login, SessId::binary(), Meta::map()}.

-type auth_opts() :: #{password=>binary()}.


%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(nkservice:id(), map()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

create(SrvId, Obj) ->
    case check_email(SrvId, Obj) of
        {ok, Obj2} ->
            nkdomain_obj_make:create(SrvId, Obj2#{type=>?DOMAIN_USER});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec auth(nkservice:id(), User::binary(), auth_opts()) ->
    {ok, UserId::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {error, user_not_found|term()}.

auth(SrvId, UserId, #{password:=Pass}) ->
    Pass2 = user_pass(Pass),
    case nkdomain_obj:sync_op(SrvId, UserId, {?MODULE, check_pass, Pass2}) of
        {ok, {true, ObjId}} ->
            {ok, ObjId};
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
    case nkdomain_token_obj:create(SrvId, DomainId, UserId, ?DOMAIN_USER, TokenOpts, TokenData) of
        {ok, TokenId, TTL, _Unknown} ->
            {ok, TokenId, TTL};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
check_token(SrvId, Token) ->
    case nkdomain:get_obj(SrvId, Token) of
        {ok, #{type:=?DOMAIN_SESSION, ?DOMAIN_SESSION:=Data}=Obj} ->
            #{parent_id:=DomainId, created_by:=UserId} = Obj,
            UserMeta1 = #{login_meta=>maps:get(login_meta, Data)},
            UserMeta2 = nkdomain_api_util:add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
            UserMeta3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, UserMeta2),
            UserMeta4 = nkdomain_api_util:add_id(?DOMAIN_SESSION, Token, UserMeta3),
            {ok, UserId, UserMeta4};
        {ok, #{type:=?DOMAIN_TOKEN, subtype:=SubTypes, ?DOMAIN_TOKEN:=Data}=Obj} ->
            case lists:member(?DOMAIN_USER, SubTypes) of
                true ->
                    #{parent_id:=DomainId, created_by:=UserId} = Obj,
                    UserMeta1 = #{login_meta=>maps:get(login_meta, Data)},
                    UserMeta2 = nkdomain_api_util:add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
                    UserMeta3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, UserMeta2),
                    {ok, UserId, UserMeta3};
                _ ->
                    {error, invalid_token}
            end;
        _ ->
            case catch base64:decode(Token) of
                Bin when is_binary(Bin) ->
                    case binary:split(Bin, <<":">>) of
                        [Login, Pass] ->
                            case auth(SrvId, Login, #{password=>Pass}) of
                                {ok, UserId} ->
                                    {ok, UserId, #{}};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        _ ->
                            {error, invalid_token}
                    end;
                _ ->
                    {error, invalid_token}
            end
    end.


%% @doc
-spec get_sessions(nkservice:id(), nkdomain:id()) ->
    {ok, #{nkdomain:obj_id() => session()}} | {error, term()}.

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


%%%% @doc
%%-spec send_push(nkservice:id(), nkdomain:id(), nkevent:event()) ->
%%    ok | {error, term()}.
%%
%%send_push(Srv, Id, Event) ->
%%    lager:error("SEND USER PUSH"),
%%    case nkdomain_lib:load(Srv, Id) of
%%        #obj_id_ext{pid=Pid} ->
%%            nkdomain_obj:async_op(Pid, {?MODULE, send_push, Event});
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc
register_session(SrvId, Id, Type, SessId, Meta) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, register_session, Type, SessId, Meta, self()}).


%% @doc
unregister_session(SrvId, Id, SessId) ->
    nkdomain_obj:async_op(SrvId, Id, {?MODULE, unregister_session, SessId}).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-type session() :: {Type::nkdomain:type(), Meta::map(), pid()}.

-record(obj_data, {
    sessions = #{} :: #{nkdomain:obj_id() => session()}
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
        get_tree_detail => fun nkdomain_user_obj_ui:table/1
    }.

%% @doc
object_create(SrvId, Obj) ->
    create(SrvId, Obj).


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
        address_t => #{type => text}
    }.


%% @private
object_es_unparse(_SrvId, Obj, Base) ->
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
    FullName = nklib_util:bjoin([Name, SurName], <<" ">>),
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
        address_t => binary
    };

object_parse(SrvId, load, Obj) ->
    Base = object_parse(SrvId, update, Obj),
    Base#{'__mandatory' => [name, surname]}.


%% @private
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
object_init(State) ->
    ObjData = #obj_data{},
    {ok, set_obj_data(ObjData, State)}.


%% @private
object_sync_op({?MODULE, check_pass, _Pass}, _From, #?STATE{is_enabled=false}=State) ->
    {reply, {error, object_is_disabled}, State};

object_sync_op({?MODULE, check_pass, Pass}, _From, #?STATE{id=Id, obj=Obj}=State) ->
    case Obj of
        #{?DOMAIN_USER:=#{password:=Pass}} ->
            #obj_id_ext{obj_id=ObjId} = Id,
            {reply, {ok, {true, ObjId}}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

object_sync_op({?MODULE, get_name}, _From, #?STATE{obj=Obj}=State) ->
    Base = nkdomain_obj_util:get_name(State),
    #{name:=UserName, surname:=UserSurName} = User = maps:get(?DOMAIN_USER, Obj),
    Data = Base#{
        ?DOMAIN_USER => #{
            name => UserName,
            surname => UserSurName,
            email => maps:get(email, User, <<>>),
            % avatar_t => maps:get(avatar_t, User, <<>>),
            phone_t => maps:get(phone_t, User, <<>>),
            address_t => maps:get(address_t, User, <<>>)
        }
    },
    {reply, {ok, Data}, State};

object_sync_op({?MODULE, register_session, Type, SessId, Meta, Pid}, _From, State) ->
    #obj_data{sessions=Sessions} = get_obj_data(State),
    case maps:find(SessId, Sessions) of
        error ->
            {reply, ok, add_session(Type, SessId, Meta, Pid, State)};
        {ok, _} ->
            State2 = rm_session(SessId, State),
            {reply, ok, add_session(Type, SessId, Meta, Pid, State2)}
    end;

object_sync_op({?MODULE, get_sessions}, _From, State) ->
    #obj_data{sessions=Sessions} = get_obj_data(State),
    {reply, {ok, Sessions}, State};

object_sync_op({?MODULE, get_sessions, Type}, _From, State) ->
    #obj_data{sessions=Sessions} = get_obj_data(State),
    Sessions2 = [
        {SessId, Meta, Pid} ||
        {SessId, {T, Meta, Pid}} <- maps:to_list(Sessions), T==Type
    ],
    {reply, {ok, Sessions2}, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, unregister_session, SessId}, State) ->
    State2 = rm_session(SessId, State),
    {noreply, State2};

object_async_op(_Op, _State) ->
    continue.


%% @private
object_link_down({usage, {?MODULE, sesison, SessId}}=Link, State) ->
    ?DEBUG("registered session down: ~s", [SessId], State),
    State2 = rm_session(SessId, State),
    {continue, [Link, State2]};

object_link_down(_Link, _State) ->
    continue.



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
    check_email2(SrvId, Email, Obj);

check_email(SrvId, #{?DOMAIN_USER:=#{<<"email">>:=Email}}=Obj) ->
    check_email2(SrvId, Email, Obj);

check_email(_SrvId, Obj) ->
    {ok, Obj}.


%% @private
check_email2(SrvId, Email, Obj) ->
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
    end.


%% @private
add_session(Type, SessId, Meta, Pid, State) ->
    #obj_data{sessions=Sessions} = ObjData = get_obj_data(State),
    Sessions2 = Sessions#{SessId => {Type, Meta, Pid}},
    ObjData2 = ObjData#obj_data{sessions=Sessions2},
    State2 = nkdomain_obj:links_add(usage, {?MODULE, session, SessId, Pid}, State),
    State3 = nkdomain_obj_util:event({session_started, Type, SessId}, State2),
    set_obj_data(ObjData2, State3).


%% @private
rm_session(SessId, State) ->
    #obj_data{sessions=Sessions} = ObjData = get_obj_data(State),
    case maps:find(SessId, Sessions) of
        {ok, {_Type, _Meta, Pid}} ->
            Sessions2 = maps:remove(SessId, Sessions),
            ObjData2 = ObjData#obj_data{sessions=Sessions2},
            State2 = nkdomain_obj:links_remove(usage, {?MODULE, session, SessId, Pid}, State),
            set_obj_data(ObjData2, State2);
        error ->
            State
    end.


%% @private
get_obj_data(State) ->
    nkdomain_obj_util:get_obj_data(State).


%% @private
set_obj_data(Data, State) ->
    nkdomain_obj_util:set_obj_data(Data, State).
