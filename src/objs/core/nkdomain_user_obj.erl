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

-export([create/3, login/3, token/3, check_token/1, check_user_token/2, get_name/2, send_push/3]).
-export([object_get_info/0, object_admin_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([user_pass/1]).
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


-type login_opts() ::
    #{
        session_id => binary(),
        session_type => module(),
        domain_id => nkdomain:obj_id(),
        password => binary(),
        api_server_pid => pid(),
        local => binary(),
        remote => binary(),
        login_meta => map()         % Meta coming from API meta field
    }.


%% ===================================================================
%% API
%% ===================================================================

%% @doc
%%create(Srv, Domain, Name, SurName, Email) ->
%%    create(root, Name, #{type=>?DOMAIN_USER, parent_id=>Domain, ?DOMAIN_USER=>#{name=>Name, surname=>SurName}}).



%% @doc
-spec create(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, Name, Obj) ->
    #{?DOMAIN_USER:=User} = Obj,
    Obj2 = case User of
        #{email:=Email} ->
            Obj#{aliases=>Email};
        #{<<"email">>:=Email} ->
            Obj#{aliases=>Email};
        _ ->
            Obj
    end,
    nkdomain_obj_lib:make_and_create(Srv, Name, Obj2, #{}).


%% @doc
-spec login(nkservice:id(), User::binary(), login_opts()) ->
    {ok, UserId::nkdomain:obj_id(), SessId::nkdomain:obj_id()} |
    {error, user_not_found|term()}.

login(SrvId, Login, Opts) ->
    case do_load(SrvId, Login) of
        {ok, ObjId, UserPid} ->
            case do_check_pass(UserPid, ObjId, Opts) of
                {ok, UserObjId} ->
                    case do_start_session(SrvId, UserObjId, Opts) of
                        {ok, SessId} ->
                            Meta = maps:get(login_meta, Opts, #{}),
                            send_login_event(UserPid, SessId, Meta),
                            {ok, UserObjId, SessId};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec token(nkservice:id(), User::binary(), login_opts()) ->
    {ok, #{token_id=>Token::nkdomain:obj_id(), ttl=>integer()}} | {error, user_not_found|term()}.

token(SrvId, Login, Opts) ->
    case do_load(SrvId, Login) of
        {ok, ObjId, UserPid} ->
            case do_check_pass(UserPid, ObjId, Opts) of
                {ok, UserObjId} ->
                    do_start_token(SrvId, UserObjId, Opts);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
check_token(Token) ->
    case nkdomain_obj:get_obj(Token) of
        {ok, #{type:=?DOMAIN_SESSION, ?DOMAIN_SESSION:=Data}} ->
            case Data of
                #{user_id:=UserId, domain_id:=DomainId, login_meta:=Meta} ->
                    UserMeta1 = #{login_meta=>Meta},
                    UserMeta2 = nkdomain_api_util:add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
                    UserMeta3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, UserMeta2),
                    UserMeta4 = nkdomain_api_util:add_id(?DOMAIN_SESSION, Token, UserMeta3),
                    {ok, UserId, UserMeta4};
                _ ->
                    {error, invalid_session}
            end;
        {ok, #{type:=?DOMAIN_TOKEN, subtype:=SubTypes, ?DOMAIN_TOKEN:=Data}} ->
            case lists:member(?DOMAIN_USER, SubTypes) andalso Data of
                #{user_id:=UserId, domain_id:=DomainId, login_meta:=Meta} ->
                    UserMeta1 = #{login_meta=>Meta},
                    UserMeta2 = nkdomain_api_util:add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
                    UserMeta3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, UserMeta2),
                    {ok, UserId, UserMeta3};
                _ ->
                    {error, invalid_token}
            end;
        _ ->
            {error, invalid_token}
    end.


%% @doc
check_user_token(SrvId, Token) ->
    case catch base64:decode(Token) of
        Bin when is_binary(Bin) ->
            case binary:split(Bin, <<":">>) of
                [Login, Pass] ->
                    case do_load(SrvId, Login) of
                        {ok, ObjId, UserPid} ->
                            case do_check_pass(UserPid, ObjId, #{password=>Pass}) of
                                {ok, UserObjId} ->
                                    {ok, UserObjId};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    {error, invalid_token}
            end;
        _ ->
            {error, invalid_token}
    end.


%% @doc
-spec get_name(nkservice:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_name(Srv, Id) ->
    sync_op(Srv, Id, {?MODULE, get_name}).


%% @doc
-spec send_push(nkservice:id(), nkdomain:id(), nkevent:event()) ->
    ok | {error, term()}.

send_push(Srv, Id, Event) ->
    lager:error("SEND USER PUSH"),
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:async_op(Pid, {?MODULE, send_push, Event});
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_USER
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 100,
        tree_id => <<"domain_tree_resources_users">>,
        detail_table => fun nkdomain_user_obj_ui:table/0
    }.


%% @private
object_mapping() ->
    #{
        name => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        surname => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        email => #{type => keyword},
        password => #{type => keyword},
        avatar_t => #{type => binary, store => true},
        phone_t => #{type => keyword},
        address_t => #{type => text}
    }.


%% @private
object_parse(_SrvId, update, _Obj) ->
    #{
        name => binary,
        surname => binary,
        password => fun ?MODULE:user_pass/1,
        email => binary,
        avatar_t => binary,
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
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_send_event(Event, Session) ->
    nkdomain_user_obj_events:event(Event, Session).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_user_obj_api:cmd(Cmd, Req).


%% @private
%% It will return 'object_is_disabled' for disabled users
object_sync_op({?MODULE, check_pass, Pass}, _From, #?NKOBJ{obj=Obj}=Session) ->
    case Obj of
        #{?DOMAIN_USER:=#{password:=Pass}} ->
            {reply, {ok, true}, Session};
        _ ->
            {reply, {ok, false}, Session}
    end;

object_sync_op({?MODULE, get_name}, _From, #?NKOBJ{obj=Obj}=Session) ->
    Base = nkdomain_obj_util:get_name(Session),
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
    {reply, {ok, Data}, Session};

object_sync_op(_Op, _From, _Session) ->
    continue.


%%object_async_op({?MODULE, send_push, Event}, Session) ->
%%    ?LLOG(notice, "sending push: ~p", [Event], Session),
%%    {noreply, Session};

object_async_op(_Op, _Session) ->
    continue.





%% ===================================================================
%% Internal
%% ===================================================================

%% @private
sync_op(Srv, Id, Op) ->
    nkdomain_obj_lib:sync_op(Srv, Id, ?DOMAIN_USER, Op, user_not_found).


%% @private
do_load(SrvId, Login) ->
    case nkdomain_obj_lib:load(SrvId, Login, #{}) of
        #obj_id_ext{type = ?DOMAIN_USER, obj_id=ObjId, pid=Pid} ->
            {ok, ObjId, Pid};
        _ ->
            case SrvId:object_store_find_alias(SrvId, Login) of
                {ok, N, [{?DOMAIN_USER, ObjId, _Path}|_]}->
                    case N > 1 of
                        true ->
                            ?LLOG(notice, "duplicated alias for ~s", [Login]);
                        false ->
                            ok
                    end,
                    case nkdomain_obj_lib:load(SrvId, ObjId, #{}) of
                        #obj_id_ext{type = ?DOMAIN_USER, obj_id=ObjId, pid=Pid} ->
                            {ok, ObjId, Pid};
                        _ ->
                            {error, user_not_found}
                    end;
                _ ->
                    {error, user_not_found}
            end
    end.


%% @private
do_check_pass(Pid, ObjId, #{password:=Pass}) ->
    {ok, Pass2} = user_pass(Pass),
    case nkdomain_obj:sync_op(Pid, {?MODULE, check_pass, Pass2}) of
        {ok, true} ->
            {ok, ObjId};
        {ok, false} ->
            timer:sleep(?INVALID_PASSWORD_TIME),
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end;

do_check_pass(_Pid, _ObjId, _Opts) ->
    {error, invalid_password}.


%% @private
do_start_session(SrvId, UserId, Opts) ->
    Opts2 = maps:with([session_id, domain_id, local, remote, login_meta, api_server_pid], Opts),
    case nkdomain_session_obj:create(SrvId, UserId, Opts2) of
        {ok, #{obj_id:=ObjId}, _Pid} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_start_token(SrvId, UserId, Opts) ->
    Opts2 = maps:with([domain_id, local, remote, login_meta], Opts),
    case nkdomain_token_obj:create_referred(SrvId, UserId, maps:with([ttl], Opts), Opts2#{user_id=>UserId}) of
        {ok, Reply, _Pid} ->
            {ok, Reply};
        {error, Error} ->
            {error, Error}
    end.




%% @private
send_login_event(Pid, SessId, Meta) ->
    nkdomain_obj:send_event(Pid, {login, SessId, Meta}).


%% @doc Generates a password from an user password or hash
-spec user_pass(string()|binary()) ->
    {ok, binary()}.

user_pass(Pass) ->
    Pass2 = nklib_util:to_binary(Pass),
    case binary:split(Pass2, <<"!">>, [global]) of
        [<<"NKD">>, <<>>, P, <<>>] when byte_size(P) > 10 ->
            {ok, Pass2};
        _ ->
            {ok, make_pass(Pass2)}
    end.


%% @doc Generates a password from an user password
-spec make_pass(string()|binary()) ->
    binary().

make_pass(Pass) ->
    Pass2 = nklib_util:to_binary(Pass),
    Salt = <<"netcomposer">>,
    Iters = nkdomain_app:get(user_password_pbkdf2_iters),
    {ok, Pbkdf2} = pbkdf2:pbkdf2(sha, Pass2, Salt, Iters),
    Hash = nklib_util:lhash(Pbkdf2),
    <<"NKD!!", Hash/binary, "!">>.

