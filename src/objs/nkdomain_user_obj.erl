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

-export([create/4, login/3, get_name/2, send_push/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4,
         object_sync_op/3, object_async_op/2]).
-export([user_pass/1]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================


-type login_opts() ::
    #{
        password => binary(),
        session_id => binary(),
        session_type => module(),
        api_server_pid => pid(),
        local => binary(),
        remote => binary(),
        login_meta => map()
    }.


%% ===================================================================
%% API
%% ===================================================================

%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:id(), nkdomain:name(), map()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, Parent, Name, Data) ->
    Opts = #{
        name => Name,
        type_obj => Data,
        aliases =>
            case Data of
                #{email:=Email} -> Email;
                _ -> []
            end
    },
    nkdomain_obj_lib:make_and_create(Srv, Parent, ?DOMAIN_USER, Opts).


%% @doc
-spec login(nkservice:id(), User::binary(), login_opts()) ->
    {ok, UserId::nkdomain:obj_id(), SessId::nkdomain:obj_id(), map()} |
    {error, user_not_found|term()}.

login(SrvId, Login, Opts) ->
    case do_load(SrvId, Login) of
        {ok, ObjId, UserPid} ->
            case do_login(UserPid, ObjId, Opts) of
                {ok, UserObjId} ->
                    case do_start_session(SrvId, UserObjId, Opts) of
                        {ok, SessId} ->
                            {ok, UserObjId, SessId, #{}};
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
-spec get_name(nkservice:id(), nkdomain:id()) ->
    {ok, map()} | {error, term()}.

get_name(Srv, Id) ->
    sync_op(Srv, Id, {?MODULE, get_name}).


%% @doc 
-spec send_push(nkservice:id(), nkdomain:id(), nkservice_events:event()) ->
    ok | {error, term()}.

send_push(Srv, Id, Event) ->
    lager:error("SEND USER PUSH"),
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:async_op(Pid, {?MODULE, send_push, Event});
        {error, Error} ->
            {error, Error}
    end.







%%%% @doc
%%%% TODO: change
%%find_referred(Srv, Id, Spec) ->
%%    case nkdomain_obj_lib:find(Srv, Id) of
%%        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
%%            SrvId:object_store_find_referred(SrvId, ObjId, Spec);
%%        {error, Error} ->
%%            {error, Error}
%%    end.





%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_USER
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
        password => #{type => keyword}
    }.


%% @private
object_syntax(update) ->
    #{
        name => binary,
        surname => binary,
        password => fun ?MODULE:user_pass/1,
        email => binary
    };

object_syntax(load) ->
    (object_syntax(update))#{
        '__mandatory' => [name, surname]
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_user_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_user_obj_api:cmd(Sub, Cmd, Data, State).


%% @private
%% It will return 'object_is_disabled' for disabled users
object_sync_op({?MODULE, check_pass, Pass}, _From, #obj_session{obj=Obj}=Session) ->
    case Obj of
        #{?DOMAIN_USER:=#{password:=Pass}} ->
            {reply, {ok, true}, Session};
        _ ->
            {reply, {ok, false}, Session}
    end;

object_sync_op({?MODULE, get_name}, _From, Session) ->
    #obj_session{type=Type, path=Path, obj=Obj, obj_id=ObjId} = Session,
    {ok, _, Name} = nkdomain_util:get_parts(Type, Path),
    #{name:=UserName, surname:=UserSurName} = User = maps:get(?DOMAIN_USER, Obj),
    Data = #{
        name => Name,
        description => maps:get(description, Obj, <<>>),
        ?DOMAIN_USER => #{
            obj_id => ObjId,
            name => UserName,
            surname => UserSurName,
            email => maps:get(email, User, <<>>),
            icon_url => <<>>
        }
    },
    {reply, {ok, Data}, Session};

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op({?MODULE, send_push, Event}, Session) ->
    ?LLOG(notice, "sending push: ~p", [Event], Session),
    {noreply, Session};

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
%%    LoadOpts = maps:with([register], Opts),
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
do_login(Pid, ObjId, #{password:=Pass}) ->
    {ok, Pass2} = user_pass(Pass),
    case nkdomain_obj:sync_op(Pid, {?MODULE, check_pass, Pass2}) of
        {ok, true} ->
            {ok, ObjId};
        {ok, false} ->
            {error, invalid_password};
        {error, Error} ->
            {error, Error}
    end;

do_login(_Pid, _ObjId, _Opts) ->
    {error, invalid_password}.


%% @private
do_start_session(SrvId, UserId, Opts) ->
    Opts1 = maps:with([session_id, api_server_pid, local, remote], Opts),
    Opts2 = Opts1#{referred_id=>UserId},
    case nkdomain_session_obj:create(SrvId, UserId, Opts2) of
        {ok, ObjId, _Pid} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.


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

