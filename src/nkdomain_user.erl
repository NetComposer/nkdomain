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

-module(nkdomain_user).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([login/4]).
-export([object_get_desc/0, object_get_mapping/0, object_add_load_syntax/1,
         object_add_update_syntax/1, object_store/1]).
-export([user_pass/1]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN User "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

-type login_opts() ::
    #{
        register => nklib:link()
    }.


%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec login(nkservice:id(), User::binary(), Pass::binary(), login_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, user_not_found|term()}.

login(SrvId, Login, Pass, Opts) ->
    case do_load(SrvId, Login, Opts) of
        {ok, ObjId, Pid} ->
            do_login(Pid, ObjId, Pass);
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_desc() ->
    #{
        type => <<"user">>,
        name => <<"user">>
    }.


%% @private
object_get_mapping() ->
    #{
        name => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        surname => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        password => #{type => keyword}
    }.


%% @private
object_add_load_syntax(Base) ->
    Base2 = nkdomain_types:make_syntax(?MODULE, [name, surname], Base),
    Base2#{
        ?MODULE => #{
            name => binary,
            surname => binary,
            password => fun ?MODULE:user_pass/1
        }
    }.


%% @private
object_add_update_syntax(Base) ->
    object_add_load_syntax(Base).


%% @private
object_store(#{?MODULE:=User}) ->
    Keys = maps:keys(object_get_mapping()),
    maps:with(Keys, User);

object_store(_) ->
    #{}.



%% ===================================================================
%% Internal
%% ===================================================================





%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_load(SrvId, Login, Opts) ->
    LoadOpts = maps:with([register], Opts),
    case nkdomain_obj:load(SrvId, Login, LoadOpts) of
        {ok, nkdomain_user, ObjId, Pid} ->
            {ok, ObjId, Pid};
        _ ->
            case SrvId:object_store_find_alias(SrvId, Login) of
                {ok, N, [{nkdomain_user, ObjId}|_]}->
                    case N > 1 of
                        true ->
                            ?LLOG(notice, "duplicated alias for ~s", [Login]);
                        false ->
                            ok
                    end,
                    case nkdomain_obj:load(SrvId, ObjId, LoadOpts) of
                        {ok, nkdomain_user, ObjId, Pid} ->
                            {ok, ObjId, Pid};
                        _ ->
                            {error, user_not_found}
                    end
            end
    end.


%% @private
do_login(Pid, ObjId, Pass) ->
    {ok, Pass2} = user_pass(Pass),
    Fun = fun(#obj_session{obj=Obj}) ->
        case Obj of
            #{?MODULE:=#{password:=Pass2}} -> {ok, true};
            _ -> {ok, false}
        end
    end,
    case nkdomain_obj:sync_op(Pid, {apply, Fun}) of
        {ok, true} ->
            {ok, ObjId};
        {ok, false} ->
            {error, invalid_password};
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

