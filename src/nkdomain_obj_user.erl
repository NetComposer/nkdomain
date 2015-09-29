%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Gonzalez Florido.  All Rights Reserved.
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

-module(nkdomain_obj_user).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj).

-export([get_tokens/1, user_pass/1, make_pass/1]).
-export([init/2, load/4, remove/2, handle_call/4, handle_cast/3, handle_info/3]).

-include("nkdomain.hrl").

-type user() ::
    nkdomain_obj:base_obj() |
    #{
        alias => [binary()],
        name => binary(),
        surname => binary(),
        password => binary()
    }.


%% ===================================================================
%% API
%% ===================================================================

-spec get_tokens(nkdomain:obj_id()) ->
    {ok, [nkdomain:obj_id()]} | {error, term()}.

get_tokens(ObjId) ->
    nkdomain_obj:do_call(user, ObjId, get_tokens, #{}).


%% @doc Generates a password from an user password or hash
-spec user_pass(string()|binary()) ->
    binary().

user_pass(Pass) ->
    Pass1 = nklib_util:to_binary(Pass),
    case binary:split(Pass1, <<"!">>, [global]) of
        [<<"NKD">>, <<>>, P, <<>>] when byte_size(P) > 10 -> Pass1;
        _ -> make_pass(Pass1)
    end.


%% @doc Generates a password from an user password
-spec make_pass(string()|binary()) ->
    binary().

make_pass(Pass) ->
    Pass1 = nklib_util:to_binary(Pass),
    <<"NKD!!", (pbkdf2(Pass1))/binary, "!">>.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(state, {
    id :: nkdomain:obj_id(),
    tokens = [] :: [{nkdomain:obj_id(), pid()}]
}).


%% @private
-spec init(nkdomain:obj_id(), user()) ->
    {ok, nkdomain_obj:init_opts(), user(), #state{}}.

init(UserId, User) ->
    Timeout = nkdomain_app:get(user_timeout),
    User1 = maps:merge(#{alias => []}, User),
    {ok, #{timeout=>Timeout}, User1, #state{id=UserId}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), user(), #state{}) ->
    {ok, nkdomain:obj(), term()} | removed | {error, term()}.

load(Data, Opts, User, State) ->
    do_load(maps:to_list(Data), Opts, User, State).


%% @private
-spec remove(user(), #state{}) ->
    ok.

remove(User, State) ->
    do_load([{alias, []}], #{replace=>true}, User, State),
    ok.
    

%% @private
-spec handle_call(term(), {pid(), term()}, user(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}}.

handle_call({check_token_data, Data}, _From, User, State) ->
    Reply = case Data of
        #{password:=DataPass1} ->
            DataPass2 = make_pass(DataPass1),
            case User of
                #{password:=DataPass2} -> 
                    ok;
                _ -> 
                   {error, invalid_password}
            end;
        _ ->
            {error, no_password}
    end,
    {reply, Reply, State};

handle_call(get_tokens, _From, _User, #state{tokens=Tokens}=State) ->
    {reply, [TokenId || {TokenId, _Pid} <-Tokens], State};

handle_call(Msg, _From, _Obj, State) -> 
    lager:warning("Module ~p received unexpected call: ~p (~p)", [?MODULE, Msg, State]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), user(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}}.

handle_cast({new_token, TokenId, Pid}, _User, #state{tokens=Tokens}=State) ->
    monitor(process, Pid),
    Tokens1 = lists:keystore(TokenId, 1, Tokens, {TokenId, Pid}),
    {noreply, State#state{tokens=Tokens1}};

handle_cast(Msg, _Obj, State) -> 
    lager:warning("Module ~p received unexpected cast: ~p (~p)", [?MODULE, Msg, State]),
    {noreply, State}.


%% @private
-spec handle_info(term(), user(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}}.

handle_info(timeout, _Obj, #state{tokens=Tokens}=State) ->
    case Tokens==[] of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, _User, State) ->
    #state{tokens=Tokens} = State,
    Tokens1 = lists:keydelete(Pid, 2, Tokens),
    {noreply, State#state{tokens=Tokens1}};

handle_info(Info, _Obj, State) -> 
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_load([], _Opts, User, State) ->
    {ok, User, State};

do_load([{alias, Aliases}|Rest], Opts, User, #state{id=UserId}=State) ->
    Aliases1 = do_load_aliases(UserId, Aliases, []),
    UserAliases = maps:get(alias, User, []),
    Aliases2 = case Opts of
        #{replace:=true} ->
            do_remove_aliases(UserId, UserAliases -- Aliases1),
            lists:usort(Aliases1);
        _ ->
            lists:usort(UserAliases++Aliases1)
    end,
    do_load(Rest, Opts, User#{alias=>Aliases2}, State);

do_load([{Key, Val}|Rest], Opts, User, State) ->
    do_load(Rest, Opts, maps:put(Key, Val, User), State).



%% @private
do_load_aliases(_UserId, [], Acc) ->
    lists:reverse(Acc);

do_load_aliases(UserId, [Alias|Rest], Acc) ->
    Acc1 = case nkdomain_obj_alias:add_alias(Alias, <<"user:", UserId/binary>>) of
        ok ->
            [Alias|Acc];
        {error, Error} ->
            lager:warning("Could not load alias ~s: ~p", [Alias, Error]),
            Acc
    end,
    do_load_aliases(UserId, Rest, Acc1).


%% @private
do_remove_aliases(_UserId, []) ->
    ok;

do_remove_aliases(UserId, [Alias|Rest]) ->
    case nkdomain_obj_alias:remove_alias(Alias, <<"user:", UserId/binary>>) of
        ok ->
            ok;
        {error, Error} ->
            lager:warning("Could not remove alias ~s: ~p", [Alias, Error])
    end,
    do_remove_aliases(UserId, Rest).


% %% @private
% extract_pass(Bin) ->
%     case binary:split(Bin, <<"!">>, [global]) of
%         [<<"NKD">>, <<>>, Pass, <<>>] -> Pass;
%         _ -> pbkdf2(Bin)
%     end.


%% @private
pbkdf2(Pass) ->
    Salt = <<"nkdomain_obj_user">>,
    Iters = nkdomain_app:get(user_password_pbkdf2_iters),
    {ok, Base} = pbkdf2:pbkdf2(sha, Pass, Salt, Iters),
    nklib_util:lhash(Base).
