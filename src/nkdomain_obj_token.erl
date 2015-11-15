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

-module(nkdomain_obj_token).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj).

-export([new_token/3, renew_token/2, check_token/2, remove_token/1]).
-export([init/2, load/4, get_backend/1, handle_call/4, handle_info/3]).
-export_type([token_data/0]).

-include("nkdomain.hrl").

-type token_data() ::
    #{
        valid_time => integer(),     % secs
        ip => inet:ip_address(),
        port => inet:port_number(),
        pass => binary()
    }.


%% ===================================================================
%% API
%% ===================================================================


%% @doc Creates a new token
%% Objects supporting tokens must support to handle_call {check_token_data, _} 
%% handle_cast for {new_token, _, _}


-spec new_token(nkdomain:class(), nkdomain:obj_id(), token_data()) ->
    {ok, nkdomain:token()} | {error, term()}.

new_token(Class, ObjId, Data) ->
    ObjId1 = nklib_util:to_binary(ObjId),
    case nkdomain_obj:do_call(Class, ObjId1, {check_token_data, Data}, #{}) of
        ok ->
            TokenId = nklib_util:luid(),
            Load = #{set=>{Class, ObjId1, Data}},
            case nkdomain_obj:load(token, TokenId, Load, #{}) of
                {error, Error} -> {error, Error};
                _ -> {ok, TokenId}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Renovates a new token
-spec renew_token(nkdomain:obj_id(), token_data()) ->
    ok | {error, term()}.

renew_token(TokenId, Data) ->
    nkdomain_obj:do_call(token, TokenId, {renew, Data}, #{}).


%% @doc Get token's referring objecy
-spec check_token(nkdomain:obj_id(), token_data()) ->
    {ok, nkdomain:class(), nkdomain:obj_id()} | {error, term()}.

check_token(TokenId, Data) ->
    nkdomain_obj:do_call(token, TokenId, {check, Data}, #{}).


%% @doc Removes token
-spec remove_token(nkdomain:obj_id()) ->
    ok | {error, term()}.

remove_token(TokenId) ->
    case nkdomain_obj:get_pid(token, TokenId) of
        {ok, Pid} -> 
            Pid ! token_timeout,
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(state, {
    id :: nkdomain:obj_id(),
    class :: ndomain:class(),
    obj_id :: nkdomain:obj_id(),
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    stop :: nklib_util:l_timestamp(),
    timer :: reference()
}).


%% @private
-spec init(nkdomain:obj_id(), nkdomain:token()) ->
    {ok, nkdomain_obj:init_opts(), #state{}}.

init(TokenId, Token) ->
    {ok, #{backend=>none}, Token, #state{id=TokenId}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), token_data(), #state{}) ->
    {ok, nkdomain:obj(), #state{}} | {removed, #state{}} | {error, term(), #state{}}.

load(#{set:={Class, ObjId, Data}}, _Opts, Token, #state{id=TokenId}=State) ->
    State1 = State#state{
        class = Class,
        obj_id = ObjId,
        ip = maps:get(ip, Data, undefined),
        port = maps:get(port, Data, undefined)
    },
    State2 = schedule(Data, State1),
    nkdomain_obj:do_cast(Class, ObjId, {new_token, TokenId, self()}),
    {ok, Token, State2}.


%% @private
-spec get_backend(nkbase:class_meta()) ->
    nkbase:class_meta().

get_backend(Base) ->
    Base#{backend=>ets, ttl=>24*60*60}.


%% @private
-spec handle_call(term(), {pid(), term()}, nkdomain:token(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}} | {removed, term()}.

handle_call({check, Data}, _From, _Token, State) ->
    case check(Data, State) of
        {error, not_found} -> 
            {removed, {error, not_found}, State};
        Reply -> 
            {reply, Reply, State}
    end;

handle_call({renew, Data}, _From, _Token, State) ->
    case check(Data, State) of
        {ok, _, _} ->
            State1 = schedule(Data, State),
            {reply, ok, State1};
        {error, not_found} ->
            {removed, {error, not_found}, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call(Msg, _From, _Obj, State) -> 
    lager:warning("Module ~p received unexpected call: ~p (~p)", [?MODULE, Msg, State]),
    {noreply, State}.


%% @private
-spec handle_info(term(), nkdomain:token(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}}.

handle_info(token_timeout, _Obj, State) ->
    {stop, normal, State};

handle_info(Info, _Obj, State) -> 
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
check(Data, #state{class=Class, obj_id=ObjId}=State) ->
    case check_time(State) of
        true ->
            case check_ip(Data, State) of
                true ->
                    case check_port(Data, State) of
                        true -> 
                            {ok, Class, ObjId};
                        false ->
                            {error, invalid_connection}
                    end;
                false ->
                    {error, invalid_connection}
            end;
        false ->
            lager:warning("T: ~p, ~p", [nklib_util:l_timestamp(), State#state.stop]),
            {error, not_found}
    end.


%% @private
check_time(#state{stop=Stop}) ->
    nklib_util:l_timestamp() < Stop.


%% @private
check_ip(_, #state{ip=undefined}) -> true;
check_ip(#{ip:=Ip}, #state{ip=Ip}) -> true;
check_ip(_, _) -> false.


%% @private
check_port(_, #state{port=undefined}) -> true;
check_port(#{port:=Port}, #state{port=Port}) -> true;
check_port(_, _) -> false.


%% @private
schedule(Data, #state{timer=Timer}=State) ->
    nklib_util:cancel_timer(Timer),
    MSecs = case maps:find(valid_time, Data) of
        {ok, Time} when is_integer(Time), Time>0 -> 
            Time;
        _ -> 
            nkdomain_app:get(token_timeout)
    end,
    State#state{
        stop = nklib_util:l_timestamp() + MSecs,
        timer = erlang:send_after(MSecs, self(), token_timeout)
    }.

