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

-module(nkdomain_token_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/7]).
-export([object_info/0, object_parse/3, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0]).
-export([get_token_data/2, consume_token/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Token "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(nkservice:id(), nkdomain:id(), nkdomain:id(), nkdomain:id(), nkdomain:subtype(),
             nkdomain_obj_make:make_opts(), map()) ->
    {ok, #obj_id_ext{}, integer(), [Unknown::binary()]} | {error, term()}.

create(SrvId, DomainId, ParentId, UserId, SubType, Opts, Data) ->
    case check_ttl(SubType, Opts) of
        {ok, SecsTTL} ->
            Obj = Opts#{
                type => ?DOMAIN_TOKEN,
                domain_id => DomainId,
                parent_id => ParentId,
                created_by => UserId,
                subtype => SubType,
                ttl => SecsTTL,
                ?DOMAIN_TOKEN => Data
            },
            case nkdomain_obj_make:create(SrvId, Obj) of
                {ok, #obj_id_ext{obj_id=TokenId}, Unknown} ->
                    {ok, TokenId, SecsTTL, Unknown};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
check_ttl(Type, Opts) ->
    Mod = nkdomain_all_types:get_module(Type),
    Info = case erlang:function_exported(Mod, object_info, 0) of
        true ->
            Mod:object_admin_info();
        false ->
            #{}
    end,
    DefTTL = maps:get(default_token_ttl, Info, ?DEF_TOKEN_TTL),
    MaxTTL = maps:get(max_token_ttl, Info, ?MAX_TOKEN_TTL),
    case maps:get(ttl, Opts, DefTTL) of
        TTL when TTL>=0, TTL < MaxTTL ->
            {ok, TTL};
        _ ->
            {error, invalid_token_ttl}
    end.


%% @doc
consume_token(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, consume}).

%% @doc
get_token_data(SrvId, Id) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_token_data}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?DOMAIN_TOKEN,
        remove_after_stop => true
    }.

%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 5000,
        tree_id => <<"domain_tree_sessions_tokens">>
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    any.


%% @private
object_send_event(_Event, State) ->
    {ok, State}.


%% @private
object_sync_op({?MODULE, get_token_data}, _From, State) ->
    #?STATE{domain_id=DomainId, obj=Obj} = State,
    #{?DOMAIN_TOKEN:=TokenData} = Obj,
    Reply = #{
        domain_id => DomainId,
        data => TokenData
    },
    {reply, {ok, Reply}, State};

%% @private
object_sync_op({?MODULE, consume}, _From, State) ->
    #?STATE{domain_id=DomainId, obj=Obj} = State,
    #{?DOMAIN_TOKEN:=TokenData} = Obj,
    Reply = #{
        domain_id => DomainId,
        data => TokenData
    },
    {stop, object_consumed, {ok, Reply}, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op(_Op, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================
