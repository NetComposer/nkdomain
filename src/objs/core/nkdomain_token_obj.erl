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

-export([create/3]).
-export([object_info/0, object_es_mapping/0, object_parse/2, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_info/0]).
-export([get_token_data/1, consume_token/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Token "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

-type create_opts() ::
    #{
        parent_id => nkdomain:id(),     % Mandatory
        created_by => nkdomain:id(),    % Mandatory
        subtype => nkdomain:subtype(),
        srv_id => nkservice:id(),
        ttl => integer()
    }.

-type token_data() :: map().



%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(nkdomain:obj_id(), create_opts(), token_data()) ->
    {ok, TokenId::nkdomain:obj_id(), pid(), integer()} | {error, term()}.

create(DomainId, Opts, Data) ->
    Base = maps:with([parent_id, created_by, subtype, srv_id], Opts),
    case check_ttl(Opts) of
        {ok, TTL} ->
            Obj = Base#{
                domain_id => DomainId,
                type => ?DOMAIN_TOKEN,
                ttl => TTL,
                ?DOMAIN_TOKEN => #{
                    data => Data
                }
            },
            case nkdomain_obj_make:create(Obj) of
                {ok, #obj_id_ext{obj_id=TokenId, pid=Pid}, _Unknown} ->
                    {ok, TokenId, Pid, TTL};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
check_ttl(TokenOpts) ->
    SubType = maps:get(subtype, TokenOpts, ?DOMAIN_TOKEN),
    Mod = nkdomain_lib:get_module(SubType),
    Info = case erlang:function_exported(Mod, object_info, 0) of
        true ->
            Mod:object_admin_info();
        false ->
            #{}
    end,
    DefTTL = maps:get(default_token_ttl, Info, ?DEF_TOKEN_TTL),
    MaxTTL = maps:get(max_token_ttl, Info, ?MAX_TOKEN_TTL),
    case maps:get(ttl, TokenOpts, DefTTL) of
        TTL when TTL>=0, TTL < MaxTTL ->
            {ok, TTL};
        _ ->
            {error, token_invalid_ttl}
    end.


%% @doc
consume_token(Id, Reason) ->
    nkdomain_obj:sync_op(Id, {?MODULE, consume, Reason}).


%% @doc
get_token_data(Id) ->
    nkdomain_obj:sync_op(Id, {?MODULE, get_token_data}).



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
        type_view_mod => nkdomain_token_obj_type_view
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        data => #{enabled => false}
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
        vsn => binary,
        data => any,
        '__defaults' => #{vsn => 1, data => #{}}
    }.


%% @private
object_send_event(_Event, State) ->
    {ok, State}.


%% @private
object_sync_op({?MODULE, get_token_data}, _From, State) ->
    #obj_state{domain_id=DomainId, obj=Obj} = State,
    #{?DOMAIN_TOKEN:=#{data:=TokenData}} = Obj,
    Reply = #{
        domain_id => DomainId,
        data => TokenData
    },
    {reply, {ok, Reply}, State};

%% @private
object_sync_op({?MODULE, consume, Reason}, From, State) ->
    #obj_state{domain_id=DomainId, obj=Obj} = State,
    #{?DOMAIN_TOKEN:=#{data:=TokenData}} = Obj,
    Reply = #{
        domain_id => DomainId,
        data => TokenData
    },
    gen_server:reply(From, {ok, Reply}),
    % Process user events before detecting token down
    timer:sleep(500),
    {stop, {object_consumed, Reason}, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op(_Op, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================
