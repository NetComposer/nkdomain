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
-export([get_token_data/1, consume_token/2, execute_token/1]).
-export([object_execute/5, object_schema/1, object_query/3, object_mutation/3]).

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
        module => module(),
        function => atom(),
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
    TokenData = maps:with([module, function], Opts),
    case check_ttl(Opts) of
        {ok, TTL} ->
            Obj = Base#{
                domain_id => DomainId,
                type => ?DOMAIN_TOKEN,
                ttl => TTL,
                ?DOMAIN_TOKEN => TokenData#{
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
    Mod = nkdomain_reg:get_type_module(SubType),
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
execute_token(Id) ->
    nkdomain_obj:sync_op(Id, {?MODULE, execute}).


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
        schema_type => 'Token',
        remove_after_stop => true
    }.

%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 5000,
        type_view_mod => nkdomain_token_obj_type_view
    }.


%% @doc
object_schema(Type) ->
    nkdomain_token_obj_schema:object_schema(Type).


%% @doc
object_execute(Field, ObjIdExt, #{?DOMAIN_USER:=User}, Args, _Ctx) ->
    nkdomain_token_obj_schema:object_execute(Field, ObjIdExt, User, Args).


%% @doc
object_query(QueryName, Params, Ctx) ->
    nkdomain_token_obj_schema:object_query(QueryName, Params, Ctx).


%% @doc
object_mutation(MutationName, Params, Ctx) ->
    nkdomain_token_obj_schema:object_mutation(MutationName, Params, Ctx).


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        data => #{enabled => false},
        module => #{type => keyword},
        function => #{type => keyword}
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
        vsn => binary,
        module => binary,
        function => binary,
        data => any,
        '__defaults' => #{vsn => 1, data => #{}}
    }.


%% @private
object_send_event(_Event, State) ->
    {ok, State}.


%% @private
object_sync_op({?MODULE, get_token_data}, _From, State) ->
    #obj_state{domain_id=DomainId, obj=Obj} = State,
    #{?DOMAIN_TOKEN:=#{data:=TokenData}, expires_time:=ExpiresTime, created_time:=CreatedTime} = Obj,
    Reply = #{
        domain_id => DomainId,
        pid => self(),
        data => TokenData,
        created_time => CreatedTime,
        expires_time => ExpiresTime
    },
    {reply, {ok, Reply}, State};

object_sync_op({?MODULE, execute}, _From, #obj_state{obj=Obj}=State) ->
    case Obj of
        #{?DOMAIN_TOKEN:=#{
            module := BinModule,
            function := BinFunction
        }} ->
            try
                Module = binary_to_existing_atom(BinModule, utf8),
                Function = binary_to_existing_atom(BinFunction, utf8),
                true = erlang:function_exported(Module, Function, 1),
                Reply = apply(Module, Function, [Obj]),
                {reply, Reply, State}
            catch
                _:_ ->
                    {reply, {error, invalid_token}, State}
            end;
        _ ->
            {reply, {error, invalid_token}, State}
    end;

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
