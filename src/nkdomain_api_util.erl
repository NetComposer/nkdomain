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

-module(nkdomain_api_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([session_login/1, token_login/2, check_token/2, check_raw_token/1]).
-export([search/1, get_id/3, get_id/4, add_id/3, add_meta/3, get_meta/2, remove_meta/2]).
-export([head_type_fields/2]).
-export_type([login_data/0, session_meta/0]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type login_data() ::
    #{
        id => binary(),
        password => binary(),
        domain_id => binary(),
        meta => map()
    }.


-type session_meta() ::
    #{
        session_id => binary(),
        local => binary(),
        remote => binary(),
        monitor => {module(), pid()}
    }.



%% ===================================================================
%% Public
%% ===================================================================



%% @doc Uses login_data() and session_meta()
-spec session_login(#nkreq{}) ->
    {ok, DomainId::nkdomain:obj_id(), UserId::nkdomain:obj_id(), SessId::nkdomain:obj_id(), pid(), #nkreq{}} |
    {error, term()}.

session_login(#nkreq{srv_id=SrvId, data=Data, session_meta=SessMeta}=Req) ->
    #{id:=User} = Data,
    case nkdomain_user:auth(User, Data) of
        {ok, UserId, UserDomainId} ->
            DomainId = case Data of
                #{domain_id:=DomainId0} ->
                    DomainId0;
                _ ->
                    UserDomainId
            end,
            SessData1 = maps:with([device_id, push_id, platform_id, platform_version], Data),
            SessData2 = maps:with([local, remote], SessMeta),
            LoginMeta = maps:get(meta, Data, #{}),
            SessData3 = maps:merge(SessData1, SessData2#{login_meta => LoginMeta}),
            SessOpts1 = maps:with([session_id, session_link], SessMeta),
            SessOpts2 = SessOpts1#{data=>SessData3},
            case nkdomain_session_obj:start(SrvId, DomainId, UserId, SessOpts2) of
                {ok, SessId, Pid} ->
                    Req2 = add_meta(login_meta, LoginMeta, Req),
                    Req3 = add_id(?DOMAIN_DOMAIN, DomainId, Req2),
                    Req4 = add_id(?DOMAIN_USER, UserId, Req3),
                    Req5 = add_id(?DOMAIN_SESSION, SessId, Req4),
                    Req6 = case Data of
                        #{role:=Role, role_id:=RoleId} ->
                            add_meta(role, {Role, RoleId}, Req5);
                        _ ->
                            Req5
                    end,
                    {ok, DomainId, UserId, SessId, Pid, Req6#nkreq{user_id=UserId}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec token_login(nkdomain:id(), #nkreq{}) ->
    {ok, TokenId::nkdomain:obj_id(), TTLSecs::integer()} | {error, term()}.

token_login(User, #nkreq{srv_id=SrvId, data=Data, session_meta=SessMeta, user_state=UserState}) ->
    case nkdomain_user:auth(User, Data) of
        {ok, UserId, UserDomainId} ->
            TokenData = #{
                session_meta => SessMeta,
                user_state => UserState
            },
            TokenOpts1 = maps:with([ttl], Data),
            TokenOpts2 = TokenOpts1#{srv_id=>SrvId},
            nkdomain_user:make_token(UserDomainId, UserId, TokenOpts2, TokenData);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
check_token(Token, #nkreq{session_meta=NewSessMeta, user_state=NewState}=Req) ->
    case check_raw_token(Token) of
        {ok, UserId, DomainId, Data, SessId} ->
            TokenSessMeta = maps:get(session_meta, Data, #{}),
            TokenState = maps:get(user_state, Data, #{}),
            Req2 = Req#nkreq{
                session_meta = maps:merge(TokenSessMeta, NewSessMeta),
                user_state = maps:merge(TokenState, NewState)
            },
            Req3 = add_id(?DOMAIN_DOMAIN, DomainId, Req2),
            Req4 = add_id(?DOMAIN_USER, UserId, Req3),
            Req5 = case SessId of
                <<>> -> Req4;
                _ -> add_id(?DOMAIN_SESSION, SessId, Req4)
            end,
            {ok, UserId, Req5};
        {error, Error} ->
            {error, Error}
    end.

%% @doc
check_raw_token(Token) ->
    case nkdomain:get_obj(Token) of
        {ok, #{type:=?DOMAIN_SESSION, ?DOMAIN_SESSION:=Data}=Obj} ->
            #{domain_id:=DomainId, created_by:=UserId} = Obj,
            {ok, UserId, DomainId, maps:get(data, Data, #{}), Token};
        {ok, #{type:=?DOMAIN_TOKEN, subtype:=SubTypes, ?DOMAIN_TOKEN:=Data}=Obj} ->
            case lists:member(?DOMAIN_USER, SubTypes) of
                true ->
                    #{domain_id:=DomainId, created_by:=UserId} = Obj,
                    {ok, UserId, DomainId,  maps:get(data, Data, #{}), <<>>};
                _ ->
                    {error, token_invalid}
            end;
        _ ->
            case catch base64:decode(Token) of
                Bin when is_binary(Bin) ->
                    case binary:split(Bin, <<":">>) of
                        [Login, Pass] ->
                            case nkdomain_user:auth(Login, #{password=>Pass}) of
                                {ok, UserId, DomainId} ->
                                    {ok, UserId, DomainId, #{}, <<>>};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        _ ->
                            {error, token_invalid}
                    end;
                _ ->
                    {error, token_invalid}
            end
    end.


%%%% @private
%%get_domain(#{domain_id:=Domain}) ->
%%    load_domain(Domain);
%%
%%get_domain(#{id:=User}) ->
%%    case nkdomain_lib:find(User) of
%%        #obj_id_ext{path=Path} ->
%%            {ok, Domain, _} = nkdomain_util:get_parts(?DOMAIN_USER, Path),
%%            load_domain(Domain);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%%%% @private
%%load_domain(Domain) ->
%%    case nkdomain_lib:find(Domain) of
%%        #obj_id_ext{type = ?DOMAIN_DOMAIN, obj_id=DomainId} ->
%%            {ok, DomainId};
%%        {error, _} ->
%%            {error, {domain_unknown, Domain}}
%%    end.


%% @doc
search({ok, Total, List}) ->
    Data = #{
        total => Total,
        data =>
            lists:map(
                fun({Type, ObjId, Path}) -> #{type=>Type, obj_id=>ObjId, path=>Path} end,
                List)
    },
    {ok, Data};

search({error, Error}) ->
    {error, Error}.


%% @doc
get_id(Type, Data, Req) ->
    get_id(Type, id, Data, Req).


%% @doc
get_id(Type, Field, Data, #nkreq{user_state=UserState}) ->
    case maps:find(Field, Data) of
        {ok, Id} ->
            {ok, Id};
        error ->
            ObjIds = maps:get(nkdomain_obj_ids, UserState, #{}),
            case maps:find(Type, ObjIds) of
                {ok, Id} ->
                    {ok, Id};
                error ->
                    % lager:error("OI: ~s ~p", [Type, ObjIds]),
                    {error, {missing_field, Field}}
            end
    end.


%% @doc Adds information in the session associated to a type
add_id(Type, Id, #nkreq{user_state=UserState}=Req) ->
    ObjIds = maps:get(nkdomain_obj_ids, UserState, #{}),
    UserState2 = UserState#{nkdomain_obj_ids=>ObjIds#{Type => Id}},
    Req#nkreq{user_state=UserState2}.


%% @doc Adds information in the session associated to a key
add_meta(Key, Id, #nkreq{user_state=UserState}=Req) ->
    UserState2 = UserState#{Key => Id},
    Req#nkreq{user_state=UserState2}.


%% @doc Adds 'logged in' information to the state
get_meta(Key, #nkreq{user_state=UserState}) ->
    maps:get(Key, UserState, <<>>).


%% @doc Adds 'logged in' information to the state
remove_meta(Type, #nkreq{user_state=UserState}=Req) ->
    UserState2 = maps:remove(Type, UserState),
    Req#nkreq{user_state=UserState2}.



%% @doc Pre-pends the type to fields in a map or list
head_type_fields(Type, Map) when is_map(Map) ->
    head_type_fields(Type, maps:to_list(Map));

head_type_fields(Type, List) when is_list(List) ->
    Fields = [
        {list_to_binary([Type, ".", to_bin(Key)]), Val}
        || {Key, Val} <- List
    ],
    maps:from_list(Fields).



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
