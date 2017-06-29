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

-export([session_login/1, token_login/1, check_token/2]).
-export([search/1, get_id/3, get_id/4, add_id/3, add_meta/3, remove_meta/2]).
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
    {ok, UserId::nkdomain:obj_id(), SessId::nkdomain:obj_id(), pid(), #nkreq{}} | {error, term()}.

session_login(#nkreq{srv_id=SrvId, data=Data, session_meta=SessMeta}=Req) ->
    #{id:=User} = Data,
    Auth = #{password => maps:get(password, Data, <<>>)},
    case get_domain(SrvId, Data) of
        {ok, DomainId} ->
            case nkdomain_user_obj:auth(SrvId, User, Auth) of
                {ok, UserId} ->
                    LoginMeta = maps:get(meta, Data, #{}),
                    SessOpts1 = maps:with([session_id, local, remote, monitor], SessMeta),
                    SessOpts2 = SessOpts1#{login_meta => LoginMeta},
                    case nkdomain_session_obj:start(SrvId, DomainId, UserId, SessOpts2) of
                        {ok, SessId, Pid} ->
                            Req2 = add_meta(login_meta, LoginMeta, Req),
                            Req3 = add_id(?DOMAIN_DOMAIN, DomainId, Req2),
                            Req4 = add_id(?DOMAIN_USER, UserId, Req3),
                            Req5 = add_id(?DOMAIN_SESSION, SessId, Req4),
                            {ok, UserId, SessId, Pid, Req5#nkreq{user_id=UserId}};
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
-spec token_login(#nkreq{}) ->
    {ok, TokenId::nkdomain:obj_id(), TTLSecs::integer()} | {error, term()}.

token_login(#nkreq{srv_id=SrvId, data=Data, session_meta=SessMeta}) ->
    #{id:=User} = Data,
    Auth = #{password => maps:get(password, Data, <<>>)},
    case get_domain(SrvId, Data) of
        {ok, DomainId} ->
            case nkdomain_user_obj:auth(SrvId, User, Auth) of
                {ok, UserId} ->
                    LoginMeta = maps:get(meta, Data, #{}),
                    TokenData1 = maps:with([session_id, local, remote], SessMeta),
                    TokenData2 = TokenData1#{login_meta => LoginMeta},
                    TokenOpts = maps:with([ttl], Data),
                    nkdomain_user_obj:make_token(SrvId, DomainId, UserId, TokenOpts, TokenData2);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
check_token(Token, #nkreq{srv_id=SrvId}=Req) ->
    case nkdomain:get_obj(SrvId, Token) of
        {ok, #{type:=?DOMAIN_SESSION, ?DOMAIN_SESSION:=Data}=Obj} ->
            #{domain_id:=DomainId, created_by:=UserId} = Obj,
            LoginMeta = maps:get(login_meta, Data, #{}),
            Req2 = add_meta(login_meta, LoginMeta, Req),
            Req3 = add_id(?DOMAIN_DOMAIN, DomainId, Req2),
            Req4 = add_id(?DOMAIN_USER, UserId, Req3),
            Req5 = add_id(?DOMAIN_SESSION, Token, Req4),
            {ok, UserId, Req5};
        {ok, #{type:=?DOMAIN_TOKEN, subtype:=SubTypes, ?DOMAIN_TOKEN:=Data}=Obj} ->
            case lists:member(?DOMAIN_USER, SubTypes) of
                true ->
                    #{domain_id:=DomainId, created_by:=UserId} = Obj,
                    UserMeta1 = #{login_meta=>maps:get(login_meta, Data)},
                    UserMeta2 = add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
                    UserMeta3 = add_id(?DOMAIN_USER, UserId, UserMeta2),
                    {ok, UserId, UserMeta3};
                _ ->
                    {error, invalid_token}
            end;
        _ ->
            case catch base64:decode(Token) of
                Bin when is_binary(Bin) ->
                    case binary:split(Bin, <<":">>) of
                        [Login, Pass] ->
                            case nkdomain_user_obj:auth(SrvId, Login, #{password=>Pass}) of
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




%% @private
get_domain(SrvId, #{domain_id:=Domain}) ->
    load_domain(SrvId, Domain);

get_domain(SrvId, #{id:=User}) ->
    case nkdomain_lib:find(SrvId, User) of
        #obj_id_ext{path=Path} ->
            {ok, Domain, _} = nkdomain_util:get_parts(?DOMAIN_USER, Path),
            load_domain(SrvId, Domain);
        {error, Error} ->
            {error, Error}
    end.


%% @private
load_domain(SrvId, Domain) ->
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{obj_id=DomainId} ->
            {ok, DomainId};
        {error, _} ->
            {error, {domain_unknown, Domain}}
    end.

%% @doc
search({ok, Total, List}) ->
    Data = #{
        <<"total">> => Total,
        <<"data">> =>
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






%% @doc Adds 'logged in' information to the state
add_id(Type, Id, #nkreq{user_state=UserState}=Req) ->
    ObjIds = maps:get(nkdomain_obj_ids, UserState, #{}),
    UserState2 = UserState#{nkdomain_obj_ids=>ObjIds#{Type => Id}},
    Req#nkreq{user_state=UserState2}.


%% @doc Adds 'logged in' information to the state
add_meta(Type, Id, #nkreq{user_state=UserState}=Req) ->
    UserState2 = UserState#{Type => Id},
    Req#nkreq{user_state=UserState2}.


%% @doc Adds 'logged in' information to the state
remove_meta(Type, #nkreq{user_state=UserState}=Req) ->
    UserState2 = maps:remove(Type, UserState),
    Req#nkreq{user_state=UserState2}.


