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

-export([session_login/4, token_login/3]).
-export([search/1, get_id/3, get_id/4, add_id/3]).

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

%% @doc
-spec session_login(nkservice:id(), login_data(), session_meta(), map()) ->
    {ok, UserId::nkdomain:obj_id(), SessId::nkdomain:obj_id(), pid(), UserMeta::map()} | {error, term()}.

session_login(SrvId, Data, SessMeta, UserMeta) ->
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
                            UserMeta1 = UserMeta#{login_meta=>LoginMeta},
                            UserMeta2 = nkdomain_api_util:add_id(?DOMAIN_DOMAIN, DomainId, UserMeta1),
                            UserMeta3 = nkdomain_api_util:add_id(?DOMAIN_USER, UserId, UserMeta2),
                            UserMeta4 = nkdomain_api_util:add_id(?DOMAIN_SESSION, SessId, UserMeta3),
                            {ok, UserId, SessId, Pid, UserMeta4};
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
-spec token_login(nkservice:id(), login_data()|#{ttl=>integer()}, session_meta()) ->
    {ok, TokenId::nkdomain:obj_id(), TTLSecs::integer()} | {error, term()}.

token_login(SrvId, Data, SessMeta) ->
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
get_id(Type, Field, Data, #nkreq{user_meta=Meta}) ->
    case maps:find(Field, Data) of
        {ok, Id} ->
            {ok, Id};
        error ->
            ObjIds = maps:get(nkdomain_obj_ids, Meta, #{}),
            case maps:find(Type, ObjIds) of
                {ok, Id} ->
                    {ok, Id};
                error ->
                    % lager:error("OI: ~s ~p", [Type, ObjIds]),
                    {error, {missing_field, Field}}
            end
    end.


%% @doc Adds 'logged in' information to the state
add_id(Type, Id, #nkreq{user_meta=Meta}) ->
    add_id(Type, Id, Meta);

add_id(Type, Id, Meta) ->
    ObjIds = maps:get(nkdomain_obj_ids, Meta, #{}),
    Meta#{nkdomain_obj_ids=>ObjIds#{Type => Id}}.


