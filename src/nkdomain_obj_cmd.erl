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

-module(nkdomain_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/3]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
api(<<"create">>, Type, #nkreq{data=Data, srv_id=SrvId, user_id=UserId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            case ?CALL_NKROOT(object_create, [SrvId, DomainId, Type, UserId, Data]) of
                {ok, ObjIdExt, Unknown} ->
                    #obj_id_ext{obj_id=ObjId, path=Path} = ObjIdExt,
                    Req2 = nkservice_api:add_unknown(Unknown, Req),
                    {ok, #{obj_id=>ObjId, path=>Path}, Req2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"get">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            nkdomain:get_obj(Id);
        {error, Error} ->
            {error, Error}
    end;

api(<<"get_info">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            nkdomain:get_info(Id);
        {error, Error} ->
            {error, Error}
    end;

api(<<"get_name">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            nkdomain:get_name(Id);
        {error, Error} ->
            {error, Error}
    end;

api(<<"stop">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            case nkdomain:unload(Id, user_stop) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"delete">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            case nkdomain:delete(Id) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"update">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            case nkdomain:update(Id, maps:remove(id, Data)) of
                {ok, Unknown} ->
                    Req2 = nkservice_api:add_unknown(Unknown, Req),
                    {ok, #{}, Req2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"update_obj_name">>, Type, #nkreq{data=#{obj_name:=ObjName}=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            case nkdomain:update_name(Id, ObjName) of
                {ok, ObjName2} ->
                    {ok, #{obj_name=>ObjName2}, Req};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"enable">>, Type, #nkreq{data=#{enable:=Enable}=Data}=Req) ->
    case nkdomain_api_util:get_id(Type, Data, Req) of
        {ok, Id} ->
            case nkdomain:enable(Id, Enable) of
                ok ->
                    {ok, #{}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"find">>, Type, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            Filters1 = maps:get(filters, Data, #{}),
            Filters2 = Filters1#{type=>Type},
            Data2 = Data#{filters=>Filters2},
            case nkdomain_domain_obj:search(SrvId, DomainId, Data2) of
                {ok, Total, List, _Meta} ->
                    {ok, #{<<"total">>=>Total, <<"data">>=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"find_all">>, Type, #nkreq{data=Data}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            Filters1 = maps:get(filters, Data, #{}),
            Filters2 = Filters1#{type=>Type},
            Data2 = Data#{filters=>Filters2},
            case nkdomain_domain_obj:search_all(DomainId, Data2) of
                {ok, Total, List, _Meta} ->
                    {ok, #{<<"total">>=>Total, <<"data">>=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(<<"make_token">>, Type, #nkreq{data=Data, user_id=UserId, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            case nkdomain_api_util:get_id(Type, Data, Req) of
                {ok, Id} ->
                    case nkdomain_token_obj:create(SrvId, DomainId, Id, UserId, Type, #{}, Data) of
                        {ok, TokenId, _Pid, TTL, Unknown} ->
                            Req2 = nkservice_api:add_unknown(Unknown, Req),
                            {ok, #{obj_id=>TokenId, ttl=>TTL}, Req2};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

api(_Cmd, _Type, _Req) ->
    lager:error("NKLOG API Not Implemented ~p", [_Cmd]),
    {error, not_implemented}.



%% ===================================================================
%% Private
%% ===================================================================

%%
%%%% @private
%%obj_id_reply(#obj_id_ext{obj_id=ObjId, path=Path}, Unknown) ->
%%    Base = #{
%%        obj_id => ObjId,
%%        path => Path
%%    },
%%    case Unknown of
%%        [] ->
%%            Base;
%%        _ ->
%%            Base#{unknown_fields=>Unknown}
%%    end.
%%

