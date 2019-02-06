%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain core class API processing support lib
-module(nkdomain_api_core_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_res_domain_name/2, check_request_obj/3, parse_params/2]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN API CoreV1: "++Txt, Args)).

-include("nkdomain.hrl").

%%-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
%%-include_lib("nkpacket/include/nkpacket.hrl").


%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Internal
%% ===================================================================



%% @doc Completes resource, domain and name, maybe from body
-spec get_res_domain_name(nkservice:id(), map()) ->
    {ok, nkdomain_api:request()} | {error, nkservice:msg()}.

get_res_domain_name(_SrvId, #{resource:=_, domain:=_, name:=_}=ApiReq) ->
    {ok, ApiReq};

get_res_domain_name(SrvId, #{group:=Group}=ApiReq) ->
    try
        Body = maps:get(body, ApiReq, #{}),
        Obj = case
            is_map(Body) andalso
            nklib_syntax:parse(Body, api_obj_syntax())
        of
            {ok, ParsedBody, _} ->
                ParsedBody;
            _ ->
                #{}
        end,
        ApiReq2 = case maps:is_key(resource, ApiReq) of
            true ->
                ApiReq;
            false ->
                case Obj of
                    #{<<"kind">>:=Kind} ->
                        case nkdomain_actor_util:find_resource(SrvId, Group, Kind) of
                            {camel, Resource} ->
                                ApiReq#{resource=>Resource};
                            undefined ->
                                throw({kind_unknown, Kind})
                        end;
                    _ ->
                        throw({field_missing, <<"resource">>})
                end
        end,
        ApiReq3 = case maps:is_key(domain, ApiReq2) of
            true ->
                ApiReq2;
            false ->
                case Obj of
                    #{<<"metadata">>:=#{<<"domain">>:=ObjDomain}} ->
                        ApiReq2#{domain=>ObjDomain};
                    _ ->
                        ApiReq2#{domain=>?ROOT_DOMAIN}
                end
        end,
        ApiReq4 = case maps:is_key(name, ApiReq3) of
            true ->
                ApiReq3;
            false ->
                case Obj of
                    #{<<"metadata">>:=#{<<"name">>:=BodyName}} when BodyName /= <<>> ->
                        ApiReq3#{name => BodyName};
                    _ ->
                        ApiReq3
                end
        end,
        {ok, ApiReq4}
    catch
        throw:Throw ->
            {error, Throw}
    end.



%% @doc Checks that fields in req are not different in body:
%%      (group, vsn, domain, resource, name) and expand links
-spec check_request_obj(nkservice:id(), #actor_id{}, map()) ->
    {ok, Obj::map()} | {error, nkservice:msg()}.

check_request_obj(SrvId, ActorId, ApiReq) ->
    #actor_id{group=Group, vsn=Vsn, domain=Domain} = ActorId,
    try
        Body = maps:get(body, ApiReq, #{}),
        Obj = case is_map(Body) of
            true ->
                case nklib_syntax:parse(Body, api_obj_syntax()) of
                    {ok, ParsedBody, _} ->
                        ParsedBody;
                    {error, Error} ->
                        throw({error, Error})
                end;
            false ->
                throw({error, {field_missing, <<"apiVersion">>}})
        end,
        #{resource:=Resource} = ApiReq,
        ApiVsn = <<Group/binary, $/, Vsn/binary>>,
        case Obj of
            #{<<"apiVersion">>:=ObjApiVsn} when ApiVsn/=ObjApiVsn->
                throw({field_invalid, <<"apiVersion">>});
            _ ->
                ok
        end,
        case Obj of
            #{<<"metadata">>:=#{<<"domain">>:=ObjDomain}} when Domain/=ObjDomain->
                throw({field_invalid, <<"metadata.domain">>});
            _ ->
                ok
        end,
        case Obj of
            #{<<"kind">>:=Kind} ->
                case nkdomain_actor_util:find_resource(SrvId, Group, Kind) of
                    {camel, Resource} ->
                        ok;
                    _ ->
                        throw({field_invalid, <<"kind">>})
                end;
            _ ->
                ok
        end,
        case Obj of
            #{<<"metadata">>:=#{<<"name">>:=BodyName}} ->
                % If name is in body, it has been already read, so there is a name
                case maps:find(name, ApiReq) of
                    {ok, BodyName} ->
                        ok;
                    error when BodyName == <<>> ->
                        ok;
                    _ ->
                        throw({field_invalid, <<"metadata.name">>})
                end;
            _ ->
                ok
        end,
        {ok, Obj}
    catch
        throw:Throw ->
            {error, Throw}
    end.


%% @doc
parse_params(Verb, #{params:=Params}=Api) ->
    case map_size(Params) of
        0 ->
            {ok, Api};
        _ ->
            Syntax = params_syntax(Verb),
            case nklib_syntax:parse(Params, Syntax, #{allow_unknown=>true}) of
                {ok, Parsed, _} ->
                    {ok, Api#{params:=Parsed}};
                {error, {syntax_error, Field}} ->
                    {error, {parameter_invalid, Field}};
                {error, Error} ->
                    ?LLOG(warning, "unexpected syntax error: ~p", [Error]),
                    {error, bad_request}
            end
    end.


%% @doc
params_syntax(get) ->
    #{
        activate => boolean,
        consume => boolean,
        ttl => {integer, 1, none}
    };

params_syntax(create) ->
    #{
        activate => boolean,
        ttl => {integer, 1, none}
    };

params_syntax(list) ->
    #{
        from => pos_integer,
        size => pos_integer,
        sort => binary,
        labelSelector => binary,
        fieldSelector => binary,
        linkedTo => binary,
        fts => binary,
        deep => boolean,
        totals => boolean
    };

params_syntax(delete) ->
    #{
        cascade => boolean
    };


params_syntax(deletecollection) ->
    #{
        from => pos_integer,
        size => pos_integer,
        sort => binary,
        labelSelector => binary,
        fieldSelector => binary,
        linkedTo => binary,
        fts => binary,
        deep => boolean
    };

params_syntax(watch) ->
    #{
        deep => boolean,
        kind => binary,
        resourceVersion => binary
    };


params_syntax(_Veb) ->
    #{}.



%% @private
api_obj_syntax() ->
    #{
        <<"apiVersion">> => binary,
        <<"kind">> => binary,
        <<"metadata">> => #{
            <<"name">> => binary,
            <<"domain">> => binary,
            <<"links">> => map,
            '__allow_unknown' => true
        },
        '__allow_unknown' => true
    }.


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).