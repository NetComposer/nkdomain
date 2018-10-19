%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain File Actor
%%
%% spec
%% ----
%% - name
%% - contentType
%% - provider
%% - size
%% - password
%% - hash
%%
%% info
%% ----
%%
%% -




-module(nkdomain_file_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_actor).

-export([op_get_body/2, op_get_download_link/2, op_get_media/2]).
-export([config/0, parse/3, request/5, sync_op/3]).


-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkservice/include/nkservice_actor_debug.hrl").



%% ===================================================================
%% Operations
%% ===================================================================

%% @doc
op_get_body(SrvId, Id) ->
    nkservice_actor_srv:sync_op(SrvId, Id, nkdomain_get_body, 60000).


%% @doc
op_get_download_link(SrvId, Id) ->
    nkservice_actor_srv:sync_op(SrvId, Id, nkdomain_get_download_link).


op_get_media(SrvId, Id) ->
    case nkservice_actor_db:activate(SrvId, Id, #{}) of
        {ok, #actor_id{group=?GROUP_CORE, resource=?RES_CORE_FILES}=ActorId, _} ->
            nkservice_actor_srv:sync_op(SrvId, ActorId, nkdomain_get_media);
        {ok, _} ->
            {error, actor_not_found};
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_FILES,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch, upload],
        filter_fields => [
            <<"spec.name">>,
            <<"spec.size">>,
            <<"spec.contentType">>,
            <<"spec.externalId">>
        ],
        sort_fields => [
            <<"spec.name">>,
            <<"spec.size">>,
            <<"spec.contentType">>
        ],
        field_type => #{
            <<"spec.size">> => integer
        },
        short_names => [],
        immutable_fields => [
            <<"spec.contentType">>,
            <<"spec.provider">>,
            <<"spec.externalId">>,
            <<"spec.size">>,
            <<"spec.hash">>,
            <<"spec.password">>
        ]
    }.


%% @doc
parse(SrvId, Actor, #{verb:=create}=ApiReq) ->
    Syntax = #{
        <<"spec">> => #{
            <<"contentType">> => binary,
            <<"provider">> => binary,
            <<"bodyBase64">> => base64,
            <<"bodyBinary">> => binary,
            <<"externalId">> => binary,
            <<"url">> => binary
        },
        '__mandatory' => [<<"spec">>]
    },
    case nkdomain_actor_util:parse_actor(Actor, Syntax, ApiReq) of
        {ok, Actor2} ->
            #actor{data=#{<<"spec">>:=Spec2}}=Actor2,
            #{params:=Params} = ApiReq,
            do_parse(SrvId, Params, Spec2, Actor);
        {error, Error} ->
            {error, Error}
    end;

%% We allow fields in case they didn't change
%% immutable_fields makes sure no one is changed
parse(_SrvId, Actor, #{verb:=update}=ApiReq) ->
    Syntax = #{
        <<"spec">> => #{
            <<"contentType">> => binary,
            <<"provider">> => binary,
            <<"externalId">> => binary,
            <<"hash">> => binary,
            <<"size">> => integer,
            <<"password">> => binary
        },
        '__mandatory' => [<<"spec">>]
    },
    nkdomain_actor_util:parse_actor(Actor, Syntax, ApiReq).


%% @doc
request(SrvId, get, ActorId, _Config, #{subresource:=[]}=ApiReq) ->
    #{params:=Params, vsn:=Vsn} = ApiReq,
    case nkservice_actor:get_actor(SrvId, ActorId) of
        {ok, #actor{data=Data}=Actor} ->
            case nklib_syntax:parse(Params, #{getBodyInline=>boolean}) of
                {ok, #{getBodyInline:=true}, _} ->
                    case op_get_body(SrvId, ActorId) of
                        {ok, _CT, Body} ->
                            #{<<"spec">>:=Spec} = Data,
                            Body2 = base64:encode(Body),
                            Spec2 = Spec#{<<"bodyBase64">> => Body2},
                            Actor2 = Actor#actor{data=Data#{<<"spec">>:=Spec2}},
                            case nkdomain_api:actor_to_external(SrvId, Actor2, Vsn) of
                                {ok, ApiActor} ->
                                    {ok, ApiActor};
                                {error, ApiActorError} ->
                                    {error, ApiActorError}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    case nkdomain_api:actor_to_external(SrvId, Actor, Vsn) of
                        {ok, ApiActor} ->
                            {ok, ApiActor};
                        {error, ApiActorError} ->
                            {error, ApiActorError}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end;

request(SrvId, upload, _ActorId, _Config, #{subresource:=[], params:=Params}=ApiReq) ->
    case Params of
        #{<<"provider">>:=Provider} ->
            case ApiReq of
                #{
                    body := Body,
                    meta := #{nkdomain_http_content_type:=CT}
                } ->
                    Body2 = #{
                        <<"spec">> => #{
                            <<"contentType">> => CT,
                            <<"bodyBinary">> => Body,
                            <<"provider">> => Provider
                        }
                    },
                    ApiReq2 = ApiReq#{
                        verb := create,
                        body := Body2,
                        subresource := []
                    },
                    nkdomain_api_core:request(SrvId, ApiReq2);
                _ ->
                    {error, request_body_invalid}
            end;
        _ ->
            {error, {field_missing, <<"provider">>}}
    end;

request(SrvId, get, ActorId, _Config, #{subresource:=[<<"_download">>]}) ->
    case op_get_body(SrvId, ActorId) of
        {ok, CT, Body} ->
            {raw, {CT, Body}};
        {error, Error} ->
            {error, Error}
    end;

request(SrvId, get, ActorId, _Config, #{subresource:=[<<"_downloadLink">>]}) ->
    case op_get_download_link(SrvId, ActorId) of
        {ok, Url, 0} ->
            {ok, #{url=>Url}};
        {ok, Url, TTL} ->
            {ok, #{url=>Url, ttlSecs=>TTL}};
        {error, Error} ->
            {error, Error}
    end;

request(_SrvId, _Verb, _ActorId, _Config, _Api) ->
    continue.


%% @doc
sync_op(nkdomain_get_body, _From, ActorSt) ->
    #actor_st{srv=SrvId, actor=#actor{data=Data, metadata=Meta}} = ActorSt,
    Links = maps:get(<<"links">>, Meta),
    ProviderUID = maps:get(?RES_CORE_FILE_PROVIDERS, Links),
    #{<<"spec">>:=#{<<"contentType">>:=CT, <<"externalId">>:=Id}=Spec} = Data,
    FileMeta1 = #{
        name => Id,
        content_type => ct1
    },
    FileMeta2 = case Spec of
        #{<<"password">>:=Pass} ->
            FileMeta1#{password => Pass};
        _ ->
            FileMeta1
    end,
    FileMeta3 = case Spec of
        #{<<"hash">>:=Hash} ->
            FileMeta2#{hash => Hash};
        _ ->
            FileMeta2
    end,
    Reply = case nkdomain_file_provider_actor:op_get_spec(SrvId, ProviderUID) of
        {ok, _ProvActorId, ProviderSpec} ->
            case nkfile:download(SrvId, ?DOMAIN_PKG_ID_FILE, ProviderSpec, FileMeta3) of
                {ok, Bin, _DownMeta} ->
                    {ok, CT, Bin};
                {error, file_not_found} ->
                    {error, file_read_error};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            ?ACTOR_LOG(warning, "FILE provider error: ~p", [Error]),
            {error, provider_error}
    end,
    {reply, Reply, ActorSt};

sync_op(nkdomain_get_download_link, _From, ActorSt) ->
    #actor_st{srv=SrvId, actor=#actor{id=ActorId, data=Data}=Actor} = ActorSt,
    #{<<"spec">>:=#{<<"externalId">>:=Id}} = Data,
    {ok, ProvUID} = nkdomain_actor_util:get_link(?GROUP_CORE, ?RES_CORE_FILE_PROVIDERS, Actor),
    Reply = case nkdomain_file_provider_actor:op_get_direct_download_link(SrvId, ProvUID, Id) of
        {ok, Link, TTL} ->
            {ok, Link, TTL};
        {error, storage_class_incompatible} ->
            case nkdomain_actor_util:get_public_self(SrvId, ActorId, ?GROUP_CORE_V1A1) of
                undefined ->
                    {error, no_public_address};
                Url ->
                    {ok, <<Url/binary, "/_download">>, 0}
            end;
        {error, Error} ->
            {error, Error}
    end,
    {reply, Reply, ActorSt};

sync_op(nkdomain_get_media, _From, #actor_st{actor=Actor}=ActorSt) ->
    #actor{id=#actor_id{uid=UID, name=Name}=ActorId, data=Data} = Actor,
    #{<<"spec">>:=#{<<"contentType">>:=CT, <<"size">>:=Size}} = Data,
    Media = #{
        <<"fileId">> => UID,
        <<"contentType">> => CT,
        <<"size">> => Size,
        <<"name">> => Name
    },
    {reply, {ok, ActorId, Media}, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_parse(SrvId, #{<<"provider">>:=Provider}, Spec, Actor) ->
    do_parse(SrvId, #{}, Spec#{<<"provider">>=>Provider}, Actor);

do_parse(SrvId, _Params, #{<<"provider">>:=ProviderId}=Spec, Actor) ->
    ProviderPath = nkdomain_api_lib:api_path_to_actor_path(ProviderId),
    case nkdomain_file_provider_actor:op_get_spec(SrvId, ProviderPath) of
        {ok, ProvActorId, ProvSpec} ->
            case ProvActorId of
                #actor_id{group=?GROUP_CORE, resource=?RES_CORE_FILE_PROVIDERS} ->
                    Actor2 = nkdomain_actor_util:add_link(Actor, ProvActorId),

                    do_parse_upload(SrvId, Spec, ProvActorId, ProvSpec, Actor2);
                _ ->
                    {error, {provider_unknown, ProviderId}}
            end;
        {error, _} ->
            {error, {provider_unknown, ProviderId}}
    end;

do_parse(_SrvId, _Params, _Spec, _Actor) ->
    {error, {field_missing, <<"spec.provider">>}}.


%% @private
do_parse_upload(SrvId, #{<<"bodyBase64">>:=Bin}=Spec, ProvActorId, ProvSpec, Actor) ->
    Spec2 = maps:remove(<<"bodyBase64">>, Spec),
    do_parse_upload(SrvId, Spec2#{<<"bodyBinary">>=>Bin}, ProvActorId, ProvSpec, Actor);

do_parse_upload(SrvId, #{<<"url">>:=Url}=Spec, ProvActorId, ProvSpec, Actor) ->
    case nkfile_util:get_url(ProvActorId, ProvSpec, Url) of
        {ok, CT, Body} ->
            Spec2 = maps:remove(<<"url">>, Spec),
            Spec3 = Spec2#{<<"bodyBinary">>=>Body, <<"contentType">>=>CT},
            do_parse_upload(SrvId, Spec3, ProvActorId, ProvSpec, Actor);
        {error, Error} ->
            {error, Error}
    end;

do_parse_upload(SrvId, #{<<"bodyBinary">>:=Bin, <<"contentType">>:=CT}=Spec, _ProvActorId, ProvSpec, Actor) ->
    #actor{id=#actor_id{uid=UID}, data=Data} = Actor,
    FileMeta1 = #{name => UID, contentType => CT},
    case nkfile:upload(SrvId, ?DOMAIN_PKG_ID_FILE, ProvSpec, FileMeta1, Bin) of
        {ok, FileMeta2, _UpMeta} ->
            #{size:=Size} = FileMeta2,
            Spec1 = maps:remove(<<"bodyBinary">>, Spec),
            Spec2 = Spec1#{
                <<"size">> => Size,
                <<"externalId">> => UID
            },
            Spec3 = case FileMeta2 of
                #{password:=Pass} ->
                    Spec2#{<<"password">> => Pass};
                _ ->
                    Spec2
            end,
            Spec4 = case FileMeta2 of
                #{hash:=Hash} ->
                    Spec3#{<<"hash">> => Hash};
                _ ->
                    Spec3
            end,
            Data2 = Data#{<<"spec">>:=Spec4},
            {ok, Actor#actor{data=Data2}};
        {error, Error} ->
            {error, Error}
    end;

do_parse_upload(_SrvId, #{<<"bodyBinary">>:=_}, _ProvActorId, _ProvSpec, _Actor) ->
    {error, {field_missing, <<"spec.contentType">>}};

do_parse_upload(SrvId, #{<<"externalId">>:=Id, <<"contentType">>:=CT}=Spec, ProvActorId, _ProvSpec, Actor) ->
    case nkdomain_file_provider_actor:op_get_check_meta(SrvId, ProvActorId, Id) of
        {ok, #{contentType:=CT, size:=Size}} ->
            #actor{data=Data} = Actor,
            Data2 = Data#{<<"spec">> => Spec#{<<"size">> => Size}},
            {ok, Actor#actor{data=Data2}};
        {ok, _} ->
            {error, content_type_invalid};
        {error, Error} ->
            {error, Error}
    end;

do_parse_upload(_SrvId, #{<<"externalId">>:=_}, _ProvActorId, _ProvSpec, _Actor) ->
    {error, {field_missing, <<"spec.contentType">>}};

do_parse_upload(_SrvId, _Spec, _ProvActorId, _ProvSpec, _Actor) ->
    {error, {field_missing, <<"bodyBase64">>}}.




%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).