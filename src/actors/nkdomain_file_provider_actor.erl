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

%% @doc NkDomain File Provider Actor
%%
%% spec
%% ----


-module(nkdomain_file_provider_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([op_get_spec/2, op_get_direct_download_link/3, op_get_upload_link/3,
         op_get_check_meta/3]).
-export([link_to_provider/3]).
-export([config/0, parse/3, request/3, make_external/3, init/2, update/2, sync_op/3]).
-export_type([run_state/0]).


-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor_debug.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

%%-define(UPLOAD_LINK_TTL_SECS, 5*60).


%% ===================================================================
%% Types
%% ===================================================================

-type run_state() :: #{
        provider_spec := nkfile:provider_spec()
    }.



%% ===================================================================
%% Operations
%% ===================================================================

%% @doc
-spec op_get_spec(nkservice:id(), nkservice_actor:id()) ->
    {ok, #actor_id{}, map()} | {error, term()}.

op_get_spec(SrvId, Id) ->
    nkservice_actor_srv:sync_op({SrvId, Id}, nkdomain_get_spec).


%% @doc
op_get_direct_download_link(SrvId, Id, ExternalId) ->
    nkservice_actor_srv:sync_op({SrvId, Id}, {nkdomain_get_direct_download_link, ExternalId}).


%% @doc
op_get_upload_link(SrvId, Id, CT) ->
    nkservice_actor_srv:sync_op({SrvId, Id}, {nkdomain_get_upload_link, CT}).


%% @doc
op_get_check_meta(SrvId, Id, ExternalId) ->
    nkservice_actor_srv:sync_op({SrvId, Id}, {nkdomain_check_meta, ExternalId}).


%% @private
link_to_provider(SrvId, ApiId, Actor) ->
    ProviderPath = nkdomain_api_lib:api_path_to_actor_path(ApiId),
    case nkservice_actor:activate({SrvId, ProviderPath}) of
        {ok, #actor_id{group=?GROUP_CORE, resource=?RES_CORE_FILE_PROVIDERS}=ProvActorId, _} ->
            LinkType = nkdomain_actor_util:link_type(?GROUP_CORE, ?LINK_CORE_FILE_PROVIDER),
            {ok, nkdomain_actor_util:add_link(ProvActorId, LinkType, Actor)};
        _ ->
            {error, {provider_invalid, ApiId}}
    end.


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_FILE_PROVIDERS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch, upload],
        short_names => [],
        camel => <<"FileProvider">>,
        permanent => true,
        filter_fields => [
            <<"spec.storageClass">>
        ],
        sort_fields => [
            <<"spec.storageClass">>
        ],
        immutable_fields => [
            <<"spec.storageClass">>,
            <<"spec.encryptionAlgo">>,
            <<"spec.hashAlgo">>,
            <<"spec.s3Config.bucket">>,
            <<"spec.s3Config.path">>,
            <<"spec.filesystemConfig.filePath">>
        ]
    }.


%% @doc
parse(_SrvId, Actor, _ApiReq) ->
    Syntax = #{
        <<"spec">> => #{
            <<"storageClass">> => {binary, [<<"filesystem">>, <<"s3">>]},
            <<"maxSize">> => {integer, 0, none},
            <<"encryptionAlgo">> => {binary, [<<"aes_cfb128">>]},
            <<"hashAlgo">> => {binary, [<<"sha256">>]},
            <<"directDownload">> => boolean,
            <<"directUpload">> => boolean,
            <<"directDownloadSecs">> => pos_integer,
            <<"directUploadSecs">> => pos_integer,
            <<"filesystemConfig">> => #{
                <<"filePath">> => binary,
                '__mandatory' => [<<"filePath">>]
            },
            <<"s3Config">> => #{
                <<"region">> => binary,
                <<"key">> => binary,
                <<"secret">> => binary,
                <<"bucket">> => binary,
                <<"path">> => binary,
                <<"scheme">> => {atom, [http, https]},
                <<"host">> => binary,
                <<"port">> => pos_integer,
                '__mandatory' => [<<"key">>, <<"secret">>,<<"bucket">>]
            },
            '__mandatory' => [<<"storageClass">>]
        },
        '__mandatory' => [<<"spec">>]
    },
    case nkservice_actor_util:parse_actor(Actor, Syntax) of
        {ok, #actor{data=Data2}=Actor2} ->
            case Data2 of
                #{<<"spec">>:=#{<<"storageClass">>:=<<"filesystem">>}=Spec2} ->
                    case maps:is_key(<<"filesystemConfig">>, Spec2) of
                        true ->
                            {ok, Actor2};
                        false ->
                            {error, {field_missing, <<"spec.filesystemConfig">>}}
                    end;
                #{<<"spec">>:=#{<<"storageClass">>:=<<"s3">>}=Spec2} ->
                    case maps:is_key(<<"s3Config">>, Spec2) of
                        true ->
                            {ok, Actor2};
                        false ->
                            {error, {field_missing, <<"spec.s3Config">>}}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
%% Redirect to files, adding parameter
request(SrvId, ActorId, #{verb:=Verb, subresource:=[<<"files">>|Rest], params:=Params}=ApiReq)
        when Verb==create; Verb==upload ->
    #{params:=Params, vsn:=Vsn}=ApiReq,
    case nkservice_actor:get_actor({SrvId, ActorId}) of
        {ok, Actor} ->
            case nkdomain_api:actor_to_external(SrvId, Actor, Vsn) of
                {ok, #{<<"metadata">>:=#{<<"selfLink">>:=ProviderId}}} ->
                    ApiReq2 = ApiReq#{
                        resource := <<"files">>,
                        subresource := Rest,
                        params := Params#{<<"provider">> => ProviderId}
                    },
                    ApiReq3 = maps:remove(name, ApiReq2),
                    nkdomain_api_core:request(SrvId, ApiReq3);
                {error, ApiActorError} ->
                    {error, ApiActorError}
            end;
        {error, Error} ->
            {error, Error}
    end;

request(SrvId, ActorId, #{verb:=get, subresource:=[<<"_rpc">>, <<"uploadLink">>], params:=Params}) ->
    Syntax = #{contentType => binary},
    case nklib_syntax:parse(Params, Syntax) of
        {ok, #{contentType:=CT}, _} ->
            case op_get_upload_link(SrvId, ActorId, CT) of
                {ok, Method, Url, Name, TTL} ->
                    {ok, #{method=>Method, url=>Url, id=>Name, ttlSecs=>TTL}};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {field_missing, contentType}}
    end;

request(_SrvId, _ActorId, _ApiReq) ->
    continue.


%% @doc
make_external(_SrvId, #actor{data=Data}=Actor, _Vsn) ->
    % This function can be called with 'fake' empty actors
    Spec = maps:get(<<"spec">>, Data, #{}),
    case maps:find(<<"s3Config">>, Spec) of
        {ok, SpecS3} ->
            SpecS3B = SpecS3#{<<"secret">>=><<>>},
            Spec2 = Spec#{<<"s3Config">>:=SpecS3B},
            {ok, Actor#actor{data=Data#{<<"spec">>:=Spec2}}};
        _ ->
            {ok, Actor}
    end.


%% @doc
init(_Op, #actor_st{actor=Actor}=ActorSt) ->
    set_spec_cache(Actor, ActorSt).


%% @doc
update(NewActor, ActorSt) ->
    case set_spec_cache(NewActor, ActorSt) of
        {ok, ActorSt2} ->
            {ok, NewActor, ActorSt2};
        {error, Error} ->
            {error, Error, ActorSt}
    end.


%% @doc
sync_op(nkdomain_get_spec, _From, #actor_st{actor=Actor, run_state=RunState}=ActorSt) ->
    #actor{id=ActorId} = Actor,
    #{provider_spec:=Spec} = RunState,
    {reply, {ok, ActorId, Spec}, ActorSt};

sync_op({nkdomain_get_upload_link, CT}, _From, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    Name = nklib_util:luid(),
    FileMeta = #{name=>Name, contentType=>nklib_util:to_binary(CT)},
    case nkfile:make_upload_link(SrvId, ?DOMAIN_PKG_ID_FILE, Spec, FileMeta) of
        {ok, Verb, Url, TTL} ->
            {reply, {ok, Verb, Url, Name, TTL}, ActorSt};
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op({nkdomain_get_direct_download_link, Id}, _From, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    FileMeta = #{name=>Id},
    case nkfile:make_download_link(SrvId, ?DOMAIN_PKG_ID_FILE, Spec, FileMeta) of
        {ok, <<"GET">>, Url, TTL} ->
            {reply, {ok, Url, TTL}, ActorSt};
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op({nkdomain_check_meta, Id}, _From, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    case nkfile:check_file_meta(SrvId, ?DOMAIN_PKG_ID_FILE, Spec, Id) of
        {ok, Meta} ->
            {reply, {ok, Meta}, ActorSt};
        {error, file_too_large} ->
            case nkfile:delete(SrvId, ?DOMAIN_PKG_ID_FILE, Spec, #{name=>Id}) of
                ok ->
                    ?ACTOR_LOG(notice, "deleted file too large: ~p", [Id], ActorSt);
                {error, Error} ->
                    ?ACTOR_LOG(warning, "could not delete file too large ~p: ~p", [Id, Error], ActorSt)
            end,
            {reply, {error, file_too_large}, ActorSt};
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op(_Op, _From, _ActorSt) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
set_spec_cache(Actor, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #actor{id=#actor_id{uid=UID}, data=Data} = Actor,
    Spec1 = maps:get(<<"spec">>, Data),
    Spec2 = Spec1#{id => UID},
    case nkfile:parse_provider_spec(SrvId, ?DOMAIN_PKG_ID_FILE, Spec2) of
        {ok, Spec3} ->
            RunState1 = case RunState of
                undefined ->
                    #{};
                _ ->
                    RunState
            end,
            RunState2 = RunState1#{provider_spec => Spec3},
            {ok, ActorSt#actor_st{run_state=RunState2}};
        {error, Error} ->
            ?ACTOR_LOG(warning, "could not parse provider spec: ~p: ~p", [Spec2, Error]),
            {error, provider_spec_invalid}
    end.



%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).