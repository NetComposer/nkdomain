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

%% @doc NkDomain core class API processing
%% https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/
%% https://github.com/kubernetes/community/blob/master/contributors/devel/api-conventions.md

-module(nkdomain_api_core_v1).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/2]).

-include("nkdomain.hrl").
-include("nkdomain_api.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-define(DELETE_COLLECTION_SIZE, 1000000).

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Incoming API request called from nkdomain_api in callbacks
%% SrvId is the service receiving the request
%% SrvId must be able to understand the resource, so it must have the
%% corresponding plugins applied. Same for response codes.
%% When processing actors belonging to other services, only if they are activated,
%% they will use that other services

-spec request(nkservice:id(), nkdomain_api:request()) ->
    nkdomain_api:response().

request(SrvId, ApiReq) ->
    nkdomain_api_core:request(SrvId, ApiReq).

