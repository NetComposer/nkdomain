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

-module(nkdomain_api_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/4]).


%% ===================================================================
%% API callbacks
%% ===================================================================

request(Method, Path, Req, {base_srv, SrvId}) ->
    Path2 = case lists:reverse(Path) of
        [<<>>|Rest] ->
            lists:reverse(Rest);
        _ ->
            Path
    end,
    nkdomain_api_http:rest_api(SrvId, Method, Path2, Req).

