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

%% @doc Session Object Syntax

-module(nkdomain_session_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/2]).

-include("nkdomain.hrl").

%% ===================================================================
%% Syntax
%% ===================================================================

api(<<"start">>, Syntax) ->
    Syntax#{
        id => binary,
        password => binary,
        domain_id => binary,
        %device_id => binary,
        %push_id => binary,
        %platform_id => binary,
        %platform_version => binary,
        meta => map,
        '__mandatory' => [id]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_SESSION, Syntax).

