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

%% @doc File Object Syntax

-module(nkdomain_file_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/2]).

-include("nkdomain.hrl").

%% ===================================================================
%% Syntax
%% ===================================================================

api(<<"create">>, Syntax) ->
    Syntax#{
        name => binary,
        ?DOMAIN_FILE => #{
            content_type => binary,
            store_id => binary,
            body => base64,
            '__mandatory' => [content_type, body]
        },
        '__mandatory' => [name]
    };

api(<<"get_inline">>, Syntax) ->
    Syntax#{
        id => binary,
        '__mandatory' => [id]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_FILE, Syntax).
