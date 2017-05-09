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

%% @doc Config Object Syntax

-module(nkdomain_config_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/3]).

-include("nkdomain.hrl").



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', create, Syntax) ->
    Syntax#{
        obj_name => binary,
        type => binary,
        parent => binary,
        ?DOMAIN_CONFIG => map,
        '__mandatory' => [subtype, parent, ?DOMAIN_CONFIG]
    };

api('', update, Syntax) ->
    Syntax#{
        id => binary,
        ?DOMAIN_CONFIG => map,
        '__mandatory' => [id, ?DOMAIN_CONFIG]
    };

api('', find, Syntax) ->
    Syntax#{
        parent => binary,
        subtype => binary,
        '__mandatory' => [parent, subtype]
    };

api(Sub, Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Sub, Cmd, ?DOMAIN_CONFIG, Syntax).