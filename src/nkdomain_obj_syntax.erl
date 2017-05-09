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

-module(nkdomain_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([syntax/3]).

-include("nkdomain.hrl").



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
syntax('', get, Syntax) ->
    Syntax#{
        id => binary
    };

syntax('', enable, Syntax) ->
    Syntax#{
        id => binary,
        enable => boolean,
        '__mandatory' => [enable]
    };

syntax('', delete, Syntax) ->
    Syntax#{
        id => binary,
        delete_childs => boolean
    };

syntax('', find, Syntax) ->
    nkdomain_obj_util:search_syntax(Syntax);

syntax('', find_all, Syntax) ->
    syntax('', find, Syntax);

syntax('', wait_for_save, Syntax) ->
    Syntax#{
        id => binary,
        time => {integer, {1, none}}
    };

syntax('', make_token, Syntax) ->
    Syntax#{
        ttl => {integer, 1, none}
    };

syntax(_Sub, _Cmd, Syntax) ->
    lager:info("~p: unknown syntax: ~p, ~p", [?MODULE, _Sub, _Cmd]),
    Syntax.


