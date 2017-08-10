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
syntax(<<"create">>, Type, Syntax) ->
    Syntax#{
        obj_name => binary,
        domain_id => binary,
        parent_id => binary,
        enabled => boolean,
        name => binary,
        description => binary,
        tags => {list, binary},
        aliases => {list, binary},
        icon_id => binary,
        Type => map,
        '__defaults' => #{Type => #{}}
    };

syntax(<<"update">>, Type, Syntax) ->
    Syntax#{
        id => binary,
        enabled => boolean,
        name => binary,
        tags => {list, binary},
        description => binary,
        aliases => {list, binary},
        icon_id => binary,
        Type => map,
        '__defaults' => #{Type => #{}},
        '__mandatory' => [id]
    };

syntax(<<"update_obj_name">>, _Type, Syntax) ->
    Syntax#{
        id => binary,
        obj_name => binary,
        '__mandatory' => [id, obj_name]
    };

syntax(<<"get">>, _Type, Syntax) ->
    Syntax#{
        id => binary
    };

syntax(<<"get_info">>, _Type, Syntax) ->
    Syntax#{
        id => binary
    };

syntax(<<"get_name">>, _Type, Syntax) ->
    Syntax#{
        id => binary
    };

syntax(<<"enable">>, _Type, Syntax) ->
    Syntax#{
        id => binary,
        enable => boolean,
        '__mandatory' => [enable]
    };

syntax(<<"stop">>, _Type, Syntax) ->
    Syntax#{
        id => binary
    };

syntax(<<"delete">>, _Type, Syntax) ->
    Syntax#{
        id => binary,
        delete_childs => boolean
    };

syntax(<<"find">>, _Type, Syntax) ->
    nkdomain_obj_util:search_syntax(Syntax);

syntax(<<"find_all">>, Type, Syntax) ->
    syntax(<<"find">>, Type, Syntax);

syntax(<<"make_token">>, _Type, Syntax) ->
    Syntax#{
        ttl => {integer, 1, none}
    };

syntax(_Cmd, _Type, Syntax) ->
    lager:info("~p: unknown syntax: ~p, ~p", [?MODULE, _Cmd, _Type]),
    Syntax.


