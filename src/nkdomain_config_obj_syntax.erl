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



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', create, Syntax) ->
    Syntax2 = Syntax#{
        obj_name => binary,
        subtype => binary,
        parent => binary,
        config => map
    },
    nklib_syntax:add_mandatory([subtype, parent, config], Syntax2);

api('', get, Syntax) ->
    Syntax2 = Syntax#{
        id => binary
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', delete, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        reason => binary
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', update, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        config => map
    },
    nklib_syntax:add_mandatory([id, config], Syntax2);

api('', find, Syntax) ->
    Syntax2 = Syntax#{
        parent => binary,
        subtype => binary
    },
    nklib_syntax:add_mandatory([parent, subtype], Syntax2);

api(_Sub, _Cmd, Syntax) ->
    lager:error("unknown syntax: ~p, ~p", [_Sub, _Cmd]),
    Syntax.
