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

%% @doc User Object Syntax

-module(nkdomain_domain_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/3]).



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', create, Syntax) ->
    Syntax2 = Syntax#{
        path => binary,
        description => binary,
        aliases => {list, binary}
    },
    nklib_syntax:add_mandatory([path], Syntax2);

api('', delete, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        reason => binary
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', update, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        description => binary,
        aliases => {list, binary}
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', get_types, Syntax) ->
    Syntax2 = Syntax#{
        id => binary
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', get_all_types, Syntax) ->
    api('', get_types, Syntax);

api('', get_childs, Syntax) ->
    Search = nkelastic_search:syntax(),
    Syntax2 = Syntax#{
        id => binary,
        type => binary
    },
    Syntax3 = maps:merge(Syntax2, Search),
    nklib_syntax:add_mandatory([id], Syntax3);

api('', get_all_childs, Syntax) ->
    api('', get_childs, Syntax);

api(_Sub, _Cmd, Syntax) ->
    lager:error("unknown syntax: ~p, ~p", [_Sub, _Cmd]),
    Syntax.
