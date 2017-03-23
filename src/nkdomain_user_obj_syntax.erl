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

-module(nkdomain_user_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/3]).



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', login, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        password => binary,
        domain => binary,
        meta => map
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', get_token, Syntax) ->
    Syntax2 = Syntax#{
        id => binary,
        password => binary,
        domain => binary
    },
    nklib_syntax:add_mandatory([id], Syntax2);

api('', create, Syntax) ->
    Syntax2 = Syntax#{
        obj_name => binary,
        user => #{
            name => binary,
            surname => binary,
            password => binary,
            email => email
        },
        domain => binary
    },
    nklib_syntax:add_mandatory([obj_name, 'user.name', 'user.surname'], Syntax2);

api('', delete, Syntax) ->
    Syntax#{
        id => binary,
        reason => binary
    };

api('', update, Syntax) ->
    Syntax#{
        id => binary,
        user => #{
            name => binary,
            surname => binary,
            password => binary,
            email => email
        }
    };

api('', find_referred, Syntax) ->
    Syntax#{
        id => binary,
        type => binary
    };

api(_Sub, _Cmd, Syntax) ->
    lager:error("unknown syntax: ~p, ~p", [_Sub, _Cmd]),
    Syntax.
