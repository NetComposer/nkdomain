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

-include("nkdomain.hrl").

%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
api('', login, Syntax) ->
    Syntax#{
        id => binary,
        password => binary,
        domain => binary,
        meta => map,
        '__mandatory' => [id]
    };

api('', get_token, Syntax) ->
    Syntax#{
        id => binary,
        password => binary,
        domain => binary,
        '__mandatory' => [id]
    };

api('', create, Syntax) ->
    Syntax#{
        obj_name => binary,
        ?DOMAIN_USER => #{
            name => binary,
            surname => binary,
            password => binary,
            email => email,
            '__mandatory' => [name, surname]
        },
        domain => binary
    };

api('', update, Syntax) ->
    Syntax#{
        id => binary,
        ?DOMAIN_USER => #{
            name => binary,
            surname => binary,
            password => binary,
            email => email
        }
    };

api(Sub, Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Sub, Cmd, Syntax).
