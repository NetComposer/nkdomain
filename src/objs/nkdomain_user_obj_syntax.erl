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
        ?DOMAIN_USER_ATOM => #{
            name => binary,
            surname => binary,
            password => binary,
            email => email
        },
        domain => binary
    },
    nklib_syntax:add_mandatory([
        obj_name,
        <<?DOMAIN_USER/binary, ".name">>,
        <<?DOMAIN_USER/binary, ".surname">>
    ], Syntax2);

api('', update, Syntax) ->
    Syntax#{
        id => binary,
        ?DOMAIN_USER_ATOM => #{
            name => binary,
            surname => binary,
            password => binary,
            email => email
        }
    };

%%api('', find_referred, Syntax) ->
%%    Syntax#{
%%        id => binary,
%%        type => binary
%%    };

api(Sub, Cmd, Syntax) ->
    nkdomain_api_util:syntax_common(Sub, Cmd, Syntax).
