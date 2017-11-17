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
-module(nkadmin_session_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/2]).

-include_lib("nkadmin/include/nkadmin.hrl").


%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
%% TODO: To REMOVE
api(<<"create">>, Syntax) ->
    Syntax#{
        language => binary,             %% <<"en">>, <<"es">>
        url => binary
    };

api(<<"start">>, Syntax) ->
    Syntax#{
        domain_id => binary,
        language => binary,             %% <<"en">>, <<"es">>
        url => binary,
        events => {list, binary}
    };

api(<<"switch_domain">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,
        url => binary,
        '__mandatory' => [domain_id]
    };

api(<<"element_action">>, Syntax) ->
    Syntax#{
        id => binary,
        action => {atom, [selected, updated, enable, disable, delete, new, save]},
        element_id => binary,
        value => any,
        '__mandatory' => [element_id, action]
    };

api(<<"get_data">>, Syntax) ->
    Syntax#{
        id => binary,
        element_id => binary,
        start => {integer, 0, none},
        'end' =>  {integer, 0, none},
        filter => map,
        sort => #{
            id => binary,
            dir => {atom, [asc, desc]}
        },
        '__mandatory' => [element_id]
    };

api(<<"get_chart_data">>, Syntax) ->
    Syntax#{
        element_id => binary,
        '__mandatory' => [element_id]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_ADMIN_SESSION, Syntax).
