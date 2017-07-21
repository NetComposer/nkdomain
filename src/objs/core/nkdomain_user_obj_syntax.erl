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

-export([api/2]).

-include("nkdomain.hrl").


%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc
%% TODO to remove
api(<<"login">>, Syntax) ->
    nkdomain_session_obj:object_api_syntax(<<"start">>, Syntax);

api(<<"get_token">>, Syntax) ->
    Syntax#{
        id => binary,
        password => binary,
        domain_id => binary,
        meta => map,
        ttl => integer,             % Only for http
        '__mandatory' => [id]
    };

api(<<"update_status">>, Syntax) ->
    Syntax#{
        id => binary,
        session_id => binary,
        status => binary,
        '__mandatory' => [session_id, status]
    };

api(<<"create_notify">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,
        session_type => binary,
        msg => map,
        ttl => {integer, 1, none},
        '__mandatory' => [session_type, msg]
    };

api(<<"remove_notify">>, Syntax) ->
    Syntax#{
        id => binary,
        notify_id => binary,
        '__mandatory' => [notify_id]
    };

api(<<"add_push_device">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,
        session_type => binary,
        device_id => binary,
        push_data => map,
        '__mandatory' => [session_type, device_id, push_data]
    };

api(<<"remove_push_device">>, Syntax) ->
    Syntax#{
        id => binary,
        device_id => binary,
        '__mandatory' => [device_id]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_USER, Syntax).
