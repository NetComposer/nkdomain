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

api(<<"get_name">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,                    % If domain and app_id, status is added
        app_id => binary                        %
    };

api(<<"add_push_device">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,
        app_id => binary,
        device_id => binary,
        push_data => #{
            push_id => binary,
            voip_push_id => binary,
            platform_id => binary,
            platform_version => binary,
            base_url => binary,
            device_manufacturer => binary,
            device_name => binary,
            device_os => binary,
            user_agent => binary
        },
        '__mandatory' => [app_id, device_id, push_data]
    };

api(<<"remove_push_device">>, Syntax) ->
    Syntax#{
        id => binary,
        device_id => binary,
        '__mandatory' => [device_id]
    };

api(<<"get_status">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,
        app_id => binary,
        '__mandatory' => [app_id]
    };

api(<<"set_status">>, Syntax) ->
    Syntax#{
        id => binary,
        domain_id => binary,
        app_id => binary,
        status => map,
        '__mandatory' => [app_id, status]
    };

api(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_USER, Syntax).
