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

%% @doc Alert Object

-module(nkdomain_alert_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nkdomain.hrl").

-export([object_info/0, object_admin_info/0, object_schema_types/0,
         object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export_type([events/0]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Alert "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================


-type events() ::
    {}.


%% ===================================================================
%% API
%% ===================================================================


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        schema_type => 'Alert',
        type => ?DOMAIN_ALERT
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 100
    }.


%% @doc
object_schema_types() ->
    #{
        'Alert' => #{
            fields => #{
            },
            is_object => true,
            comment => "An Alert"
        }
    }.


% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_alert_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_send_event(Event, State) ->
    nkdomain_alert_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_alert_obj_cmd:cmd(Cmd, Req).





%% ===================================================================
%% Internal
%% ===================================================================
