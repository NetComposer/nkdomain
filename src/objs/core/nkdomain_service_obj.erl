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

%% @doc Service Object

-module(nkdomain_service_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nkdomain.hrl").

-export([object_info/0, object_admin_info/0, object_es_mapping/0, object_parse/2]).
-export([object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export_type([events/0]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Service "++Txt, Args)).


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
        type => ?DOMAIN_SERVICE,
        default_ttl => permanent
    }.


%% @doc
object_admin_info() ->
    #{
        class => service,
        weight => 100
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        spec => #{enabled => false}
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
        vsn => binary,
        spec => map,
        '__defaults' => #{vsn => 1, spec => #{}}
    }.


% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_service_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_send_event(Event, State) ->
    nkdomain_service_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_service_obj_cmd:cmd(Cmd, Req).





%% ===================================================================
%% Internal
%% ===================================================================
