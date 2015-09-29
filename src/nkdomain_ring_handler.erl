%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% TODO
%% - roles sólo se pueden reemplazar completos con load
%% - replace borra todo lo que no esté
%% - api especial para añadir o quitar roles
%% - mirar conflictos en riak core metadata (ver cómo accede riak)
%% - usar nkbase (y poner tokens)
%%
%%
%%





-module(nkdomain_ring_handler).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% gen_event callbacks
%% ===================================================================

-record(state, {}).

init([]) ->
    {ok, #state{}}.


handle_event({ring_update, _Ring}, State) ->
    % lager:warning("RING UPDATE"),
    {ok, State}.

handle_call(_Event, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

