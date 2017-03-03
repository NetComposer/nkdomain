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

%% @doc Service Objects
%% Services must have a class that is previously registered
-module(nkdomain_obj_service).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj2).

-export([init/2, load/4, removed/2]).


-type service() ::
    nkdomain_obj2:base_obj() |
	#{
        class => atom(),
        term() => term()
	}.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(state, {
    id :: nkdomain:obj_id()
 }).


%% @private
-spec init(service(), service()) ->
    {ok, nkdomain_obj2:init_opts(), service(), #state{}}.

init(ServiceId, Service) ->
    {ok, #{}, Service, #state{id=ServiceId}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), service(), #state{}) ->
    {ok, nkdomain:obj(), #state{}} | {removed, #state{}} | {error, term(), #state{}}.

load(Data, _Opts, Service, State) ->
    do_load(maps:to_list(Data), Service, State).


%% @private
-spec removed(service(), #state{}) ->
    ok.

removed(#{class:=Class}, #state{id=ServiceId}) ->
    catch Class:nkdomain_removed(ServiceId),
    ok.
   


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_load([], Service, State) ->
    {ok, Service, State};

do_load([{Key, Val}|Rest], Service, State) ->
    do_load(Rest, maps:put(Key, Val, Service), State).



