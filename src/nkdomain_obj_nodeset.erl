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

-module(nkdomain_obj_nodeset).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj).

-export([init/2, load/4, remove/2]).


-type nodeset() ::
    nkdomain_obj:base_obj() |
	#{
        meta => binary()
	}.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(state, {}).


%% @private
-spec init(nkdomain:obj_id(), nodeset()) ->
    {ok, nkdomain_obj:init_opts(), nodeset(), #state{}}.

init(_NodesId, Nodeset) ->
    {ok, #{}, Nodeset, #state{}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), nodeset(), #state{}) ->
    {ok, nkdomain:obj(), #state{}} | removed | {error, term()}.

load(Data, _Opts, Nodeset, State) ->
    do_load(maps:to_list(Data), Nodeset, State).


%% @private
-spec remove(nodeset(), #state{}) ->
    ok.

remove(_Nodeset, _State) ->
    ok.
    

%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_load([], Nodeset, State) ->
    {ok, Nodeset, State};

do_load([{Key, Val}|Rest], Nodeset, State) ->
    do_load(Rest, maps:put(Key, Val, Nodeset), State).



