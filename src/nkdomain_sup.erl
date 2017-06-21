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

%% @private Main supervisor
-module(nkdomain_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(supervisor).

-export([init/1, start_link/0]).

-include("nkdomain.hrl").


%% @private
start_link() ->
    ChildsSpec = [
        {nkdomain_types_sup,
            {nkdomain_types_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [nkdomain_types_sup]},
        {nkdomain_all_types,
            {nkdomain_all_types, start_link, []},
            permanent,
            5000,
            worker,
            [nkdomain_all_types]}
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, ChildsSpec}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


