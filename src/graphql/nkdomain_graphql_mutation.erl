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

%% @doc NkDomain main module
-module(nkdomain_graphql_mutation).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([execute/4]).

execute(Ctx, _, MutationName, #{<<"input">>:=Params}) ->
    #{nkmeta:=#{start:=Start}} = Ctx,
    case nklib_types:get_module(nkdomain_mutation, MutationName) of
        undefined ->
            {error, unknown_mutation};
        Module ->
            Params2 = maps:filter(fun(_K, V) -> V /= null end, Params),
            Res = Module:object_mutation(MutationName, Params2, Ctx),
            lager:info("Mutarion time: ~p", [nklib_util:l_timestamp()-Start]),
            Res
    end.

