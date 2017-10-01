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

%% tag::execute[]
execute(Ctx, _, Field, #{ <<"input">> := Input}) ->
    with_client_mutation(Ctx, Field, Input).

with_client_mutation(Ctx, Field, Input) ->
    {CM, Rest} = maps:take(<<"clientMutationId">>, Input),
    case execute_mutation(Ctx, Field, Rest) of
        {ok, Payload} ->
            {ok, Payload#{ <<"clientMutationId">> => CM }};
        {error, Reason} ->
            {error, Reason}
    end.
%% end::execute[]

%% tag::executeMutation[]
execute_mutation(Ctx, <<"introduceFaction">>, Input) ->
    {ok, Faction} = sw_core_faction:introduce(Ctx, Input),
    {ok, #{ <<"faction">> => Faction }};
execute_mutation(Ctx, <<"introduceStarship">>, Input) ->
    {ok, Faction, Starship} = sw_core_starship:introduce(Ctx, Input),
    {ok, #{ <<"faction">> => Faction,
            <<"starship">> => Starship }}.
%% end::executeMutation[]

