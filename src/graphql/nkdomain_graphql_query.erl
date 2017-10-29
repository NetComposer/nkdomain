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
-module(nkdomain_graphql_query).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([execute/4]).

%% tag::execute[]
execute(Ctx, _DummyObj, <<"node">>, #{ <<"id">> := Id }) ->
    #{nkmeta:=#{start:=Start}} = Ctx,
    lager:notice("NKLOG T1 ~p", [nklib_util:l_timestamp()-Start]),
    case nkdomain:get_obj(Id) of
        {ok, Obj} ->
            lager:notice("NKLOG T2 ~p", [nklib_util:l_timestamp()-Start]),
            {ok, Obj};
        {error, Error} ->
            {error, Error}
    end.


%%execute(_Ctx, _DummyObj, <<"starship">>, #{ <<"id">> := ID }) ->
%%    load_node(['Starship'], ID);
%%execute(_Ctx, _DummyObj, <<"allPlanets">>, _Args) ->
%%    {atomic, Planets} = mnesia:transaction(load_all(planet)),
%%    {ok, Planets};
%%
%%execute(_Ctx, _DummyObj, <<"allStarships">>, _Args) ->
%%    {atomic, Starships} = mnesia:transaction(load_all(starship)),
%%    {ok, Starships};
%%execute(_Ctx, _DummyObj, <<"allPeople">>, _Args) ->
%%    {atomic, People} = mnesia:transaction(load_all(person)),
%%    {ok, People};
%%execute(_Ctx, _DummyObj, <<"allVehicles">>, _Args) ->
%%    {atomic, Vehicles} = mnesia:transaction(load_all(vehicle)),
%%    {ok, Vehicles};
%%execute(_Ctx, _DummyObj, <<"allSpecies">>, _Args) ->
%%    {atomic, Species} = mnesia:transaction(load_all(species)),
%%    {ok, Species};
%%execute(_Ctx, _DummyObj, <<"allFilms">>, _Args) ->
%%    {atomic, Films} = mnesia:transaction(load_all(film)),
%%    {ok, Films}.

%%load_all(vehicle) ->
%%    fun() ->
%%        QH = qlc:q([{ok, #{ vehicle => V, transport => T }} ||
%%                       V <- mnesia:table(vehicle),
%%                       T <- mnesia:table(transport),
%%                    V#vehicle.id == T#transport.id]),
%%        qlc:e(QH)
%%    end;
%%load_all(starship) ->
%%    fun() ->
%%        QH = qlc:q([{ok, #{ starship => S, transport => T }} ||
%%                       S <- mnesia:table(starship),
%%                       T <- mnesia:table(transport),
%%                    S#starship.id == T#transport.id]),
%%        qlc:e(QH)
%%    end;
%%load_all(Tab) ->
%%    fun() ->
%%        QH = qlc:q([{ok, F} || F <- mnesia:table(Tab)]),
%%        qlc:e(QH)
%%    end.


