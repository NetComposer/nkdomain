%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDOMAIN queries for PGSQL
-module(nkdomain_queries_pgsql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([get_query/3]).

-include("nkdomain.hrl").

%% ===================================================================
%% Core queries
%% ===================================================================

% Params: deep
get_query(SrvId, {domain_find_event_version, Domain, Kind, Name, Hash, Params}, _Opts) ->
    Query = [
        <<"SELECT uid,metadata->>'creationTime' FROM actors">>,
        <<" WHERE ">>, nkservice_actor_queries_pgsql:filter_path(Domain, Params),
        <<" AND \"group\"=">>, quote(?GROUP_CORE),
        <<" AND resource=">>, quote(?RES_CORE_EVENTS),
        <<" AND hash=">>, quote(Hash),
        case Kind of
            all ->
                <<>>;
            _ ->
                [<<" AND data->'involvedObject'->>'kind'=">>, quote(Kind)]
        end,
        case Name of
            all ->
                <<>>;
            _ ->
                [<<" AND data->'involvedObject'->>'name'=">>, quote(Name)]
        end,
        <<" ORDER BY metadata->>'creationTime' DESC;">>
    ],
    {ok, {pgsql, Query, #{result_fun=>fun pgsql_version/2, srv=>SrvId}}};

% Params: See service_search_actors
get_query(SrvId, {domain_search_events_from_version, Kind, Name, UID, Date, Params}, _Opts) ->
    Filters1 = maps:get(filter, Params, #{}),
    AndFilters1 = maps:get('and', Filters1, []),
    AndFilters2 = [
        #{field=><<"group">>, op=>eq, value=>?GROUP_CORE},
        #{field=><<"resource">>, op=>eq, value=>?RES_CORE_EVENTS},
        #{field=><<"uid">>, op=>ne, value=>UID},
        #{field=><<"metadata.creationTime">>, op=>gte, value=>Date}
        | AndFilters1
    ],
    AndFilters3 = case Kind of
        all ->
            AndFilters2;
        _ ->
            [#{field=><<"metadata.involvedObject.kind">>, value=>Kind} | AndFilters2]
    end,
    AndFilters4 = case Name of
        all ->
            AndFilters3;
        _ ->
            [#{field=><<"metadata.involvedObject.name">>, value=>Name} | AndFilters3]
    end,
    Filters2 = Filters1#{'and' => AndFilters4},
    Sort1 = maps:get(sort, Params, []),
    Sort2 = [#{field=><<"metadata.creationTime">>, order=>asc}|Sort1],
    SearchSpec2 = Params#{filter => Filters2, sort=>Sort2, totals=>false},
    % lager:error("NKLOG SEARCH SPEC ~p", [SearchSpec2]),
    nkservice_actor_queries_pgsql:get_query(SrvId, {service_search_actors, SearchSpec2}, []);

get_query(_SrvId, _QueryType, _Opts) ->
    continue.

%% @private
quote(Term) ->
    nkservice_pgsql_util:quote(Term).


%% @private
pgsql_version([{{select, _Size}, Rows, _OpMeta}], Meta) ->
    case Rows of
        [{UID, Date}|_] ->
            {ok, {Date, UID}, Meta};
        [] ->
            {ok, not_found, Meta}
    end.

