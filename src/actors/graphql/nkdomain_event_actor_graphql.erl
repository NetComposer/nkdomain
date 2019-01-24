%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain Event Actor GraphQL
-module(nkdomain_event_actor_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([config/0, schema/1, connections/1, query/5, execute/5]).
-import(nkdomain_graphql_execute, [get_value/2, get_map/2, get_time/3, get_object/2]).
-behavior(nkservice_graphql_schema).

-include("nkdomain.hrl").

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================


config() ->
    #{
        type => ?KIND_CORE_EVENT,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_EVENTS
    }.


%%  @doc Generates new schema entries
schema(types) ->
    Time = #{params => #{format => timeFormat}},
    #{
        'EventSource' => #{
            fields => #{
                component => {no_null, string},
                host => {no_null, string}
            }
        },
        'EventInvolvedObject' => #{
            fields => #{
                apiVersion => {no_null, string},
                kind => {no_null, string},
                % type => {no_null, actorType},
                name => {no_null, string},
                domain => {no_null, string},
                uid => {no_null, string},
                resourceVersion => string,
                subtype => string
            }
        },
        'Event' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(
                #{
                    reason => {no_null, string},
                    message => string,
                    source => {no_null, 'EventSource'},
                    involvedObject => {no_null, 'EventInvolvedObject'},
                    firstTimestamp => {no_null, string, Time},
                    lastTimestamp => {no_null, string, Time},
                    count => {no_null, integer},
                    body => {list, 'Map'}
                }),
            comment => "An Event",
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(
                #{
                    reason => 'KeywordFilter',
                    firstTimestamp => 'KeywordFilter',
                    lastTimestamp => 'KeywordFilter',
                    count => 'IntegerFilter',
                    involvedObject => 'EventInvolvedObjectFilterFields'
                }),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(
                #{
                    reason => 'SortSpec',
                    firstTimestamp => 'SortSpec',
                    lastTimestamp => 'SortSpec',
                    count => 'SortSpec',
                    involvedObject => 'EventInvolvedObjectSortFields'
                })
        }
    };

schema(inputs) ->
    #{
        'EventInvolvedObjectSortFields' => #{
            fields => #{
                apiVersion => 'SortSpec',
                kind => 'SortSpec',
                % type => 'SortSpec',
                name => 'SortSpec',
                domain => 'SortSpec'
            }
        },
        'EventInvolvedObjectFilterFields' => #{
            fields => #{
                apiVersion => 'KeywordFilter',
                kind => 'KeywordFilter',
                %type => 'KeywordFilter',
                name => 'KeywordFilter',
                domain => 'KeywordFilter',
                uid => 'KeywordFilter',
                resourceVersion => 'KeywordFilter'
            }
        }
    };

schema(queries) ->
    #{
        allEvents => nkdomain_graphql_schema:actor_query(?KIND_CORE_EVENT, #{})
    };


schema(_) ->
    #{}.


%% @private
connections(<<"User">>) ->
    #{
        eventsConnection => nkdomain_graphql_schema:actor_connection(?KIND_CORE_EVENT, #{
            comment => "Events created by this Actor"
        })
    };

connections(_) ->
    #{}.


query(SrvId, <<"allEvents">>, Params, _Meta, _Ctx) ->
    Opts = #{
        apiGroup => ?GROUP_CORE,
        kind => ?KIND_CORE_EVENT
    },
    nkdomain_graphql_search:search(SrvId, Params, Opts).


%% @private
execute(SrvId, Field, {nkdomain, {actor, ?KIND_CORE_EVENT, Actor}}, _Meta, Params) ->
    case Field of
        _ when Field==<<"firstTimestamp">>; Field==<<"lastTimestamp">> ->
            get_time(Field, Actor, Params);
        <<"involvedObject">> ->
            Obj = maps:get(<<"involvedObject">>, Actor),
            get_object(SrvId, {field, ?KIND_CORE_EVENT, involvedObject, Obj});
        <<"source">> ->
            Obj = maps:get(<<"source">>, Actor),
            get_object(SrvId, {field, ?KIND_CORE_EVENT, source, Obj});
        <<"body">> ->
            get_map(Field, Actor);
        _ ->
            get_value(Field, Actor)
    end;

execute(_SrvId, Field, {nkdomain, {field, ?KIND_CORE_EVENT, involvedObject, Obj}}, _Meta, _Params) ->
    get_value(Field, Obj);

execute(_SrvId, Field, {nkdomain, {field, ?KIND_CORE_EVENT, source, Obj}}, _Meta, _Params) ->
    get_value(Field, Obj);

execute(SrvId, <<"eventsConnection">>, {nkdomain, {actor, _Type, Actor}}, _Meta, Params) ->
    #{<<"metadata">>:=#{<<"uid">>:=UID}} = Actor,
    Opts = #{
        apiGroup => ?GROUP_CORE,
        kind => ?KIND_CORE_EVENT,
        search_spec => #{
            deep => true,
            filter => #{
                'and' => [
                    #{field=><<"involvedObject.uid">>, op=>eq, value=>UID}
                ]
            }
        }
    },
    nkdomain_graphql_search:search(SrvId, Params, Opts);

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.
