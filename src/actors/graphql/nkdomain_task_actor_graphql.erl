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

%% @doc NkDomain Task Actor GraphQL
-module(nkdomain_task_actor_graphql).
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
        type => <<"Task">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_TASKS
    }.


%%  @doc Generates new schema entries
schema(types) ->
    #{
        'TaskSpec' => #{
            fields => #{
                job => {list, 'Map'},
                maxTries => integer,
                maxSecs => integer
            }
        },
        'TaskStatus' => #{
            fields => nkdomain_graphql_schema:actor_status_fields(#{
                lastTryStartTime => {no_null, string},
                tries => {no_null, integer},
                status => string,
                progress => integer
            })
        },
        'Task' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => {no_null, 'TaskSpec'},
                status => 'TaskStatus'
            }),
            comment => "A Task",
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{
                spec => 'TaskSpecFilterFields'
            }),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{
                spec=>'TaskSpecSortFields'
            })
        }
    };

schema(inputs) ->
    #{
        'TaskSpecSortFields' => #{
            fields => #{
                maxTries => 'SortSpec',
                maxSecs => 'SortSpec'
            }
        },
        'TaskSpecFilterFields' => #{
            fields => #{
                maxTries => {'KeywordFilter', #{comment => "Task type"}}
            }
        }
    };

schema(queries) ->
    #{
        allTasks => nkdomain_graphql_schema:actor_query(<<"Task">>, #{})
    };

schema(_) ->
    #{}.


connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allTasks">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"Task">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).


%% @private
execute(_SrvId, Field, {nkdomain, {spec, _Type, Spec, _Actor}}, _Meta, _Params) ->
    case Field of
        <<"job">> ->
            get_map(<<"job">>, Spec);
        _ ->
            get_value(Field, Spec)
    end;

execute(_SrvId, Field, {nkdomain, {status, _Type, Status}}, _Meta, _Params) ->
    get_value(Field, Status);

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.

