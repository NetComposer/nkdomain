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

%% @doc NkDomain File Actor GraphQL
-module(nkdomain_file_actor_graphql).
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
        type => <<"File">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_FILES
    }.


%%  @doc Generates new schema entries
schema(types) ->
    #{
        'FileSpec' => #{
            fields => #{
                contentType => string,
                size => integer,
                password => string,
                hash => string,
                provider => string,
                externalId => string,
                downloadLink => string
            }
        },
        'File' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => {no_null, 'FileSpec'},
                status => 'ActorStatus'
            }),
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{spec=>'FileSpecFilterFields'}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{spec=>'FileSpecSortFields'}),
            comment => "A File"
        }
    };

schema(inputs) ->
    #{
        'FileSpecSortFields' => #{
            fields => #{
                contentType => 'SortSpec',
                size => 'SortSpec',
                password => 'SortSpec',
                hash => 'SortSpec'
            }
        },
        'FileSpecFilterFields' => #{
            fields => #{
                contentType => 'KeywordFilter',
                size => 'IntegerFilter',
                password => 'KeywordFilter',
                hash => 'KeywordFilter',
                externalId => 'KeywordFilter'
            }
        }
    };

schema(queries) ->
    #{
        allFiles => nkdomain_graphql_schema:actor_query(<<"File">>, #{})
    };

schema(_) ->
    #{}.


connections(<<"File">>) ->
    #{
        fileProviderConnection => 'FileProvider'
    };

connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allFiles">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"File">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).


%% @private
execute(SrvId, Field, {nkdomain, {spec, _Type, Spec, Actor}}, _Meta, _Params) ->
    case Field of
        <<"downloadLink">> ->
            [Url|_] = nkdomain_plugin:get_external_urls(SrvId),
            Meta = maps:get(<<"metadata">>, Actor),
            Self1 = maps:get(<<"selfLink">>, Meta),
            {ok, <<Url/binary, Self1/binary, "/_download">>};
        _ ->
            get_value(Field, Spec)
    end;

execute(SrvId, <<"fileProviderConnection">>, {nkdomain, {actor, _Type, Actor}}, _Meta, _Params) ->
    #{<<"metadata">>:=Meta} = Actor,
    LinkKey = nkdomain_actor_util:link_key2(?GROUP_CORE, ?LINK_CORE_FILE_PROVIDER),
    case maps:get(<<"links">>, Meta, #{}) of
        #{LinkKey:=FileProviderUID} ->
            nkdomain_graphql_search:get_uid(SrvId, FileProviderUID);
        _ ->
            {ok, null}
    end;

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.

