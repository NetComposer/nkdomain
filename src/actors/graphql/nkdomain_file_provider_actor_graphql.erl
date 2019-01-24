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

%% @doc NkDomain FileProvider Actor GraphQL
-module(nkdomain_file_provider_actor_graphql).
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
        type => <<"FileProvider">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_FILE_PROVIDERS
    }.


%%  @doc Generates new schema entries
schema(types) ->
    #{
        'FileProviderSpecFilesystem' => #{
            fields => #{
                filePath => string
            }
        },
        'FileProviderSpecS3' => #{
            fields => #{
                region => string,
                key => string,
                secret => string,
                bucket => string,
                path => string,
                scheme => string,
                host => string,
                port => integer
            }
        },
        'FileProviderSpec' => #{
            fields => #{
                storageClass => string,
                maxSize => integer,
                encryptionAlgo => string,
                hashAlgo => string,
                directDownload => boolean,
                directUpload => boolean,
                directDownloadSecs => integer,
                directUploadSecs => integer,
                filesystemConfig => 'FileProviderSpecFilesystem',
                s3Config => 'FileProviderSpecS3'
            }
        },
        'FileProvider' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => {no_null, 'FileProviderSpec'},
                status => 'ActorStatus'
            }),
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{spec=>'FileProviderSpecFilterFields'}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{spec=>'FileProviderSpecSortFields'}),
            comment => "A FileProvider"
        }
    };

schema(inputs) ->
    #{
        'FileProviderSpecSortFields' => #{
            fields => #{
                storageClass => 'SortSpec'
            }
        },
        'FileProviderSpecFilterFields' => #{
            fields => #{
                storageClass => 'KeywordFilter'
            }
        }
    };

schema(queries) ->
    #{
        allFileProviders => nkdomain_graphql_schema:actor_query(<<"FileProvider">>, #{})
    };

schema(_) ->
    #{}.


connections(<<"FileProvider">>) ->
    #{
        filesConnection => 'FileSearchResult'
    };

connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allFileProviders">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"FileProvider">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).


%% @private
execute(SrvId, Field, {nkdomain, {spec, _Type, Spec, _Actor}}, _Meta, _Params) ->
    case Field of
        _ when Field==<<"filesystemConfig">>; Field==<<"s3Config">> ->
            case get_value(Field, Spec) of
                {ok, null} ->
                    {ok, null};
                {ok, Value} ->
                    get_object(SrvId, {field, <<"FileProvider">>, Field, Value})
            end;
        _ ->
            get_value(Field, Spec)
    end;

execute(_SrvId, Field, {nkdomain, {field, <<"FileProvider">>, <<"filesystemConfig">>, Map}}, _Meta, _Params) ->
    get_value(Field, Map);

execute(_SrvId, Field, {nkdomain, {field, <<"FileProvider">>, <<"s3Config">>, Map}}, _Meta, _Params) ->
    get_value(Field, Map);

execute(SrvId, <<"filesConnection">>, {nkdomain, {actor, _Type, Actor}}, _Meta, Params) ->
    #{<<"metadata">>:=#{<<"uid">>:=UID}} = Actor,
    LinkType = nkdomain_actor_util:link_type(?GROUP_CORE, ?LINK_CORE_FILE_PROVIDER),
    Opts = #{
        apiGroup => ?GROUP_CORE,
        kind => <<"File">>,
        search_spec => #{
            deep => true,
            filter => #{
                'and' => [
                    #{field=><<"metadata.links.", UID/binary>>, op=>eq, value=>LinkType}
                ]
            }
        }
    },
    nkdomain_graphql_search:search(SrvId, Params, Opts);

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.

