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

%% @doc NkDomain GraphQL schema
-module(nkdomain_graphql_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([core_schema/2]).
-export([actor_query/2, actor_connection/2]).
-export([actor_type_fields/1, actor_filter_fields/1, actor_sort_fields/1,
         actor_status_fields/1]).
-export([make_object/2, get_graphql_type/2]).
-export([add_event_connection/1, event_execute/5]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
% Refers to ActorSearchResult, ActorQueryFilter, ActorQuerySort
% They are created automatically by type if class = actor
% (nkservice_graphql_schema_lib)
%% @see nkservice_graphql_schema:actor_query/3
actor_query(BaseType, Opts) ->
    Params1 = maps:get(params, Opts, #{}),
    Params2 = Params1#{
        domain => string, % Default not working! #{default => "root"}},
        deep =>  {boolean, #{default=>false, comment=>"Find in subdomains"}},
        from => integer,
        size => integer,
        first => {integer, #{comment=>"First N records, sorted by updateTime"}},
        last => {integer, #{comment=>"Last N records, sorted by updateTime"}}
    },
    Meta1 = maps:get(meta, Opts, #{}),
    Meta2 = Meta1#{type => nkdomain_query},
    Opts2 = Opts#{params=>Params2, meta=>Meta2},
    nkservice_graphql_schema:actor_query(BaseType, Opts2).


%% @doc
% Refers to ActorSearchResult, ActorQueryFilter, ActorQuerySort
% They are created automatically by type if class = actor
% (nkservice_graphql_schema_lib)
%% @see nkservice_graphql_schema:actor_query/3
actor_connection(BaseType, Opts) ->
    Params1 = maps:get(params, Opts, #{}),
    Params2 = Params1#{
        from => integer,
        size => integer,
        first => {integer, #{comment=>"First N records, sorted by updateTime"}},
        last => {integer, #{comment=>"Last N records, sorted by updateTime"}}
    },
    Meta1 = maps:get(meta, Opts, #{}),
    Meta2 = Meta1#{type => nkdomain_connection},
    Opts2 = Opts#{params=>Params2, meta=>Meta2},
    nkservice_graphql_schema:actor_query(BaseType, Opts2).



%% @private Fields you can get in an actor
%% @see nkservice_graphql_execute
actor_type_fields(Fields) ->
    Fields#{
        id => {no_null, id, #{comment=>"Actor's globally unique UID"}},
        % type => {no_null, actorType, #{comment => "Actor's type"}},
        apiVersion => {no_null, string, #{comment => "Actors's apiGroup and version"}},
        kind => {no_null, string, #{comment => "Actors's kind"}},
        metadata => {no_null, 'ActorMetadata', #{comment => "Actor Metadata"}}
    }.


%% @private Fields you can get in an actor
%% @see nkservice_graphql_execute
actor_status_fields(Fields) ->
    Fields#{
        isActivated => {boolean, #{comment=>"For search operations it will be null, meaning that it is not known. Use a direct operation"}}
    }.


%% @private
actor_filter_fields(Fields) ->
    Fields#{
        id => {'IdFilter', #{comment => "Actor's ID"}},
        % type => 'TypeFilter',
        apiGroup => {'KeywordFilter', #{comment => "Actors's apiGroup"}},
        kind => {'KeywordFilter', #{comment => "Actors's kind"}},
        metadata => {'MetadataFilterFields', #{comment => "Filter on metadata's fields"}}
    }.


%% @private
actor_sort_fields(Fields) ->
    Fields#{
        %type => 'SortSpec',
        apiGroup => 'SortSpec',
        kind => 'SortSpec',
        metadata => 'MetadataSortFields'
    }.


%% @doc Generates a GraphQL object to be parsed by execute
%% A full actor
make_object(SrvId, {actor, Actor}) ->
    case get_graphql_type(SrvId, Actor) of
        {ok, {enum, Type}} ->
            {ok, {nkdomain, {actor, Type, Actor}}};
        {error, Error} ->
            {error, Error}
    end;

%% The spec field, includes the type
make_object(_SrvId, {spec, Type, Actor}) ->
    case maps:get(<<"spec">>, Actor, null) of
        null ->
            {ok, null};
        Spec ->
            {ok, {nkdomain, {spec, Type, Spec, Actor}}}
    end;

%% The data field, includes the type
make_object(_SrvId, {data, Actor}) ->
    Data1 = maps:get(<<"data">>, Actor, #{}),
    Data2 = [{ok, {nkdomain, {map, {K, V}}}} || {K, V} <- maps:to_list(Data1)],
    {ok, Data2};

% The metadata field
make_object(_SrvId, {metadata, Actor}) ->
    %% Standard metadata
    Metadata = maps:get(<<"metadata">>, Actor),
    {ok, {nkdomain, {metadata, Metadata}}};

% The status field
make_object(_SrvId, {status, Type, Actor}) ->
    case maps:get(<<"status">>, Actor, null) of
        null ->
            {ok, null};
        Status ->
            {ok, {nkdomain, {status, Type, Status}}}
    end;

% A deep map
make_object(SrvId, {field, Type, SubType, List}) when is_list(List) ->
    {ok, [make_object(SrvId, {field, Type, SubType, Data}) || Data <- List]};

make_object(_SrvId, {field, Type, SubType, Data}) ->
    {ok, {nkdomain, {field, Type, SubType, Data}}};

make_object(_SrvId, {map, null}) ->
    {ok, null};

make_object(_SrvId, {map, Map}) ->
    %% Standard map
    Data = [{ok, {nkdomain, {map, {K, V}}}} || {K, V} <- maps:to_list(Map)],
    {ok, Data}.


%% @private
-spec get_graphql_type(nkservice:id(), term()) ->
    {ok, {enum, binary()}} | {error, term()}.

get_graphql_type(SrvId, #{<<"apiVersion">>:=ApiVsn, <<"kind">>:=Kind}) ->
    {Group, _Vsn} = nkdomain_api_lib:get_group_vsn(ApiVsn),
    case nkdomain_actor_util:find_resource(SrvId, Group, Kind) of
        {camel, Resource} ->
            case catch nkservice_graphql_plugin:get_type(SrvId, Group, Resource) of
                Type when is_binary(Type) ->
                    {ok, {enum, Type}};
                _ ->
                    {error, {kind_unknown, Kind}}
            end;
        _ ->
            {error, {kind_unknown, Kind}}
    end.


%% @doc
add_event_connection(Base) ->
    Base#{
        eventsConnection =>
            nkdomain_graphql_schema:actor_connection(<<"Event">>, #{
              comment => "Events created by this Actor"
        })
    }.


%% @doc
event_execute(SrvId, <<"eventsConnection">>, Type, Meta, Params) ->
    nkdomain_event_actor_graphql:execute(SrvId, <<"eventsConnection">>, Type, Meta, Params);

event_execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.





%% ===================================================================
%% Core schema
%% ===================================================================


-type schema_class() :: nkservice_graphql_schema:schema_class().
-type schema_def() :: nkservice_graphql_schema:schema_def().


%% @doc
-spec core_schema(nkservice:id(), schema_class()) ->
    schema_def().

core_schema(_SrvId, scalars) ->
    #{
        %'Cursor' => #{comment=>"Pagination cursor"},
        %'UnixTime' => #{comment=>"Standard milisecond-resolution unix time"}
    };

core_schema(_SrvId, enums) ->
    #{
        filterOp => #{
            opts => ['AND', 'OR', 'NOT'],
            comment => "Operation mode for a filter"
        },
        timeFormat => #{
            opts => ['UNIXTIME', 'RFC3339']
        },
        sortOrder => #{
            opts => ['ASC', 'DESC']
        },
        gender => #{
            opts => ['MALE', 'FEMALE']
        }
   };

core_schema(_SrvId, types) ->
    #{
        'Map' => #{
            fields => #{
                key => {no_null, string},
                value => {no_null, string}
            }
        },
        'ActorAlarm' => #{
            fields => #{
                class => {no_null, string},
                code => string,
                message => string,
                lastTime => {string, #{params => #{format => timeFormat}}},
                meta => {list, 'Map'}
            }
        },
        'ActorStatus' => #{
            fields => actor_status_fields(#{})
        },
        'ActorMetadata' => #{
            fields => core_actor_metadata_fields(),
            comment => "Common metadata for all actors"
        },
        % 'Actor' is not real type son SearchResult is not generated after the type
        'ActorSearchResult' => #{
            fields => #{
                actors => {list, 'Actor', #{comment => "My Actors"}},
                totalCount => integer
            }
        }
    };

core_schema(_SrvId, inputs) ->
    #{
        'IdFilter' => #{
            fields => #{
                eq => string,
                values => {list, string},
                exists => bool
            }
        },
        'KeywordFilter' => #{
            fields => #{
                eq => string,
                values => {list, string},
                gt => string,
                gte => string,
                lt => string,
                lte => string,
                prefix => string,
                exists => bool
            }
        },
%%        'NormalizedStringFilter' => #{
%%            fields => #{
%%                eq => string,
%%                prefix => string,
%%                wordsAndPrefix => string,
%%                fuzzy => string
%%            }
%%        },
%%        'PathFilter' => #{
%%            fields => #{
%%                eq => string,
%%                values => {list, string},
%%                gt => string,
%%                gte => string,
%%                lt => string,
%%                lte => string,
%%                childsOf => string,
%%                exists => bool
%%            }
%%        },
        'IntegerFilter' => #{
            fields => #{
                values => {list, integer},
                eq => integer,
                ne => integer,
                gt => integer,
                gte => integer,
                lt => integer,
                lte => integer,
                exists => bool
            }
        },
        'BooleanFilter'=> #{
            fields => #{
                eq => boolean,
                exists => bool
            }
        },
        'MapFilter' => #{
            fields => #{
                key => {no_null, string},
                eq => string,
                values => {list, string},
                gt => string,
                gte => string,
                lt => string,
                lte => string,
                prefix => string,
                exists => bool
            }
        },
        'FTSFilter' => #{
            fields => #{
                field => {string, #{comment=>"Can end in '*'. Default is '*'"}},
                word => {no_null, string, #{comment=>"Can end in '*'"}}
            }
        },
        'SortSpec' => #{
            fields => #{
                order => {sortOrder,
                    #{default => <<"ASC">>, comment=><<"ASC or DESC, default is ASC">>}}
            },
            comment => "Sort options"
        },
        'MetadataFilterFields'=> #{
            fields => actor_metadata_filter_fields(#{}),
            comment => "Metadata filters"
        },
        'MetadataSortFields' => #{
            fields => actor_metadata_sort_fields(),
            comment => "Sort for metadata fields"
        },
        % 'Actor' is not real type son FilterSpec, FilterFields and SortFields
        % are not generated after the type
        'ActorFilterFields' => #{
            fields => actor_filter_fields(#{}),
            comment => "Filter values to sort on"
        },
        'ActorFilterSpec' => #{
            fields => #{
                'and' => {list, 'ActorFilterFields'},
                'or' => {list, 'ActorFilterFields'},
                'not' => {list, 'ActorFilterFields'}
            }
        },
        'ActorSortFields' => #{
            fields => actor_sort_fields(#{}),
            comment => "Fields to sort on"
        }
    };

core_schema(_SrvId, interfaces) ->
    #{
        'Node' => #{
            fields => #{id => {no_null, id}},
            comment => "Relay Modern Node Interface"
        },
        'Actor'=> #{
            fields => actor_type_fields(#{}),
            comment => "Standard NetComposer Actor"
        }
    };

core_schema(_SrvId, queries) ->
    #{
        node => {'Node', #{
                     params => #{id => {no_null, id}},
                     comment => "Relay Modern specification Node fetcher"
                 }}
    };

core_schema(_SrvId, _) ->
    #{
    }.


%% @private Fields you can get in an actor's metadata field
%% @see nkservice_graphql_execute
core_actor_metadata_fields() ->
    Time = #{params => #{format => timeFormat}},
    #{
        uid => {no_null, id, #{comment=>"Actor's globally unique UID"}},
        name => {no_null, string, #{comment => "Actor's domain unique name"}},
        domain => {no_null, string, #{comment => "Domain this actor belongs to"}},
        resourceVersion => {no_null, string, #{comment => "Hash of the content of Actor"}},
        generation => {no_null, integer, #{comment => "Incremented at each modification"}},
        creationTime => {no_null, string, Time#{comment => "Actor creation time"}},
        updateTime => {no_null, string, Time#{comment => "Actor update time"}},
        subtype => {string, #{comment => "Actor sub-type"}},
        description => {string, #{comment => "User-related description"}},
        selfLink => {string, #{comment => "Rest link"}},
        isEnabled => {no_null, boolean, #{comment => "False if actor is disabled"}},
        isInAlarm => {no_null, boolean, #{comment => "False if actor is in alarm state"}},
        alarms => {list, 'ActorAlarm'},
        expiresTime => {string, Time#{comment => "Time this actor will expire"}},
        labels => {list, 'Map', #{comment => "Labels for this Actor"}},
        links => {list, 'Map', #{comment => "Links for this Actor"}},
        annotations => {list, 'Map', #{comment => "Annotations for this Actor"}},
        fts => {list, 'Map', #{comment => "Full text search for this Actor"}}
    }.



%% @private
actor_metadata_filter_fields(Fields) ->
    Fields#{
        uid => {'IdFilter', #{comment => "Actor's ID"}},
        name => {'KeywordFilter', #{comment => "Actor's name"}},
        domain => {'KeywordFilter', #{comment => "Actors's domain"}},
        resourceVersion => 'KeywordFilter',
        generation => 'IntegerFilter',
        creationTime => {'KeywordFilter', #{comment => "Actor creation time"}},
        updateTime => {'KeywordFilter', #{comment => "Actor update time"}},
        isEnabled => {'BooleanFilter', #{comment => "Filter enabled or disabled actors"}},
        expiresTime => {'KeywordFilter', #{comment => "Time this actor will expire"}},
        subtype => {'KeywordFilter', #{comment => "Actor subtype"}},
        isInAlarm => {'BooleanFilter', #{comment => "Filter actors in alarm status"}},
        labels => 'MapFilter',
        links => 'MapFilter',
        fts => {'FTSFilter', #{comment => "FTS Word filter"}}
    }.


%% @private
actor_metadata_sort_fields() ->
    #{
        name => 'SortSpec',
        domain => 'SortSpec',
        creationTime => 'SortSpec',
        updateTime => 'SortSpec',
        expiresTime => 'SortSpec',
        isEnabled => 'SortSpec',
        subtype => 'SortSpec'
    }.



%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).


