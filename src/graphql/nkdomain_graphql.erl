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

%% @doc NkDomain GraphQL main module

%% When a query is received, it first go to nkdomain_graphql_query
%% If it is an abstract type (interface or union) it will go to find the type,
%% and the to the type manager object (nkdomain_graphql_object manages all of them

-module(nkdomain_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([load_schema/0]).
-export([object_schema_scalars/0, object_schema_enums/0, object_schema_interfaces/0,
         object_schema_types/0, object_schema_inputs/0, object_schema_queries/0, object_schema_mutations/0]).
-export([object_query/3]).
-export([object_fields/0]).


%% ===================================================================
%% Types
%% ===================================================================

-type schema_type() :: id | int | boolean | string | object | time | atom().


-type field_key() :: atom().


-type field_value() ::
    schema_type() | {schema_type(), field_opts()} |
    {no_null, schema_type()} | {no_null, schema_type(), field_opts()} |
    {list, schema_type()} | {list, schema_type(), field_opts()} |
    {list_not_null, schema_type()} | {list_not_null, schema_type(), field_opts()} |
    {connection, schema_type()} | {connection, schema_type(), field_opts()}.


-type field_opts() ::
    #{
        comment => string(),
        params => #{field_key() => field_value()}
    }.


-type schema_fields() ::
    #{field_key() => field_value()}.


-type query_name() :: atom().

-type mutation_name() :: atom().



%% ===================================================================
%% API
%% ===================================================================


%% @doc Generates an loads a new schema
load_schema() ->
    nkdomain_graphql_util:load_schema().



%% ===================================================================
%% Common Schema
%% ===================================================================


%% @doc Generates new scalars
-spec object_schema_scalars() ->
    #{schema_type() => #{comment => string()}}.

object_schema_scalars() ->
    #{
        'Cursor' => #{comment=>"Pagination cursor"},
        'UnixTime' => #{comment=>"Standard milisecond-resolution unix time"}
    }.


%% @doc Generates new enums
-spec object_schema_enums() ->
    #{schema_type() => #{opts => [atom()], comment => string()}}.

object_schema_enums() ->
    #{
        objectSortByFields => #{
            opts => [domain]
        },
        sortOrder => #{
            opts => [asc, desc]
        },
        'RangeOperators' => #{
            opts => [gt, gte, eq, lte, lt],
            comment => "Range operators for queries"
        }
    }.


%% @doc Generates new types
-spec object_schema_types() ->
    #{schema_type() =>
        #{
            fields => schema_fields(),
            comment => string(),
            is_object => boolean(),         % Generates an Object instance
            is_connection => boolean()      % Generates specific connection types
        }}.

object_schema_types() ->
    #{
        'SearchResult' => #{
            fields => #{
                objects => {list_no_null, 'Object'},
                pageInfo => {no_null, 'PageInfo'},
                totalCount => int,
                cursor => 'Cursor'
            }
        },
        'PageInfo' => #{
            fields => #{
                hasNextPage => {no_null, boolean},
                hasPreviousPage => {no_null, boolean}
            }
        }
    }.


%% @doc Generates new inputs
-spec object_schema_inputs() ->
    #{schema_type() =>
        #{
            fields => schema_fields(),
            comment => string()
        }}.

object_schema_inputs() ->
    #{
        'Range' => #{
            fields => #{
                value => int,
                min => int,
                max => int,
                operator => 'RangeOperators'
            },
            comment => "A value to filter against, or a min and a max value"
        },
        objectFilter => #{
            fields => object_fields_filter(),
            comment => "Filter values to sort on"
        },
        objectSortBy => #{
            fields => #{
                fields => objectSortByFields,
                sortOrder => sortOrder
            },
            comment => "Fields to sort on"
        }
    }.


%% @doc Generates new interfaces
-spec object_schema_interfaces() ->
    #{schema_type() =>
        #{
            fields => schema_fields(),
            comment => string()
        }}.

object_schema_interfaces() ->
    #{
        'Node' => #{
            fields => #{id => {no_null, id}},
            comment => "Relay Modern Node Interface"
        },
        'Object'=> #{
            fields => object_fields(),
            comment => "Standard NetComposer Object"
        }
    }.


%% @doc Generates new queries
-spec object_schema_queries() ->
    #{query_name() => field_value()}.

object_schema_queries() ->
    #{
        node => {'Node',
                    #{
                        params => #{id => {no_null, id}},
                        comment => "Relay Modern specification Node fetcher"
                    }},
        allObjects => {'SearchResult',
                    #{
                        params => #{
                            filter => objectFilter,
                            sort => objectSortBy,
                            first => int,
                            last => int,
                            'after' => 'Cursor',
                            before => 'Cursor'
                        }

                    }}
    }.


%% @doc Generates new mutations
-spec object_schema_mutations() ->
    #{mutation_name() =>
        #{
            inputs => schema_fields(),
            outputs => schema_fields(),
            comment => string()
        }}.

object_schema_mutations() ->
    #{
    }.



%% ===================================================================
%% Queries implementations
%% ===================================================================


object_query(<<"node">>, #{<<"id">>:=Id}, _Ctx) ->
    case nkdomain:get_obj(Id) of
        {ok, Obj} ->
            {ok, Obj};
        {error, Error} ->
            {error, Error}
    end;

object_query(<<"allObjects">>, _Params, _Ctx) ->
    case nkdomain:search(#{filters=>#{type=>user}, fields=>[]}) of
        {ok, Total, Data, _Meta} ->
            Data2 = lists:foldl(
                fun(#{<<"obj_id">>:=ObjId}, Acc) ->
                    {ok, O} = nkdomain:get_obj(ObjId),
                    [O|Acc]
                end,
                [],
                Data),
            lager:error("NKLOG DATA ~p", [Data2]),
            Result = #{
                <<"objects">> => Data2,
                <<"totalCount">> => Total,
                <<"pageInfo">> => #{
                    <<"hasNextPage">> => false,
                    <<"hasPreviousPage">> => false
                },
                <<"cursor">> => <<>>
            },
            {ok, Result};
        {error, Error} ->
            {error, Error}
    end.





%% ===================================================================
%% Internal
%% ===================================================================


%% @private
object_fields() ->
    #{
        active => {no_null, boolean, #{comment => "True if object is performing a task"}},
        aliases => {list, string, #{comment => "List of object aliases"}},
        createdBy => {no_null, 'User', #{comment => "User that created the object"}},
        createdById => {no_null, string, #{comment => "UserId that created the object"}},
        createdTime => {no_null, time, #{comment => "Object creation time"}},
        description => {string, #{comment => "User-related description"}},
        destroyed => {boolean, #{comment => "True if the object is destroyed"}},
        destroyedCode => {string, #{comment => "Destruction reason code"}},
        destroyedReason => {string, #{comment => "Destruction reason text"}},
        destroyedTime => {time, #{comment => "Destruction time"}},
        domain => {no_null, 'Domain', #{comment => "Domain this object belongs to"}},
        domainId => {no_null, string, #{comment => "DomainId this object belongs to"}},
        enabled => {no_null, boolean, #{comment => "False if object is disabled"}},
        expiresTime => {time, #{comment => "Time this object will expire"}},
        icon => {'File', #{comment => "Object File icon"}},
        iconId => {string, #{comment => "Object FileId icon"}},
        id => {no_null, id, #{comment=>"Main ID fetcher"}},
        name => {string, #{comment => "Object's user-related name"}},
        objId => {no_null, string, #{comment => "Object's ID"}},
        objName => {no_null, string, #{comment => "Object's short name"}},
        parent => {no_null, object, #{comment => "Object's parent Object"}},
        parentId => {no_null, string, #{comment => "Object's parent ID Object"}},
        path => {no_null, string, #{comment => "Object's directory path"}},
        srvId => {string, #{comment => "Object's service"}},
        subtypes => {list, string, #{comment => "Object's subtypes"}},
        tags => {list, string, #{comment => "Object's tags"}},
        type => {no_null, string, #{comment => "Object's type"}},
        updatedBy => {no_null, 'User', #{comment => "User that updated the object"}},
        updatedById => {no_null, string, #{comment => "UserId that updated the object"}},
        updatedTime => {no_null, time, #{comment => "Object updation time"}},
        vsn => {string, #{comment => "Object's current version"}}
    }.


%% @private
object_fields_filter() ->
    #{
          active => {boolean, #{comment => "Look for objects performing a task"}},
          aliases => {list, string, #{comment => "Object has an alias"}},
          createdBy => {string, #{comment => "Objects created by this user"}},
          createdTime => {'Range', #{comment => "Object creation time"}},
          description => {string, #{comment => "Words in description"}},
          destroyed => {boolean, #{comment => "Filter by destroyed objects"}},
          destroyedTime => {'Range', #{comment => "Destruction time"}},
          domain => {string, #{comment => "Filter objects belonging to this domain"}},
          enabled => {boolean, #{comment => "Filter enabled or disabled objects"}},
          expiresTime => {'Range', #{comment => "Time this object will expire"}},
          has_icon => {boolean, #{comment => "Objects having an icon"}},
          iconId => {string, #{comment => "Objects hanving this iconId"}},
          name => {string, #{comment => "Words in name"}},
          objId => {string, #{comment => "Object's ID"}},
          objName => {string, #{comment => "Object's with this short name"}},
          parent => {string, #{comment => "Object's parent"}},
          path => {string, #{comment => "Filter on this path"}},
          srvId => {string, #{comment => "Object's service"}},
          subtypes => {list, string, #{comment => "Object's subtypes"}},
          tags => {list, string, #{comment => "Object's tags"}},
          type => {string, #{comment => "Object's type"}},
          updatedBy => {string, #{comment => "User that updated the object"}},
          updatedTime => {'Range', #{comment => "Object updation time"}},
          vsn => {string, #{comment => "Object's current version"}}
    }.






