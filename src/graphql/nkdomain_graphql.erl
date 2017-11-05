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

%% Query processing:
%%
%% - nkdomain_graphql_query:execute/4 is called to find who is in charge of the query
%% - for 'node' queries, nkdomain_graphql_obj:object_query/3 find the object details
%% - since the schema says that 'node' queries return an abstract type,
%%   nkdomain_graphql_type:execute/1 is called to find the type
%% - nkdomain_graphql_obj:execute/4 is called to extract each field


-module(nkdomain_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([load_schema/0]).
-export_type([schema_type/0, field_key/0, field_value/0, field_opts/0]).
-export_type([schema_fields/0, query_name/0, mutation_name/0]).


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
    ok = graphql_schema:reset(),
    Mapping = mapping_rules(),
    Schema = nkdomain_graphql_schema:make_schema(),
    ok = graphql:load_schema(Mapping, Schema),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.


%% @private
mapping_rules() ->
    #{
        scalars => #{default => nkdomain_graphql_scalar},
        enums => #{default => graphql_enum_coerce},
        interfaces => #{default => nkdomain_graphql_type},
        unions => #{default => nkdomain_graphql_type},
        objects => #{
            'Query' => nkdomain_graphql_query,
            'Mutation' => nkdomain_graphql_mutation,
            default => nkdomain_graphql_object
        }
    }.


%% @private
setup_root() ->
    Root = {
        root,
        #{
            query => 'Query',
            mutation => 'Mutation',
            interfaces => ['Node']
        }
    },
    ok = graphql:insert_schema_definition(Root),
    ok.


