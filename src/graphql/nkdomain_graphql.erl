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
-export([load_schema/0, make_schema/0]).
-export([object_schema_scalars/0, object_schema_enums/0, object_schema_interfaces/0,
         object_schema_types/0, object_schema_queries/0, object_schema_mutations/0]).
-export([object_query/3]).



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
    ok = graphql:load_schema(Mapping, make_schema()),
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


%% @doc Generates and schema
make_schema() ->
    Modules = [?MODULE|nkdomain_lib:get_all_modules()],
    Scalars = make_schema_scalars(Modules),
    Enums = make_schema_enums(Modules),
    Types = make_schema_types(Modules),
    Interfaces = make_schema_interfaces(Modules),
    Queries = make_schema_queries(Modules),
    Mutations = make_schema_mutations(Modules),
    Bin = list_to_binary([Scalars, Enums, Interfaces, Types, Queries, Mutations]),
    io:format("\n~s\n\n", [Bin]),
    Bin.

%% ===================================================================
%% Common Schema
%% ===================================================================


%% @doc Generates new scalars
-spec object_schema_scalars() ->
    #{schema_type() => #{comment => string()}}.

object_schema_scalars() ->
    #{
        'UnixTime' => #{comment=>"Standard milisecond-resolution unix time"}
    }.


%% @doc Generates new enums
-spec object_schema_enums() ->
    #{schema_type() => #{opts => [atom()], comment => string()}}.

object_schema_enums() ->
    #{
        'Mood' => #{
            opts => ['TRANQUIL', 'DODGY', 'AGGRESSIVE'],
            comment => "Sample enum"
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
        'PageInfo' => #{
            fields => #{
                hasNextPage => {no_null, boolean},
                hasPreviousPage => {no_null, boolean}
            }
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
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
make_schema_scalars(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_scalars, []) of
                not_exported ->
                    Acc;
                Map ->
                    Acc ++[
                        [
                            comment(Opts#{no_margin=>true}),
                            "scalar ", to_bin(T), "\n\n"
                        ]
                        || {T, Opts} <- maps:to_list(Map)
                    ]
            end
        end,
        [],
        Modules).


%% @private
make_schema_enums(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_enums, []) of
                not_exported ->
                    Acc;
                Map ->
                    Acc ++ [
                        [
                            comment(Opts#{no_margin=>true}),
                            "enum ", to_bin(Name), " {\n",
                            [["    ", to_bin(E), "\n"] || E <- EnumOpts], "}\n\n"
                        ]
                        || {Name, #{opts:=EnumOpts}=Opts} <- maps:to_list(Map)
                    ]
            end
        end,
        [],
        Modules).


%% @private
make_schema_types(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_types, []) of
                not_exported ->
                    Acc;
                Map ->
                    Acc ++ [
                        [
                            comment(Opts#{no_margin=>true}),
                            "type ",to_bin(Name),
                            case Opts of
                                #{is_object:=true}  ->
                                    [
                                        " implements Node, Object {\n",
                                        parse_fields(object_fields()), "\n"
                                    ];
                                _ ->
                                    " {\n"
                            end,
                            parse_fields(Fields),
                            "}\n\n",
                            case Opts of
                                #{is_connection:=true} ->
                                    [
                                        "type ", to_bin(Name), "Connection {\n",
                                        parse_fields(#{
                                            pageInfo => {no_null, 'PageInfo'},
                                            edges => {list, <<(to_bin(Name))/binary, "Edge">>},
                                            totalCount => int
                                        }), "}\n\n",
                                        "type ", to_bin(Name), "Edge {\n",
                                        parse_fields(#{
                                            node => to_bin(Name),
                                            cursor => {no_null, string}
                                        }), "}\n\n"
                                    ];
                                _ ->
                                    []
                            end
                        ]
                        || {Name, #{fields:=Fields}=Opts} <- maps:to_list(Map)
                    ]
            end
        end,
        [],
        Modules).


%% @private
make_schema_interfaces(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_interfaces, []) of
                not_exported ->
                    Acc;
                Map ->
                    Acc ++ [
                        [
                            comment(Opts#{no_margin=>true}),
                            "interface ",to_bin(Name), " {\n", parse_fields(Fields), "}\n\n"
                        ]
                        || {Name, #{fields:=Fields}=Opts} <- maps:to_list(Map)
                    ]
            end
        end,
        [],
        Modules).


%% @private Process queries and register modules
make_schema_queries(Modules) ->
    List = lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_queries, []) of
                not_exported ->
                    Acc;
                Map ->
                    lists:foreach(
                        fun(Query) ->
                            nklib_types:register_type(nkdomain_query, Query, Module)
                        end,
                        maps:keys(Map)),
                    Acc ++ parse_fields(Map)
            end
        end,
        [],
        Modules),
    ["type Query {\n", List, "}\n\n"].


%% @private
make_schema_mutations(Modules) ->
    {List, Inputs, Types} = lists:foldl(
        fun(Module, {Acc, AccInputs, AccTypes}) ->
            case nkdomain_lib:type_apply(Module, object_schema_mutations, []) of
                not_exported ->
                    {Acc, AccInputs, AccTypes};
                Map ->
                    lists:foreach(
                        fun(Mutation) ->
                            nklib_types:register_type(nkdomain_mutation, Mutation, Module)
                        end,
                        maps:keys(Map)),
                    Acc2 = Acc ++ [
                        [
                            comment(Opts),
                            sp(), to_bin(Name), "(input: ", to_upper(Name), "Input!) : ",
                            to_upper(Name), "Payload\n"
                        ]
                        || {Name, Opts} <- maps:to_list(Map)
                    ],
                    AccInputs2 = AccInputs ++ [
                        [
                            "input ", to_upper(Name), "Input {\n",
                            parse_fields(Input#{clientMutationId => string}),
                            "}\n\n"
                        ]
                        || {Name, #{input:=Input}} <- maps:to_list(Map)
                    ],
                    AccTypes2 = AccTypes ++ [
                        [
                            "type ", to_upper(Name), "Payload {\n",
                            parse_fields(Output#{clientMutationId => string}),
                            "}\n\n"
                        ]
                        || {Name, #{output:=Output}} <- maps:to_list(Map)
                    ],
                    {Acc2, AccInputs2, AccTypes2}
            end
        end,
        {[], [], []},
        Modules),
    [
        "type Mutation {\n", List, "}\n\n",
        Inputs,
        Types
    ].



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
        destroyedTime => {time, #{comment => "Destructin time"}},
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
parse_fields(Map) when is_map(Map) ->
    parse_fields(maps:to_list(Map), []);

parse_fields(List) when is_list(List) ->
    parse_fields(List, []).


%% @private
parse_fields([], Acc) ->
    [Data || {_Field, Data} <- lists:sort(Acc)];

parse_fields([{Field, Value}|Rest], Acc) ->
    Line = case Value of
        {no_null, V} ->
            [field(Field), " : ", value(V), "!"];
        {no_null, V, Opts} ->
            [comment(Opts), field(Field, Opts), " : ", value(V), "!"];
        {list_no_null, V} ->
            [field(Field), " : [", value(V), "!]"];
        {list_no_null, V, Opts} ->
            [comment(Opts), field(Field, Opts), ": [", value(V), "!]"];
        {list, V} ->
            [field(Field), " : [", value(V), "]"];
        {list, V, Opts} ->
            [comment(Opts), field(Field, Opts), " : [", value(V), "]"];
        {connection, V} ->
            [field(Field), connection(), " : ", to_bin(V), "Connection"];
        {connection, V, Opts} ->
            [comment(Opts), field(Field, Opts), connection(), " : ", to_bin(V), "Connection"];
        {V, Opts} ->
            [comment(Opts), field(Field, Opts), " : ", value(V)];
        _ ->
            [field(Field), " : ", value(Value)]
    end,
    parse_fields(Rest, [{Field, [Line, "\n"]} | Acc]).


%% @private
value(id)      -> <<"ID">>;
value(int)     -> <<"Int">>;
value(string)  -> <<"String">>;
value(object)  -> <<"Object">>;
value(time)    -> <<"UnixTime">>;
value(boolean) -> <<"Boolean">>;
value(Other)   -> to_bin(Other).


%% @private
field(F) ->
    field(F, #{}).


%% @private
field(F, Opts) ->
    [sp(), to_bin(F), params(Opts)].

%% @private
connection() ->
    ["Connection", params(#{params=>#{'after'=>string, first=>int, before=>string, last=>int}})].


%% @private
comment(#{comment:=C}=Opts) ->
    case to_bin(C) of
        <<>> ->
            [];
        C2 ->
            [
                case Opts of #{no_margin:=true} -> []; _ -> sp() end,
                "+description(text: \"", C2, "\")\n"]
    end;

comment(_) ->
    [].


%% @private
sp() -> <<"    ">>.


%% @private
params(#{params:=Map}) ->
    ["(\n", [[sp(), L] || L <- parse_fields(Map)], sp(), ")"];

params(_) ->
    [].


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).


%% @private A Type with uppercase in the first letter
to_upper(T) ->
    <<First, Rest/binary>> = to_bin(T),
    <<(nklib_util:to_upper(<<First>>))/binary, Rest/binary>>.



