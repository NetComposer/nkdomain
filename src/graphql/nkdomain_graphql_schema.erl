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

-module(nkdomain_graphql_schema).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([make_schema/0]).



%% ===================================================================
%% API
%% ===================================================================


%% @doc Generates and schema
make_schema() ->
    Modules = [nkdomain_graphql_obj|nkdomain_reg:get_all_type_modules()],
    Scalars = make_schema_scalars(Modules),
    Enums = make_schema_enums(Modules),
    Interfaces = make_schema_interfaces(Modules),
    Types = make_schema_types(Modules),
    Inputs = make_schema_inputs(Modules),
    Queries = make_schema_queries(Modules),
    Mutations = make_schema_mutations(Modules),
    Bin = list_to_binary([Scalars, Enums, Interfaces, Types, Inputs, Queries, Mutations]),
    io:format("\n~s\n\n", [Bin]),
    Bin.


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
                                        parse_fields(nkdomain_graphql_obj:object_fields()), "\n"
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
make_schema_inputs(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_inputs, []) of
                not_exported ->
                    Acc;
                Map ->
                    Acc ++ [
                        [
                            comment(Opts#{no_margin=>true}),
                            "input ",to_bin(Name), " {\n",
                            parse_fields(Fields),
                            "}\n\n"
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



