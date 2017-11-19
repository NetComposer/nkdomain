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
    LogPath = nkservice_app:get(log_path),
    file:write_file(filename:join(LogPath, "schema.txt"), Bin),
    Bin.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
make_schema_scalars(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            Map = get_module_schema(scalars, Module),
            Acc ++[
                [
                    comment(Opts#{no_margin=>true}),
                    "scalar ", to_bin(T), "\n\n"
                ]
                || {T, Opts} <- maps:to_list(Map)
            ]
        end,
        [],
        Modules).


%% @private
make_schema_enums(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            Map = get_module_schema(enums, Module),
            Acc ++ [
                [
                    comment(Opts#{no_margin=>true}),
                    "enum ", to_bin(Name), " {\n",
                    [["    ", to_bin(E), "\n"] || E <- EnumOpts], "}\n\n"
                ]
                || {Name, #{opts:=EnumOpts}=Opts} <- maps:to_list(Map)
            ]
        end,
        [],
        Modules).


%% @private
make_schema_types(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            Map = get_module_schema(types, Module),
            Acc ++ [
                case maps:get(type_class, Opts, none) of
                    none ->
                        [
                            comment(Opts#{no_margin=>true}),
                            "type ",to_bin(Name), " {\n",
                            parse_fields(maps:get(fields, Opts, #{})), "}\n\n"
                        ];
                    nkobject ->
                        [
                            comment(Opts#{no_margin=>true}),
                            "type ", to_bin(Name), " implements Node, Object {\n",
                            parse_fields(nkdomain_graphql_obj:object_fields(#{})), "\n",
                            parse_fields(maps:get(fields, Opts, #{})), "}\n\n",
                            "type ", to_bin(Name), "SearchResult {\n",
                            parse_fields(#{objects=>{list_no_null, Name}, totalCount=>{no_null, int}}),
                            "}\n\n"
                        ];
                    connection ->
                        Name2 = get_base_type(Name, <<"Connection">>),
                        [
                            "type ", Name2, "Connection {\n",
                            parse_fields(#{
                                 objects => {list, Name2},
                                 totalCount => int
                            }), "}\n\n"
                        ]
                end
                || {Name, Opts} <- maps:to_list(Map)
            ]
        end,
        [],
        Modules).


%% @private
make_schema_inputs(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            Map = get_module_schema(inputs, Module),
            Acc ++ [
                [
                    comment(Opts#{no_margin=>true}),
                    "input ",to_bin(Name), " {\n",
                    parse_fields(Fields),
                    "}\n\n"
                ]
                || {Name, #{fields:=Fields}=Opts} <- maps:to_list(Map)
            ]
        end,
        [],
        Modules).


%% @private
make_schema_interfaces(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            Map = get_module_schema(interfaces, Module),
            Acc ++ [
                [
                    comment(Opts#{no_margin=>true}),
                    "interface ",to_bin(Name), " {\n", parse_fields(Fields), "}\n\n"
                ]
                || {Name, #{fields:=Fields}=Opts} <- maps:to_list(Map)
            ]
        end,
        [],
        Modules).


%% @private Process queries and register modules
make_schema_queries(Modules) ->
    List = lists:foldl(
        fun(Module, Acc) ->
            Map = get_module_schema(queries, Module),
            lists:foreach(
                fun(Query) ->
                    nklib_types:register_type(nkdomain_query, Query, Module)
                end,
                maps:keys(Map)),
            Acc ++ parse_fields(Map)
        end,
        [],
        Modules),
    case List of
        [] ->
            [];
        _ ->
            ["type Query {\n", List, "}\n\n"]
    end.


%% @private
make_schema_mutations(Modules) ->
    {List, Inputs, Types} = lists:foldl(
        fun(Module, {Acc, AccInputs, AccTypes}) ->
            Map = get_module_schema(mutations, Module),
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
        end,
        {[], [], []},
        Modules),
    case List of
        [] ->
            [];
        _ ->
            [
                "type Mutation {\n", List, "}\n\n",
                Inputs,
                Types
            ]
    end.


%% @private
get_module_schema(Type, Module) ->
    code:ensure_loaded(Module),
    case nkdomain_lib:type_apply(Module, object_schema, [Type]) of
        not_exported ->
            #{};
        Map when is_map(Map) ->
            Map
    end.


%% @private
get_base_type(Name, Bin) ->
    [Name2, _] = binary:split(to_bin(Name), Bin),
    Name2.



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
            [comment(Opts), field(Field, Opts), " : ", value(V), "!", default(Opts)];
        {list_no_null, V} ->
            [field(Field), " : [", value(V), "!]"];
        {list_no_null, V, Opts} ->
            [comment(Opts), field(Field, Opts), ": [", value(V), "!]", default(Opts)];
        {list, V} ->
            [field(Field), " : [", value(V), "]"];
        {list, V, Opts} ->
            [comment(Opts), field(Field, Opts), " : [", value(V), "]", default(Opts)];
        {connection, V} ->
            [field(Field), connection(#{}), " : ", to_bin(V), "Connection"];
        {connection, V, Opts} ->
            [comment(Opts), field(Field, Opts), connection(Opts), " : ", to_bin(V), "Connection"];
        {V, Opts} ->
            [comment(Opts), field(Field, Opts), " : ", value(V), default(Opts)];
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
connection(Opts) ->
    Params = maps:with([from, size, filter, sort, last], Opts),
    ["Connection", params(#{params=>Params})].


%%%% @private
%%connection_last() ->
%%    ["Connection", params(#{params=>#{last=>{int, #{default=>5}}}})].


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
default(#{default:=D}) ->
    <<" = ", (to_bin(D))/binary>>;

default(_Opts) ->
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



