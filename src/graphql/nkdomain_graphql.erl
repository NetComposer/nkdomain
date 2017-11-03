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

%% When a query is received, it first go to nkdomain_graphql_object_query
%% If it is an abstract type (interface or union) it will go to find the type,
%% and the to the type manager object (nkdomain_graphql_object manages all of them

-module(nkdomain_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([load_schema/0, make_schema/0]).
-export([object_schema_scalars/0, object_schema_enums/0, object_schema_interfaces/0, object_schema_types/0,
         object_schema_queries/0, object_schema_mutations/0]).



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


load_schema() ->
    ok = graphql_schema:reset(),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, make_schema()),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.


%% tag::setupRoot[]
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


%% @doc
make_schema() ->
    Modules = [?MODULE|nkdomain_lib:get_all_modules()],
    Scalars = schema_scalars(Modules),
    Enums = schema_enums(Modules),
    Types = schema_types(Modules),
    Interfaces = schema_interfaces(Modules),
    Queries = schema_queries(Modules),
    Mutations = schema_mutations(Modules),
    Bin = list_to_binary([Scalars, Enums, Interfaces, Types, Queries, Mutations]),
    io:format("\n~s\n\n", [Bin]),
    Bin.


%% @doc
object_schema_scalars() ->
    #{
        'UnixTime' => #{comment=>"Standard milisecond-resolution unix time"}
    }.


%% @doc
object_schema_enums() ->
    #{
        'Mood' => #{
            opts => ['TRANQUIL', 'DODGY', 'AGGRESSIVE'],
            comment => "Sample enum"
        }
    }.

object_schema_types() ->
    #{
        'PageInfo' => #{
            fields => #{
                hasNextPage => {no_null, boolean},
                hasPreviousPage => {no_null, boolean}
            }
        }
    }.


object_schema_interfaces() ->
    #{
        'Node' => #{
            fields => #{id => {no_null, id}},
            comment => "Relay Modern Node Interface"
        },
        'Object'=> #{
            fields => object2(),
            comment => "Standard NetComposer Object"
        }
    }.


object_schema_queries() ->
    #{
        node => {'Node',
                    #{
                        params => #{id => {no_null, id}},
                        comment => "Relay Modern specification Node fetcher"
                    }}
    }.



object_schema_mutations() ->
    #{
    }.


%% @private
schema_scalars(Modules) ->
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
schema_enums(Modules) ->
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
schema_types(Modules) ->
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
                                        parse_fields(object2()), "\n"
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
schema_interfaces(Modules) ->
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


%% @private
schema_queries(Modules) ->
    List = lists:foldl(
        fun(Module, Acc) ->
            case nkdomain_lib:type_apply(Module, object_schema_queries, []) of
                not_exported ->
                    Acc;
                Map ->
                    Acc ++ parse_fields(Map)
            end
        end,
        [],
        Modules),
    ["type Query {\n", List, "}\n\n"].


%% @private
schema_mutations(Modules) ->
    {List, Inputs, Types} = lists:foldl(
        fun(Module, {Acc, AccInputs, AccTypes}) ->
            case nkdomain_lib:type_apply(Module, object_schema_mutations, []) of
                not_exported ->
                    {Acc, AccInputs, AccTypes};
                Map ->
                    Acc2 = Acc ++ [
                        [
                            comment(Opts),
                            sp(), "introduce", to_bin(Name), "(input: Introduce", to_bin(Name), "Input!) : ",
                            "Introduce", to_bin(Name), "Payload\n"
                        ]
                        || {Name, Opts} <- maps:to_list(Map)
                    ],
                    AccInputs2 = AccInputs ++ [
                        [
                            "input Introduce", to_bin(Name), "Input {\n",
                            parse_fields(Input#{clientMutationId => string}),
                            "}\n\n"
                        ]
                        || {Name, #{input:=Input}} <- maps:to_list(Map)
                    ],
                    AccTypes2 = AccTypes ++ [
                        [
                            "type Introduce", to_bin(Name), "Payload {\n",
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



%%object_schema(Type, Data) ->
%%    T = to_bin(Type),
%%    [
%%        "+description(text: \"Representation of an ", T, "\")\n",
%%        "type ", T, " implements Node, Object {\n",
%%        parse_fields(object2()), "\n",
%%        parse_fields(Data),
%%        "}\n\n"
%%    ].


%% @doc Sample for type 'UserStats' and data 'domainPath:...'
%%  type UserStatusConnection {                 # This is an 'abstract' concept but UserStatusEdge is not
%%      pageInfo : PageInfo!                    # it is the 'line' connecting Users and UserStatus objects
%%      edges : [UserStatusEdge]                # an 'edge' is the 'relation' between among two objects
%%      totalCount : Int                        # in this case, between 'User' and 'UserStatus' objects
%%  }
%%
%%  type UserStatusEdge {                       # The real relation between the User and the UserStatus
%%      node : UserStatus                       # it could have more metadata, but it is not common
%%      cursor : String!
%%  }
%%
%%  type UserStatus {
%%      domainPath : String!
%%      userStatus : String
%%      updatedTime : UnixTime
%%  }


%%object_schema_connection(Type, Data) ->
%%    T = to_bin(Type),
%%    [
%%        "type ", T, "Connection {\n",
%%        parse_fields(#{
%%            pageInfo => {no_null, 'PageInfo'},
%%            edges => {list, <<T/binary, "Edge">>},
%%            totalCount => int
%%        }), "\n\n",
%%        "type ", T, "Edge {\n",
%%        parse_fields(#{
%%            node => T,
%%            cursor => {no_null, string}
%%         }), "\n\n",
%%        "type ", T, "{\n",
%%        parse_fields(Data),
%%        "}\n\n"
%%    ].

%%object_schema_mutation(Type, Data1, Data2) ->
%%    T = to_bin(Type),
%%    [
%%        "    introduce", T, "(input: Introduce", T, "Input!) : Introduce", T, "Payload\n",
%%        "}\n\n",
%%        "input Introduce", T, "Input {\n",
%%        parse_fields(Data1#{clientMutationId => string}),
%%        "}\n\n",
%%        "type Introduce", T, "Payload {\n",
%%        parse_fields(Data2#{clientMutationId => string})
%%    ].


parse_fields(Map) when is_map(Map) ->
    parse_fields(maps:to_list(Map), []);

parse_fields(List) when is_list(List) ->
    parse_fields(List, []).



parse_fields([], Acc) ->
    Acc;

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
    parse_fields(Rest, [Line, "\n" |Acc]).


value(id)      -> <<"ID">>;
value(int)     -> <<"Int">>;
value(string)  -> <<"String">>;
value(object)  -> <<"Object">>;
value(time)    -> <<"UnixTime">>;
value(boolean) -> <<"Boolean">>;
value(Other)   -> to_bin(Other).


field(F) ->
    field(F, #{}).


field(F, Opts) ->
    [sp(), to_bin(F), params(Opts)].




connection() ->
    ["Connection", params(#{params=>#{'after'=>string, first=>int, before=>string, last=>int}})].


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


sp() -> <<"    ">>.


params(#{params:=Map}) ->
    ["(\n", [[sp(), L] || L <- parse_fields(Map)], sp(), ")"];

params(_) ->
    [].




object2() ->
    #{
        id => {no_null, id, #{comment=>"Main ID fetcher"}},
        vsn => string,
        objId => {no_null, string},
        type => {no_null, string},
        path => {no_null, string},
        objName => {no_null, string},
        domainId => {no_null, string},
        domain => {no_null, 'Domain'},
        parentId => {no_null, string},
        parent => {no_null, object},
        srvId => string,
        subtype => {list, string},
        createdById => {no_null, string},
        createdBy => {no_null, 'User'},
        createdTime => {no_null, time},
        updatedBy => {no_null, 'User'},
        updatedTime => {no_null, time},
        enabled => {no_null, boolean},
        active => {no_null, boolean},
        expiresTime => time,
        destroyed => boolean,
        destroyedTime => time,
        destroyedCode => string,
        destroyedReason => string,
        name => string,
        description => string,
        tags => {list, string},
        aliases => {list, string},
        iconId => 'File',
        nextStatusTime => time
    }.








%%schema() -> <<"
%%+descripton(text: \"Standard miliseconds unix time\")
%%scalar UnixTime
%%
%%enum Mood {
%%     TRANQUIL
%%     DODGY
%%     AGGRESSIVE
%%}
%%
%%type Query {
%%  +description(text: \"Relay Modern specification Node fetcher\")
%%  node(id : ID!) : Node
%%  +description(text: \"Get all users\")
%%  allUsers : [User]
%%}
%%
%%
%%
%%+description(text: \"Relay Modern Node Interface\")
%%interface Node {
%%  +description(text: \"Unique Identity of a Node\")
%%  id : ID!
%%}
%%
%%
%%interface Object {",
%%(object())/binary,
%%"}
%%
%%
%%type PageInfo {
%%  hasNextPage : Boolean!
%%  hasPreviousPage : Boolean!
%%}
%%
%%+description(text: \"Representation of Domains\")
%%type Domain implements Node, Object {",
%%(object())/binary, "
%%}
%%
%%+description(text: \"Representation of Users\")
%%type User implements Node, Object {",
%%(object())/binary, "
%%  userName : String,
%%  userSurname : String
%%  email : String
%%  #password : String
%%  phone : String,
%%  address : String
%%  statusConnection(         # connections to 'UserStatus' objects
%%    after : String
%%    first : Int
%%    before : String
%%    last : Int) : UserStatusConnection
%%}
%%
%%
%%type UserStatusConnection {                 # This is an 'abstract' concept but UserStatusEdge is not
%%    pageInfo : PageInfo!                    # it is the 'line' connecting Users and UserStatus objects
%%    edges : [UserStatusEdge]                # an 'edge' is the 'relation' between among two objects
%%    totalCount : Int                        # in this case, between 'User' and 'UserStatus' objects
%%}
%%
%%type UserStatusEdge {                       # The real relation between the User and the UserStatus
%%    node : UserStatus                       # it could have more metadata, but it is not common
%%    cursor : String!
%%}
%%
%%type UserStatus {
%%    domainPath : String!
%%    userStatus : String
%%    updatedTime : UnixTime
%%}
%%
%%
%%type File implements Node, Object {",
%%(object())/binary, "
%%  contentType : String
%%  size : Int
%%}
%%
%%
%%## -- MUTATION OBJECTS ----------
%%
%%type Mutation {
%%    introduceUser(input: IntroduceUserInput!) : IntroduceUserPayload
%%}
%%
%%
%%input IntroduceUserInput {
%%    clientMutationId : String
%%    name : String!
%%}
%%
%%
%%type IntroduceUserPayload {
%%    clientMutationId : String
%%}
%%
%%">>.
%%
%%
%%
%%object() ->
%%    <<"    id : ID!
%%    vsn : String
%%    objId : String!
%%    type : String!
%%    path : String!
%%    objName : String!
%%    domainId : String!
%%    domain : Domain!
%%    parentId : String!
%%    parent : Object!
%%    srvId : String
%%    subtype : [String]
%%    createdById : String!
%%    createdBy : User!
%%    createdTime : UnixTime!
%%    updatedBy : User!
%%    updatedTime : UnixTime!
%%    enabled : Boolean!
%%    active : boolean!
%%    expiresTime : UnixTime
%%    destroyed : Boolean
%%    destroyedTime : UnixTime,
%%    destroyedCode : String
%%    destroyedReason : String
%%    name : String
%%    description : String
%%    tags : [String]
%%    aliases : [String]
%%    iconId : File
%%    nextStatusTime : UnixTime\n">>.





%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).