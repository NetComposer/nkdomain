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
-export([load_schema/0, request/2]).
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
        params => #{field_key() => field_value()},
        default => string(),
        comment => string()
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
            default => nkdomain_graphql_obj
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


%% @doc Launches a request
request(Str, Meta) when is_list(Str) ->
    request(list_to_binary(Str), Meta);

request(Str, Meta) when is_binary(Str) ->
    case gather(#{<<"query">>=>Str, <<"vaiables">>=>null}, #{}) of
        {ok, Decoded} ->
            Start = nklib_util:l_timestamp(),
            run_request(Decoded#{nkmeta=>#{start=>Start}, nkuser=>Meta});
        {error, Error} ->
            {error, Error}
    end.


%% @private
run_request(#{document:=Doc}=Ctx) ->
    case graphql:parse(Doc) of
        {ok, AST} ->
            run_preprocess(Ctx#{document:=AST});
        {error, Reason} ->
            lager:error("NKLOG RR1 ~p", [Reason]),
            {error, Reason}
    end.


%% @private
run_preprocess(#{document:=AST}=ReqCtx) ->
    try
        Elaborated = graphql:elaborate(AST),
        {ok, #{
                fun_env := FunEnv,
                ast := AST2
            }
        } = graphql:type_check(Elaborated),
        ok = graphql:validate(AST2),
        run_execute(ReqCtx#{document := AST2, fun_env => FunEnv})
    catch
        throw:{error, Map} ->
            make_error(Map);
        throw:Err ->
            lager:error("NKLOG RR2 ~p", [Err]),
            {error, Err}
    end.


%% @private
run_execute(ReqCtx) ->
    #{
        document := AST,
        fun_env := FunEnv,
        vars := Vars,
        operation_name := OpName,
        nkmeta := NkMeta,
        nkuser := NkUser
    } = ReqCtx,
    Coerced = graphql:type_check_params(FunEnv, OpName, Vars), % <1>
    Ctx = #{
        params => Coerced,
        operation_name => OpName,
        nkmeta => NkMeta,
        nkuser => NkUser
    },
    Res = graphql:execute(Ctx, AST),
    % lager:error("NKLOG L ~p", [Res]),
    case Res of
        #{errors:=[Error1|_], data:=_Data} ->
            make_error(Error1);
        #{data:=Data} ->
            {ok, Data};
        {error, {error, Error}} ->
            {error, Error};
        {error, Error} ->
            {error, Error}
    end.



%% @private
gather(Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {ok, Vars} ->
            Operation = operation_name([Params, Body]),
            {ok, #{ document => QueryDocument,
                    vars => Vars,
                    operation_name => Operation}};
        {error, Reason} ->
            {error, Reason}
    end.


%% @private
document([#{ <<"query">> := Q }|_]) -> Q;
document([_|Next]) -> document(Next);
document([]) -> undefined.


%% @private
variables([#{ <<"variables">> := Vars} | _]) ->
    if
        is_binary(Vars) ->
            case nklib_json:decode(Vars) of
                null ->
                    {ok, #{}};
                JSON when is_map(JSON) ->
                    {ok, JSON};
                _ ->
                    {error, invalid_json}
            end;
        is_map(Vars) ->
            {ok, Vars};
        Vars == null ->
            {ok, #{}}
    end;
variables([_ | Next]) ->
    variables(Next);
variables([]) ->
    {ok, #{}}.


%% @private
operation_name([#{ <<"operationName">> := OpName } | _]) ->
    OpName;
operation_name([_ | Next]) ->
    operation_name(Next);
operation_name([]) ->
    undefined.


%% @private
make_error(#{key:=Key, message:=Msg, path:=Path}) ->
    Text = list_to_binary([Msg, " (", nklib_util:bjoin(Path, <<".">>), ")"]),
    {error, {Key, Text}}.

