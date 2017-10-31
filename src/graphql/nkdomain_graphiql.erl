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
-module(nkdomain_graphiql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([http/3]).




%%====================================================================
%% GraphQL
%%====================================================================

%% @private
http(get, [], Req) ->
    http(get, [<<"index.html">>], Req);

http(get, [<<"index.html">>], _Req) ->
    Path = filename:join(code:priv_dir(nkdomain), "graphiql/index.html"),
    {ok, File} = file:read_file(Path),
    {http, 200, content_type(Path), File};

http(get, [<<"assets">>, Name], _Req) ->
    Path = filename:join([code:priv_dir(nkdomain), "graphiql/assets", Name]),
    content_type(Path),
    {ok, File} = file:read_file(Path),
    {http, 200, content_type(Path), File};

http(post, [], Req) ->
    Start = nklib_util:l_timestamp(),
    case gather(nkservice_rest_http:get_cowboy_req(Req)) of
        {error, Reason} ->
            err(400, Reason);
        {ok, Decoded} ->
            %io:format("DECODED ~p\n", [Decoded]),
            run_request(Decoded#{nkmeta=>#{start=>Start}})
    end;

http(Method, Path, _Req) ->
    lager:error("NKLOG GRAPHQL ~p ~p", [Method, Path]),
    {http, 404, [], <<>>}.


content_type(Path) ->
    {A, B, _C} = cow_mimetypes:all(nklib_util:to_binary(Path)),
    [{<<"content-type">>, <<A/binary, $/, B/binary, ";charset=utf-8">>}].





%% @private
run_request(#{ document := undefined }) ->
    err(400, no_query_supplied);

run_request(#{document:=Doc} = ReqCtx) ->
    case graphql:parse(Doc) of
        {ok, AST} ->
            run_preprocess(ReqCtx#{ document := AST});
        {error, Reason} ->
            err(400, Reason)
    end.


%% @private
run_preprocess(#{document:=AST}=ReqCtx) ->
    try
        Elaborated = graphql:elaborate(AST), % <1>
        {ok, #{
            fun_env := FunEnv,
            ast := AST2 }} = graphql:type_check(Elaborated), % <2>
        ok = graphql:validate(AST2), % <3>
        run_execute(ReqCtx#{document := AST2, fun_env => FunEnv})
    catch
        throw:Err ->
            err(400, Err)
    end.


%% @private
run_execute(ReqCtx) ->
    #{
        document := AST,
        fun_env := FunEnv,
        vars := Vars,
        operation_name := OpName,
        nkmeta :=NkMeta
    } = ReqCtx,
    Coerced = graphql:type_check_params(FunEnv, OpName, Vars), % <1>
    Ctx = #{
        params => Coerced,
        operation_name => OpName,
        nkmeta => NkMeta
    },
    Response = graphql:execute(Ctx, AST), % <2>
    ResponseBody = jsx:encode(fixup(Response)), % <3>
    {http, 200, [], ResponseBody}.


%% @private
gather(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Bindings = cowboy_req:bindings(Req2),
    Params = maps:from_list(Bindings),
    case nklib_json:decode(Body) of
        JSON when is_map(JSON) ->
            gather(JSON, Params);
        _ ->
            {error, invalid_json_body}
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



%% Ground types
fixup(Term) when is_number(Term) -> Term;
fixup(Term) when is_atom(Term) -> Term;
fixup(Term) when is_binary(Term) -> Term;
%% Compound types
fixup(Term) when is_list(Term) ->
    [fixup(T) || T <- Term];
fixup(Term) when is_map(Term) ->
    KVs = maps:to_list(Term),
    maps:from_list([{fixup_key(K), fixup(V)} || {K, V} <- KVs]);
fixup(Term) ->
    %% Every other term is transformed into a binary value
    iolist_to_binary(
        io_lib:format("~p", [Term])).

fixup_key(Term) ->
    case fixup(Term) of
        T when is_binary(T) ->
            T;
        T ->
            iolist_to_binary(io_lib:format("~p", [T]))
    end.



err(Code, Msg) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Msg])),
    Err = #{
        type => error,
         message => Formatted
    },
    Body = nklib_json:encode_pretty(#{errors => [Err]}),
    {http, Code, [], Body}.

