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

%% @doc NkDomain main module
-module(nkdomain_graphql_rest_handler).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

%% Cowboy Handler Interface
-export([init/2]).

%% REST callbacks
-export([
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2,
    charsets_provided/2
]).

%% Data input/output callbacks
-export([
    from_json/2,
    to_json/2,
    to_html/2
]).


%% -- API ---------------------------------------------------

init(_Req, _Options) ->
    {upgrade, protocol, cowboy_rest}.



rest_init(Req, {priv_file, _, _} = PrivFile) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req2,
     #{ method => Method,
        index_location => PrivFile }}.



allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.



content_types_accepted(Req, State) ->
    {[
         {{<<"application">>, <<"json">>, []}, from_json}
     ], Req, State}.



content_types_provided(Req, State) ->
    {[
         {{<<"application">>, <<"json">>, []}, to_json},
         {{<<"text">>, <<"html">>, []}, to_html}
     ], Req, State}.



charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.



resource_exists(Req, #{ method := <<"GET">> } = State) ->
    {true, Req, State};
resource_exists(Req, #{ method := <<"POST">> } = State) ->
    {false, Req, State}.



to_html(Req, #{ index_location :=
                {priv_file, App, FileLocation}} = State) ->
    Filename = filename:join(code:priv_dir(App), FileLocation),
    {ok, Data} = file:read_file(Filename),
    {Data, Req, State}.



json_request(Req, State) ->
    case gather(Req) of
        {error, Reason} ->
            err(400, Reason, Req, State);
        {ok, Req2, Decoded} ->
            run_request(Decoded, Req2, State)
    end.

from_json(Req, State) -> json_request(Req, State).
to_json(Req, State) -> json_request(Req, State).


%% -- INTERNAL FUNCTIONS ---------------------------------------


run_request(#{ document := undefined }, Req, State) ->
    err(400, no_query_supplied, Req, State);
run_request(#{ document := Doc} = ReqCtx, Req, State) ->
    case graphql:parse(Doc) of
        {ok, AST} ->
            run_preprocess(ReqCtx#{ document := AST }, Req, State);
        {error, Reason} ->
            err(400, Reason, Req, State)
    end.



run_preprocess(#{ document := AST } = ReqCtx, Req, State) ->
    try
        Elaborated = graphql:elaborate(AST), % <1>
        {ok, #{
            fun_env := FunEnv,
            ast := AST2 }} = graphql:type_check(Elaborated), % <2>
        ok = graphql:validate(AST2), % <3>
        run_execute(ReqCtx#{ document := AST2, fun_env => FunEnv }, Req, State)
    catch
        throw:Err ->
            err(400, Err, Req, State)
    end.



run_execute(#{ document := AST,
               fun_env := FunEnv,
               vars := Vars,
               operation_name := OpName }, Req, State) ->
    Coerced = graphql:type_check_params(FunEnv, OpName, Vars), % <1>
    Ctx = #{
        params => Coerced,
        operation_name => OpName },
    Response = graphql:execute(Ctx, AST), % <2>
    ResponseBody = term_to_json(Response), % <3>
    Req2 = cowboy_req:set_resp_body(ResponseBody, Req), % <4>
    {ok, Reply} = cowboy_req:reply(200, Req2),
    {halt, Reply, State}.



gather(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Bindings, Req3} = cowboy_req:bindings(Req2),
    Params = maps:from_list(Bindings),
    try jsx:decode(Body, [return_maps]) of
        JSON ->
            gather(Req3, JSON, Params)
    catch
        error:badarg ->
            {error, invalid_json_body}
    end.

gather(Req, Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {ok, Vars} ->
            Operation = operation_name([Params, Body]),
            {ok, Req, #{ document => QueryDocument,
                         vars => Vars,
                         operation_name => Operation}};
        {error, Reason} ->
            {error, Reason}
    end.



document([#{ <<"query">> := Q }|_]) -> Q;
document([_|Next]) -> document(Next);
document([]) -> undefined.



variables([#{ <<"variables">> := Vars} | _]) ->
    if
        is_binary(Vars) ->
            try jsx:decode(Vars, [return_maps]) of
                null -> {ok, #{}};
                JSON when is_map(JSON) -> {ok, JSON};
                _ -> {error, invalid_json}
            catch
                error:badarg ->
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



operation_name([#{ <<"operationName">> := OpName } | _]) ->
    OpName;
operation_name([_ | Next]) ->
    operation_name(Next);
operation_name([]) ->
    undefined.




err(Code, Msg, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Msg])),
    Err = #{ type => error,
             message => Formatted },
    Body = jsx:encode(#{ errors => [Err] }),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {ok, Reply} = cowboy_req:reply(Code, Req2),
    {halt, Reply, State}.




%%% -------- Util

term_to_json(Term) ->
    jsx:encode(fixup(Term)).

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
