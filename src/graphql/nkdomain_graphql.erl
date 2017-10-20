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
-module(nkdomain_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([load_schema/0, get_listen/3, rest/3]).
-export([start_cowboy/0]).




%%====================================================================
%% GraphQL
%%====================================================================


load_schema() ->
    PrivDir = code:priv_dir(nkdomain),
    {ok, SchemaData} = file:read_file(filename:join(PrivDir, "graphql.schema")),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.


mapping_rules() ->
    #{
        scalars => #{
            default => nkdomain_graphql_scalar
        },
        interfaces => #{
            default => nkdomain_graphql_type
        },
        unions => #{
            default => nkdomain_graphql_type
        },
        objects => #{
            'User' => nkdomain_user_obj,
            'Query' => nkdomain_graphql_query,
            'Mutation' => nkdomain_graphql_mutation,
            default => nkdomain_graphql_object
        }
    }.

setup_root() ->
    Root = {root,
            #{ query => 'Query',
               mutation => 'Mutation',
               interfaces => ['Node']
            }},
    ok = graphql:insert_schema_definition(Root),
    ok.


start_cowboy() ->
    Dispatch =
    cowboy_router:compile(
        [{'_',
          [{"/assets/[...]", cowboy_static,{priv_dir, sw_web, "site/assets"}},
           {"/", nkdomain_graphql_rest_handler, {priv_file, sw_web, "site/index.html"}}
          ]}]),
    cowboy:start_http(sw_http, 100,
                      [{port, 9701}],
                      [{compress, true},
                       {env, [{dispatch, Dispatch}]},

                       %% Bump the default limit of 4096 to 65536 to allow us to submit
                       %% slightly larger, human readable, query documents. The limit of
                       %% 65536 is chosen to allow us to have 16 times bigger documents
                       %% than the default where we hit the limit of 4096. If you are
                       %% hitting the bumped limit you should probably consider splitting
                       %% up your query document into two.
                       %%
                       %% Caveat: If you are testing on localhost you might not see the
                       %% max limit have any effect since the socket might make the entire
                       %% HTTP request available when cowboy does a gen_tcp:read(Socket, 0)
                       %% and will ignore the limit.
                       {max_request_line_length, 65536},

                       %% Bump the default limit of 4096 on Header lengths to 16384. The
                       %% problem is we will eventually get a very large document as a
                       %% referrer from GraphiQL and this will break the server side as it
                       %% has to process through that header
                       {max_header_value_length, 16384}
                      ]).


rest(get, [], _Req) ->
    {redirect, "/index.html"};

rest(get, [<<"index.html">>], _Req) ->
    rest_serve("index.html");

rest(get, [<<"assets">>, File], _Req) ->
    rest_serve(<<"assets/", File/binary>>).




rest_serve(File) ->
    Priv = list_to_binary(code:priv_dir(nkdomain)),
    DirPath = filename:join([Priv, "graphiql", File]),
    case file:read_file(DirPath) of
        {ok, Bin} ->
            {http, 200, [], Bin};
        {error, Error} ->
            lager:error("Could not read file ~s: ~p", [DirPath, Error]),
            {http, 500, [], <<"file read error">>}
    end.




get_listen(SrvId, Url, Config) ->
    Opts = #{resolve_type=>listen},
    {ok, List} = nkpacket:multi_resolve(Url, Opts),
    NetOpts = nkpacket_util:get_plugin_net_opts(Config),
    PacketDebug = case Config of
        #{debug:=DebugList} when is_list(DebugList) ->
            lists:member(nkpacket, DebugList);
        _ ->
            false
    end,
    Priv = list_to_binary(code:priv_dir(nkdomain)),
    DirPath = <<Priv/binary, "/graphql">>,
    DirPath2 = nklib_parse:fullpath(filename:absname(DirPath)),
    RestOpts = #{dir_path=>DirPath2},
    L = lists:map(
        fun({Conns, ConnOpts}) ->
            UrlPath = maps:get(path, ConnOpts, <<>>),
            lager:error("NKLOG URL ~p", [UrlPath]),
            Route = {<<UrlPath/binary, "/[...]">>, nkdomain_graphql_rest_handler, RestOpts},
            ConnOpts2 = maps:merge(ConnOpts, NetOpts),
            ConnOpts3 = ConnOpts2#{
                class => {nkdomain_graphql, SrvId},
                http_proto => {custom, #{
                    env => [{dispatch, cowboy_router:compile([{'_', [Route]}])}],
                    middlewares => [cowboy_router, cowboy_handler]
                }},
                debug => PacketDebug
            },
            {Conns, ConnOpts3}
        end,
        List),
    lager:error("GRAOHL: ~p", [L]),
    L.
