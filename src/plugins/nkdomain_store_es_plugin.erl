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

%% @doc Elasticsearch plugin
-module(nkdomain_store_es_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).


-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).




%% ===================================================================
%% Plugin Callbacks
%% ===================================================================

plugin_deps() ->
    [nkdomain_nkroot, nkelastic].


%% Other plugins can also parse db_clusters
plugin_syntax() ->
    #{
        nkdomain => #{
            db_store => binary,
            db_clusters => {list, #{
                id => binary,
                class => atom,
                url => binary,
                pool_size => {integer, 1, none},
                pool_overflow => {integer, 1, none},
                replicas => {integer, 1, 5},
                database => binary,
                '__mandatory' => [class]
            }},
            '__mandatory' => [db_store]
        }
    }.


plugin_config(#{nkdomain:=NkDomain}=Config, _Service) ->
    #{db_store:=DbStore} = NkDomain,
    Clusters = maps:get(db_clusters, NkDomain, []),
    Config2 = parse_clusters(Clusters, DbStore, Config),
    {ok, Config2};

plugin_config(_Config, _Service) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
parse_clusters([], _DbStore, Config) ->
    Config;

parse_clusters([#{class:=nkelastic}=Data|Rest], DbStore, Config) ->
    Id = maps:get(id, Data, <<"main">>),
    Previous = maps:get(nkelastic_pools, Config, []),
    Data2 = maps:with([id, url, pool_size, pool_overflow, replicas, database], Data#{id=>Id}),
    Config2 = Config#{nkelastic_pools => [Data2|Previous]},
    Config3 = case DbStore of
        Id ->
            IndexOpts = #{
                number_of_replicas => maps:get(replicas, Data2, 2)
            },
            Database = maps:get(database, Data, <<"nkobjects">>),
            EsOpts = #{
                srv_id => ?NKROOT,
                cluster_id => Id,
                index => Database,
                type => <<"objs">>,
                refresh => true
            },
            % nkdomain_store will be captured by nkdomain and generate cache
            Config2#{nkdomain_db_store=>{elastic, IndexOpts, EsOpts}};
        _ ->
            Config2
    end,
    parse_clusters(Rest, DbStore, Config3);

parse_clusters([_|Rest], DbStore, Config) ->
    parse_clusters(Rest, DbStore, Config).

