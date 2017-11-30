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

%% @doc Elasticsearch Events plugin
-module(nkdomain_event_es_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).


-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).




%% ===================================================================
%% Plugin Callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [nkdomain_store_es].


%% Other plugins can also parse db_clusters
plugin_syntax() ->
    #{
        nkdomain => #{
            event_store => binary,
            db_clusters => {list, #{
                id => binary,
                class => atom,
                url => binary,
                pool_size => {integer, 1, none},
                pool_overflow => {integer, 1, none},
                replicas => {integer, 1, 5},
                database => binary,
                '__mandatory' => [class],
                '__defaults' => #{id => <<"main">>}
            }}
        }
    }.


%% @private
plugin_config(#{nkdomain:=#{event_store:=Store, db_clusters:=Clusters}}=Config, _Service) ->
    case parse_clusters(Clusters, Store) of
        {ok, IndexOpts, EsOpts} ->
            {ok, Config, {nkdomain_event_es, IndexOpts, EsOpts}};
        {error, Error} ->
            {error, Error};
        not_found ->
            {ok, Config, {}}
    end;

plugin_config(Config, _Service) ->
    {ok, Config, {}}.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
parse_clusters([], _DbStore) ->
    not_found;

parse_clusters([#{id:=DbStore, class:=nkelastic}=Cluster|_], DbStore) ->
    IndexOpts = #{
        number_of_replicas => maps:get(replicas, Cluster, 1)
    },
    EsOpts = #{
        srv_id => ?NKROOT,
        cluster_id => DbStore,
        index => maps:get(database, Cluster, <<"nkevents">>),
        type => <<"events">>,
        type_is_dynamic => true,
        refresh => false
    },
    {ok, IndexOpts, EsOpts};

parse_clusters([_Cluster|Rest], _DbStore) ->
    parse_clusters(Rest, _DbStore).
