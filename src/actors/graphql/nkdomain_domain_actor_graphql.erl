%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain Actor Domain Graphql
-module(nkdomain_domain_actor_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behavior(nkservice_graphql_schema).

-export([config/0, schema/1, query/5, execute/5]).
-import(nkdomain_graphql_execute, [get_value/2, get_object/2]).

-include("nkdomain.hrl").

%% ===================================================================
%% Internal
%% ===================================================================


config() ->
    #{
        type => ?KIND_CORE_DOMAIN,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_DOMAINS
    }.


schema(types) ->
    #{
        'DomainHttpPool' => #{
            fields => #{
                maxConnections => integer,
                timeout => integer
            }
        },
        'DomainSpec' => #{
            fields => #{
                httpPool => {'DomainHttpPool', #{comment => "HttpPool data"}}
            }
        },
        'Domain' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => 'DomainSpec',
                status => 'ActorStatus'
            }),
            comment => "A Domain",
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{})
        }
    };


schema(queries) ->
    #{
        allActors => nkdomain_graphql_schema:actor_query(<<"Actor">>, #{}),
        allDomains => nkdomain_graphql_schema:actor_query(?KIND_CORE_DOMAIN, #{})
    };


schema(mutations) ->
    #{
        introduceDomain => #{
            input => #{
                domain => string
            },
            output => #{
                objId => {no_null, string}
            },
            comment => "Creates a new domain"
        }
    };

schema(_) ->
    #{}.


%% @doc
query(SrvId, <<"allActors">>, Params, _Meta, _Ctx) ->
    nkdomain_graphql_search:search(SrvId, Params, #{});

query(SrvId, <<"allDomains">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => ?KIND_CORE_DOMAIN},
    nkdomain_graphql_search:search(SrvId, Params, Opts);

query(_SrvId, _Name, _Params, _Meta, _Ctx) ->
    continue.


%% @private
execute(SrvId, Field, {nkdomain, {spec, _Type, Spec, _Actor}}, _Meta, _Params) ->
    case Field of
        _ when Field==<<"httpPool">> ->
            List = maps:get(Field, Spec, #{}),
            get_object(SrvId, {field, ?KIND_CORE_DOMAIN, Field, List});
        _ ->
            continue
    end;

execute(_SrvId, Field, {nkdomain, {field, ?KIND_CORE_DOMAIN, <<"httpPool">>, Data}}, _Meta, _Params) ->
    get_value(Field, Data);

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.
