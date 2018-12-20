%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain service callback module
%%%% It prepares a set of information as cache in service
%% - pbkdfIters: integer()
%% - groups: [Group]
%% - {types, Group}: [Type]
%% - {resource_module, Group, Type}: module()
%% - {resource_config, Group, Type}: config()
%% - {resource_id, Group, Id}: {singular|camel|short_name, Type}
%% They can be accessed from get_domain_cache and the other getters


-module(nkdomain_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_config/3, plugin_start/4, plugin_update/5]).
-export([get_config/1]).
-export([get_domain_cache/2, get_external_urls/1, get_groups/1, get_resources/2,
         get_resource_module/3, get_resource_config/3,
         find_resource/3, get_modules/1]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Plugin: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkpacket/include/nkpacket.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Plugin callbacks
%% ===================================================================



%% @doc
plugin_deps() ->
    [
        nkservice_rest, nkservice_pgsql, nkservice_graphql, nkservice_openapi,
        nkfile, nkfile_filesystem, nkfile_s3
    ].


%% @doc
plugin_config(?PACKAGE_CLASS_DOMAIN, #{config:=Config}=Spec, _Service) ->
    Syntax = #{
        actorModules => {list, module},
        graphqlActorModules => {list, module},
        apiUrl => binary,
        apiUrlOpts => nkpacket_syntax:safe_syntax(),
        apiDebug => {list, {atom, [erlang, ws, http, nkpacket]}},
        debug_groups => {list, binary},
        pbkdfIters => {integer, 1, 100},
        adminPass => binary,
        '__mandatory' => [],
        '__allow_unknown' => true
    },
    case nklib_syntax:parse(Config, Syntax) of
        {ok, Config2, _} ->
            case get_listen(Config2) of
                {ok, _Conns, _ConnOpts, ExtUrls} ->
                    Modules1 = maps:get(actorModules, Config2, []),
                    Modules2 = core_actor_modules(),
                    Modules3 = lists:usort(Modules2++Modules1),
                    Config3 = Config2#{actorModules => Modules3},
                    GraphQlModules1 = maps:get(graphqlActorModules, Config3, []),
                    GraphQlModules2 = core_graphql_modules(),
                    GraphQlModules3 = lists:usort(GraphQlModules2 ++ GraphQlModules1),
                    Config4 = Config3#{graphqlActorModules => GraphQlModules3},
                    Spec2 = Spec#{config:=Config4},
                    Spec3 = add_cache(ExtUrls, Spec2),
                    Spec4 = add_debug(Spec3),
                    {ok, Spec4};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

plugin_config(_Class, _Package, _Service) ->
    continue.


%% @doc
plugin_start(?PACKAGE_CLASS_DOMAIN, #{id:=Id, config:=Config}, Pid, #{id:=SrvId}=Srv) ->
    Domain = maps:get(domain, Srv),
    case nkdomain_init:check_database(SrvId, Domain) of
        ok ->
            {ok, Conns, Opts, _ExtUrls} = get_listen(Config),
            {ok, Listeners} = make_listen_transps(SrvId, Conns, Opts),
            insert_listeners(Id, Pid, Listeners);
        {error, Error} ->
            {error, Error}
    end;

plugin_start(_Id, _Spec, _Pid, _Service) ->
    continue.


%% @doc
%% Even if we are called only with modified config, we check if the spec is new
plugin_update(?PACKAGE_CLASS_DOMAIN, #{id:=Id, config:=#{apiUrl:=_}=NewConfig}, OldSpec, Pid, #{id:=SrvId}) ->
    case OldSpec of
        #{config:=NewConfig} ->
            ok;
        _ ->
            {ok, Conns, Opts, _ExtUrls} = get_listen(NewConfig),
            {ok, Listeners} = make_listen_transps(SrvId, Conns, Opts),
            insert_listeners(Id, Pid, Listeners)
    end;

plugin_update(_Class, _NewSpec, _OldSpec, _Pid, _Service) ->
    ok.



%% ===================================================================
%% Core config
%% ===================================================================


core_actor_modules() ->
    [
        nkdomain_configmap_actor,
        nkdomain_contact_actor,
        nkdomain_domain_actor,
        nkdomain_event_actor,
        nkdomain_file_actor,
        nkdomain_file_provider_actor,
        nkdomain_node_actor,
        nkdomain_session_actor,
        nkdomain_task_actor,
        nkdomain_token_actor,
        nkdomain_user_actor
    ].


%% @private
core_graphql_modules() ->
    [
        nkdomain_configmap_actor_graphql,
        nkdomain_contact_actor_graphql,
        nkdomain_domain_actor_graphql,
        nkdomain_event_actor_graphql,
        nkdomain_file_actor_graphql,
        nkdomain_file_provider_actor_graphql,
        nkdomain_node_actor_graphql,
        nkdomain_session_actor_graphql,
        nkdomain_task_actor_graphql,
        nkdomain_token_actor_graphql,
        nkdomain_user_actor_graphql
    ].


%% ===================================================================
%% Getters
%% ===================================================================


%% @doc
get_config(SrvId) ->
    #{packages:=#{?PACKAGE_CLASS_DOMAIN:=#{config:=Config}}} = ?CALL_SRV(SrvId, service),
    Config.


-type cache_key() ::
    domain |
    externalUrls |
    actor_id |
    pbkdfIters |
    groups |
    {resources, nkservice_actor:group()} |
    {resource_module, nkservice_actor:group(), nkservice_actor:resource()} |
    {resource_config, nkservice_actor:group(), nkservice_actor:resource()} |
    {resource_id, nkservice_actor:group(), Id::binary()}.


%% @doc Gets cached config
-spec get_domain_cache(nkservice:id(), cache_key()) ->
    term().

get_domain_cache(SrvId, CacheKey) ->
    nkservice_util:get_cache(SrvId, nkdomain, single, CacheKey).


%% @doc Gets all registered external Urls
-spec get_external_urls(nkservice:id()) ->
    [binary()].

get_external_urls(SrvId) ->
    get_domain_cache(SrvId, externalUrls).


%% @doc Gets all registered groups
-spec get_groups(nkservice:id()) ->
    [nkservice_actor:group()].

get_groups(SrvId) ->
    get_domain_cache(SrvId, groups).


%% @doc Gets all registered resources
-spec get_resources(nkservice:id(), nkservice_actor:group()) ->
    [nkservice_actor:resource()].

get_resources(SrvId, Group) ->
    get_domain_cache(SrvId, {resources, to_bin(Group)}).


%% @doc Gets the module callback for a resource
-spec get_resource_module(nkservice:id(), nkservice_actor:group(), nkservice_actor:resource()) ->
    module() | undefined.

get_resource_module(SrvId, Group, Resource) ->
    get_domain_cache(SrvId, {resource_module, to_bin(Group), to_bin(Resource)}).


%% @doc Gets the config for a resource
-spec get_resource_config(nkservice:id(), nkservice_actor:group(), nkservice_actor:resource()) ->
    nkdomain:config() | undefined.

%% @doc Gets the module config for a resource
get_resource_config(SrvId, Group, Resource) ->
    get_domain_cache(SrvId, {resource_config, to_bin(Group), to_bin(Resource)}).


%% @doc Gets the resource for an alternative type id
-spec find_resource(nkservice:id(), nkservice_actor:group(), binary()) ->
    {camel|short_name, nkservice_actor:resource()} | undefined.

find_resource(SrvId, Group, Id) ->
    case catch get_domain_cache(SrvId, {resource_id, to_bin(Group), to_bin(Id)}) of
        {'EXIT', _} ->
            undefined;
        {IdType, Type} ->
            {IdType, Type}
    end.


%% @doc Find detected modules in config
-spec get_modules(nkservice:id()) ->
    module().

get_modules(SrvId) ->
    Config = nkservice_util:get_config(SrvId, ?PACKAGE_CLASS_DOMAIN),
    maps:get(actorModules, Config).


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
add_cache(ExtUrls, #{config:=Config}=Spec) ->
    CacheItems1 = #{
        externalUrls => ExtUrls,
        pbkdfIters => maps:get(pbkdfIters, Config, 1)
    },
    Modules = maps:get(actorModules, Config),
    CacheItems2 = make_type_cache(Modules, CacheItems1),
    nkservice_config_util:set_cache_items(nkdomain, single, CacheItems2, Spec).


%% @private
add_debug(#{config:=Config}=Spec) ->
    DebugItems1 = lists:foldl(
        % nkdomain_api
        fun(Type, Acc) -> Acc#{{api, Type} => true} end,
        #{},
        maps:get(apiDebug, Config, [])),
    DebugItems2 = lists:foldl(
        fun(Group, Acc) -> Acc#{{group, Group} => true} end,
        DebugItems1,
        maps:get(debug_groups, Config, [])),
    nkservice_config_util:set_debug_items(nkdomain, single, DebugItems2, Spec).


%% @private
make_type_cache([], Cache) ->
    Cache;

make_type_cache([Module|Rest], Cache) ->
    code:ensure_loaded(Module),
    Config = case nkservice_actor:config(Module) of
        not_exported ->
            ?LLOG(error, "Invalid actor callback module '~s'", [Module]),
            error({module_unknown, Module});
        Config0 ->
            Config0
    end,
    #{group:=Group} = Config,
    Groups1 =  maps:get(groups, Cache, []),
    Groups2 = lists:usort([Group|Groups1]),
    Cache2 = Cache#{groups => Groups2},
    Cache3 = make_resources_cache_module(Config#{module=>Module}, Cache2),
    make_type_cache(Rest, Cache3).


%% @private
make_resources_cache_module(Config, Cache) ->
    #{group:=Group, resource:=Res} = Config,
    Singular = case Config of
        #{singular:=S0} ->
            S0;
        _ ->
            nkservice_actor_util:make_singular(Res)
    end,
    Camel = case Config of
        #{camel:=C0} ->
            to_bin(C0);
        _ ->
            nklib_util:to_capital(Singular)
    end,
    ShortNames = case Config of
        #{short_names:=SN} ->
            [to_bin(N) || N <- SN];
        _ ->
            []
    end,
    Verbs = case Config of
        #{verbs:=Vs} ->
            [binary_to_atom(to_bin(V), utf8) || V <- Vs];
        _ ->
            [get, list, create]
    end,
    Versions = maps:get(versions, Config),
    FilterFields1 = maps:get(filter_fields, Config, []),
    FilterFields2 = [to_bin(Field) || Field <- FilterFields1],
    FilterFields3 = nkservice_actor:filter_fields(),
    FilterFields4 = lists:usort(FilterFields2++FilterFields3),
    SortFields1 = maps:get(sort_fields, Config, []),
    SortFields2 = [to_bin(Field) || Field <- SortFields1],
    SortFields3 = nkservice_actor:sort_fields(),
    SortFields4 = lists:usort(SortFields2++SortFields3),
    FieldType1 = maps:to_list(maps:get(field_type, Config, #{})),
    FieldType2 = maps:from_list([{to_bin(Field), Value} || {Field, Value} <- FieldType1]),
    FieldType3 = nkservice_actor:field_type(),
    FieldType4 = maps:merge(FieldType3, FieldType2),
    FieldTrans1 = maps:to_list(maps:get(field_trans, Config, #{})),
    FieldTrans2 = maps:from_list([{to_bin(Field), Value} || {Field, Value} <- FieldTrans1]),
    FieldTrans3 = nkservice_actor:field_trans(),
    FieldTrans4 = maps:merge(FieldTrans3, FieldTrans2),
    StaticFields1 = maps:get(immutable_fields, Config, []),
    StaticFields2 = [to_bin(Field) || Field <- StaticFields1],
    StaticFields3 = lists:usort(StaticFields2),
    Config2 = Config#{
        resource => Res,
        singular => Singular,
        camel => Camel,
        short_names => ShortNames,
        verbs => Verbs,
        versions => Versions,
        filter_fields => FilterFields4,
        sort_fields => SortFields4,
        field_type => FieldType4,
        field_trans => FieldTrans4,
        immutable_fields => StaticFields3
    },
    Resources1 = maps:get({resources, Group}, Cache, []),
    Resources2 = lists:usort([Res|Resources1]),
    Cache2 = Cache#{{resources, Group} => Resources2},
    make_type_caches_group(Group, Config2, Cache2).


%% @private
make_type_caches_group(Group, Config, Cache) ->
    #{
        module := Module,
        group := Group,
        resource := Resource,
        singular := Singular,
        camel := Camel,
        short_names := ShortNames
    } = Config,
    Cache2 = Cache#{
        {resource_module, Group, Resource} => Module,
        {resource_config, Group, Resource} => Config,
        {resource_id, Group, Singular} => {singular, Resource},
        {resource_id, Group, Camel} => {camel, Resource}
    },
    lists:foldl(
        fun(FSN, FCache) -> FCache#{{resource_id, Group, FSN} => {short_name, Resource}} end,
        Cache2,
        ShortNames).


%% @private
get_listen(#{apiUrl:=Url}=Entry) ->
    ApiOpts = maps:get(apiUrlOpts, Entry, #{}),
    ResolveOpts = ApiOpts#{
        resolve_type => listen,
        protocol => nkservice_rest_protocol
    },
    case nkpacket_resolve:resolve(Url, ResolveOpts) of
        {ok, Conns} ->
            ExtUrls = [maps:get(external_url, Opts) || #nkconn{opts=Opts} <- Conns],
            Debug = maps:get(apiDebug, Entry, []),
            ApiOpts2 = ApiOpts#{
                debug => lists:member(nkpacket, Debug)
            },
            {ok, Conns, ApiOpts2, ExtUrls};
        {error, Error} ->
            {error, Error}
    end;

get_listen(_Entry) ->
    {ok, [], #{}, []}.


%% @private
make_listen_transps(SrvId, Conns, Opts) ->
    make_listen_transps(SrvId, ?DOMAIN_PKG_ID_API, Conns, Opts, []).


%% @private
make_listen_transps(_SrvId, _Id, [], _Opts, Acc) ->
    {ok, Acc};

make_listen_transps(SrvId, Id, [Conn|Rest], Opts, Acc) ->
    #nkconn{opts=ConnOpts} = Conn,
    Opts2 = maps:merge(ConnOpts, Opts),
    Opts3 = Opts2#{
        id => Id,
        class => {nkservice_rest, SrvId, Id},
        path => maps:get(path, Opts2, <<"/">>),
        get_headers => [<<"user-agent">>],
        idle_timeout => 500000,
        cowboy_opts => #{
            inactivity_timeout => 500000,
            request_timeout => 500000
        }
    },
    Conn2 = Conn#nkconn{opts=Opts3},
    case nkpacket:get_listener(Conn2) of
        {ok, Id, Spec} ->
            make_listen_transps(SrvId, Id, Rest, Opts, [Spec|Acc]);
        {error, Error} ->
            {error, Error}
    end.


%% @private
insert_listeners(Id, Pid, SpecList) ->
    case nkservice_packages_sup:update_child_multi(Pid, SpecList, #{}) of
        ok ->
            ?LLOG(debug, "started ~s", [Id]),
            ok;
        not_updated ->
            ?LLOG(debug, "didn't upgrade ~s", [Id]),
            ok;
        upgraded ->
            ?LLOG(info, "upgraded ~s", [Id]),
            ok;
        {error, Error} ->
            ?LLOG(notice, "start/update error ~s: ~p", [Id, Error]),
            {error, Error}
    end.



%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).