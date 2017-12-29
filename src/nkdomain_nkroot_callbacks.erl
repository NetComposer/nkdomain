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

%% @doc NkDomain service callback module
-module(nkdomain_nkroot_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([object_db_init/1, object_db_read/1, object_db_save/1, object_db_delete/1]).
-export([object_db_find_obj/2, object_db_search_objs/4, object_db_agg_objs/4,
         object_db_iterate_objs/6, object_db_clean/0,
         object_db_get_query/4, object_db_get_agg/4]).
-export([service_init/2, service_handle_cast/2, service_handle_info/2]).


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN NKROOT Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: nkdomain:obj_id().
-type type() :: nkdomain:type().
%-type path() :: nkdomain:path().


%% ===================================================================
%% Service callbacks
%% ===================================================================


%% @private
service_init(_Service, State) ->
    nkdomain_nkroot_plugin:init(State).


%% @private
service_handle_cast(nkdomain_load_domain, State) ->
    #{id:=SrvId} = State,
    #{domain:=Domain} = SrvId:config(),
    case nkdomain_db:load(Domain) of
        #obj_id_ext{type = ?DOMAIN_DOMAIN, obj_id=ObjId, path=Path, pid=Pid} ->
            lager:info("Service loaded domain ~s (~s)", [Path, ObjId]),
            monitor(process, Pid),
            DomainData = #{
                domain_obj_id => ObjId,
                domain_path => Path,
                domain_pid => Pid
            },
            nkservice_srv:put(SrvId, nkdomain_data, DomainData),
            State2 = State#{nkdomain => DomainData},
            {noreply, State2};
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [Domain, Error]),
            {noreply, State}
    end;

service_handle_cast(_Msg, _State) ->
    continue.


%% @private
service_handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case State of
        #{nkdomain:=#{domain_pid:=Pid, domain_path:=Path}} ->
            lager:info("Service received domain '~s' down", [Path]),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
service_handle_info(_Msg, _State) ->
    continue.



%% ===================================================================
%% DB Management
%%
%% This callbacks will be implemented by a plugin like nkdomain_store_es
%% ===================================================================


%% @doc Called to initialize the database
-spec object_db_init(nkservice:service()) ->
    {ok, nkservice:service()}| {error, term()}.

object_db_init(_State) ->
    {error, db_not_defined}.



%% @doc Reads an object from main database
-spec object_db_read(obj_id()) ->
    {ok, nkdomain:obj(), Meta::map()} | {error, term()}.

object_db_read(_ObjId) ->
    {error, db_not_defined}.


%% @doc Saves an object to database
-spec object_db_save(nkdomain:obj()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_save(_Obj) ->
    {error, db_not_defined}.


%% @doc Deletes an object from database
-spec object_db_delete(nkdomain:obj_id()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_delete(_ObjId) ->
    {error, db_not_defined}.


%% @doc Finds an object from its ID or Path
-spec object_db_find_obj(nkdomain:id(), FindDeleted::boolean()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

object_db_find_obj(_ObjId, _FindDeleted) ->
    {error, db_not_defined}.


%%%% @doc
%%-spec object_db_search_types(obj_id(), nkdomain:search_spec()) ->
%%    {ok, Total::integer(), [{Srv::binary(), type(), integer()}]} | {error, term()}.
%%
%%object_db_search_types(_ObjId, _Spec) ->
%%    {error, db_not_defined}.
%%
%%
%%%% @doc
%%-spec object_db_search_all_types(path(), nkdomain:search_spec()) ->
%%    {ok, Total::integer(), [{type(), integer()}]} | {error, term()}.
%%
%%object_db_search_all_types(_ObjId, _Spec) ->
%%    {error, db_not_defined}.


%%%% @doc
%%-spec object_db_search_childs(obj_id(), nkdomain:search_spec()) ->
%%    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
%%    {error, term()}.
%%
%%object_db_search_childs(_ObjId, _Spec) ->
%%    {error, db_not_defined}.
%%
%%
%%%% @doc
%%-spec object_db_search_all_childs(path(), nkdomain:search_spec()) ->
%%    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
%%    {error, term()}.
%%
%%object_db_search_all_childs(_Path, _Spec) ->
%%    {error, db_not_defined}.


%%%% @doc
%%-spec object_db_search(nkdomain:search_spec()) ->
%%    {ok, Total::integer(), Objs::[map()], map(), Meta::map()} |
%%    {error, term()}.
%%
%%object_db_search(_Spec) ->
%%    {error, db_not_defined}.


%% @doc This function must be implemented by DB plugins like nkdomain_store_es
-spec object_db_search_objs(nkservice:id(), type()|core, nkdomain_db:search_type(), nkdomain_db:opts()) ->
    {ok, Total::integer(), [nkdomain_db:search_objs()]}| {error, term()}.

object_db_search_objs(_SrvId, _Type, _SearchType, _DbOpts) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_iterate_objs(nkservice:id(), type()|core, nkdomain_db:search_type(),
                             nkdomain_db:iterate_fun(), term(), nkdomain_db:opts()) ->
    {ok, term()} | {error, term()}.

object_db_iterate_objs(_SrvId, _Type, _SearchType, _Fun, _Acc, _DbOpts) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_agg_objs(nkservice:id(), type()|core, nkdomain_db:aggregation_type(), nkdomain_db:opts()) ->
    {ok, Total::integer(), [{binary(), integer()}]}| {error, term()}.

object_db_agg_objs(_SrvId, _Type, _AggType, _DbOpts) ->
    {error, db_not_defined}.



%%%% @doc
%%-spec object_db_iterate_agg_field(nkdomain:id(), binary(),
%%                                 nkdomain:search_spec(), SubChilds::boolean()) ->
%%                                    {ok, Total::integer(), [{nkdomain:type(), integer()}], Map::map()} | {error, term()}.
%%
%%object_db_search_agg_field(_Id, _Field, _Spec, _SubChilds) ->
%%    {error, db_not_defined}.


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_do_active/3 for each 'active' object found
-spec object_db_clean() ->
    ok | {error, term()}.

object_db_clean() ->
    {error, db_not_defined}.



-type core_query() ::
    {query_graphql, Filters::term(), nkdomain_db:search_objs_opts()} |
    {query_alias, binary(), nkdomain_db:search_objs_opts()} |
    {query_paths, nkdomain:id(), nkdomain_db:search_objs_opts() | #{deep=>boolean(), subtypes=>[binary()], sort => path | rpath}} |
    {query_childs, nkdomain:id(), nkdomain_db:search_objs_opts() | #{sort => path | rpath}} |
    {query_child_roles, nkdomain:id(), [type()], User::obj_id(), nkdomain_db:search_objs_opts()}.


%% @doc Called when a backend needs to process a query
-spec object_db_get_query(module(), type()|core, core_query()|nkdomain_db:search_type(), nkdomain_db:opts()) ->
    {ok, term()} | {error, term()}.

object_db_get_query(nkelastic, core, {query_graphql, Filters, Opts}, DbOpts) ->
    {ok, {nkelastic, Filters, maps:merge(DbOpts, Opts)}};

object_db_get_query(nkelastic, core, {query_alias, Alias, Opts}, DbOpts) ->
    {ok, {nkelastic, [{aliases, eq, to_bin(Alias)}], maps:merge(DbOpts, Opts)}};

object_db_get_query(nkelastic, core, {query_paths, Domain, Opts}, DbOpts) ->
    case nkdomain_store_es_util:domain_filter(Domain, Opts) of
        {ok, FilterList1} ->
            FilterList2 = case Opts of
                #{subtypes:=SubTypes} ->
                    [{subtype, values, SubTypes}|FilterList1];
                _ ->
                    FilterList1
            end,
            Opts2 = case Opts of
                #{sort:=Sort} ->
                    case Sort of
                        path -> Opts#{sort:=[<<"asc:path">>]};
                        rpath -> Opts#{sort:=[<<"desc:path">>]};
                        _ -> Opts
                    end;
                _ ->
                    Opts
            end,
            {ok, {nkelastic, FilterList2, maps:merge(DbOpts, Opts2)}};
        {error, Error2} ->
            {error, Error2}
    end;

object_db_get_query(nkelastic, core, {query_childs, Id, Opts}, DbOpts) ->
    case nkdomain_store_es_util:get_obj_id(Id) of
        {ok, ObjId} ->
            FilterList = [{parent_id, eq, ObjId}],
            Opts2 = case Opts of
                #{sort:=Sort} ->
                    case Sort of
                        path -> Opts#{sort:=[<<"asc:path">>]};
                        rpath -> Opts#{sort:=[<<"desc:path">>]};
                        _ -> Opts
                    end;
                _ ->
                    Opts
            end,
            {ok, {nkelastic, FilterList, maps:merge(DbOpts, Opts2)}};
        {error, Error2} ->
            {error, Error2}
    end;

object_db_get_query(nkelastic, core, {query_child_roles, Domain, Roles, User, Opts}, DbOpts) ->
    case nkdomain_store_es_util:domain_filter(Domain, Opts#{deep=>true}) of
        {ok, FilterList1} ->
            case nkdomain_store_es_util:get_obj_id(User) of
                {ok, UserId} ->
                    FilterList2 = [{type, values, Roles}, {parent_id, eq, UserId}|FilterList1],
                    {ok, {nkelastic, FilterList2, maps:merge(DbOpts, Opts)}};
                {error, Error2} ->
                    {error, Error2}
            end;
        {error, Error} ->
            {error, Error}
    end;

object_db_get_query(_Backend, core, QueryType, _DbOpts) ->
    {error, {unknown_query_type, QueryType}};

object_db_get_query(Backend, Type, QueryType, DbOpts) ->
    case nkdomain_util:type_apply(Type, object_db_get_query, [Backend, QueryType, DbOpts]) of
        {ok, Data} ->
            {ok, Data};
        {error, Error} ->
            {error, Error};
        not_exported ->
            {error, {unknown_query_type, QueryType}}
    end.



% Internal search specification
-type core_agg() ::
    {query_types, nkdomain:id(), nkdomain_db:search_objs_opts() | #{deep=>boolean(), size=>integer()}} |
    {query_values, nkdomain:id(), nkdomain_db:search_objs_opts() | #{deep=>boolean(), size=>integer()}}.


%% @doc Called when a backend needs to process an aggregation
-spec object_db_get_agg(module(), type()|core, core_agg() | nkdomain_db:aggregation_type(), nkdomain_db:opts()) ->
    {ok, term()} | {error, term()}.

object_db_get_agg(nkelastic, core, {query_types, Domain, Opts}, DbOpts) ->
    case nkdomain_store_es_util:domain_filter(Domain, Opts) of
        {ok, Filters} ->
            {ok, {nkelastic, Filters, <<"type">>, maps:merge(DbOpts, Opts)}};
        {error, Error2} ->
            {error, Error2}
    end;

object_db_get_agg(nkelastic, core, {query_values, Domain, Field, Opts}, DbOpts) ->
    case nkdomain_store_es_util:domain_filter(Domain, Opts) of
        {ok, Filters} ->
            {ok, {nkelastic, Filters, to_bin(Field), maps:merge(DbOpts, Opts)}};
        {error, Error2} ->
            {error, Error2}
    end;

object_db_get_agg(nkelastic, core, QueryType, _DbOpts) ->
    {error, {unknown_query_type, QueryType}};

object_db_get_agg(Backend, Type, QueryType, DbOpts) ->
    case nkdomain_util:type_apply(Type, object_db_get_agg, [Backend, QueryType, DbOpts]) of
        {ok, Data} ->
            {ok, Data};
        {error, Error} ->
            {error, Error};
        not_exported ->
            {error, {unknown_query_type, QueryType}}
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
