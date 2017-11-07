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
-module(nkdomain_store_es_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([error/1]).
-export([object_db_init/1, object_db_read/1, object_db_save/1, object_db_delete/1,
         object_db_find_obj/1, object_db_search/1, object_db_search_alias/1,
         object_db_search_childs/2, object_db_search_all_childs/2,
         object_db_search_types/2, object_db_search_all_types/2, object_db_search_agg_field/4,
         object_db_delete_all_childs/2, object_db_clean/0]).


-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(_) -> continue.






%% ===================================================================
%% Implemented callbacks
%% ===================================================================

%% @doc Initializes database
-spec object_db_init(nkservice:state()) ->
    {ok, nkservice:state()} | {error, term()}.

object_db_init(State) ->
    case nkdomain_store_es_util:get_index_opts() of
        {ok, IndexOpts, EsOpts} ->
            case nkdomain_store_es_util:db_init(IndexOpts, EsOpts) of
                ok ->
                    {ok, State};
                {error, Error} ->
                    {error, {object_db_init, Error}}
            end;
        _ ->
            continue
    end.


%% @doc Called to get and parse an object
-spec object_db_read(nkdomain:obj_id()) ->
    {ok, map(), Meta::map()} | {error, term()}.

object_db_read(ObjId) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkelastic:get(ObjId, EsOpts);
        _ ->
            continue
    end.


%% @doc Saves an object to database
-spec object_db_save(nkdomain:obj()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_save(#{obj_id:=ObjId}=Obj) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            Map = nkdomain_store_es_util:unparse(Obj),
            nkelastic:put(ObjId, Map, EsOpts);
        _ ->
            continue
    end.


%% @doc Deletes an object to database
-spec object_db_delete(nkdomain:obj_id()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_delete(ObjId) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            case nkdomain_store_es_search:search_childs(ObjId, #{size=>0}, EsOpts) of
                {ok, 0, []} ->
                    case nkdomain_lib:find_loaded(ObjId) of
                        #obj_id_ext{pid=Pid} ->
                            nkdomain_obj:object_deleted(Pid);
                        not_found ->
                            ok
                    end,
                    nkelastic:delete(ObjId, EsOpts);
                {ok, _, _} ->
                    {error, object_has_childs};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.


%% @doc Finds an object from its ID or Path
-spec object_db_find_obj(nkdomain:id()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

object_db_find_obj(Id) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:find_obj(Id, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search(nkdomain:search_spec()) ->
    {ok, Total::integer(), Objs::[map()], Aggs::map(), Meta::map()} |
    {error, term()}.

object_db_search(Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search(Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_alias(nkdomain:alias()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], Meta::map()} |
    {error, term()}.

object_db_search_alias(Alias) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search_obj_alias(Alias, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_types(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), integer()}], Meta::map()} | {error, term()}.

object_db_search_types(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search_types(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_all_types(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_all_types(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search_all_types(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_childs(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], Meta::map()} |
    {error, term()}.

object_db_search_childs(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search_childs(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_all_childs(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{nkdomain:type(), nkdomain:obj_id(), nkdomain:path()}], Meta::map()} |
    {error, term()}.

object_db_search_all_childs(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search_all_childs(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc Must stop loaded objects
-spec object_db_delete_all_childs(nkdomain:id(), nkdomain:search_spec()) ->
    {ok, Total::integer()} | {error, term()}.

object_db_delete_all_childs(Id, Spec) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:delete_all_childs(Id, Spec, EsOpts);
        _ ->
            continue
    end.


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_do_active/3 for each 'active' object found
-spec object_db_clean() ->
    ok | {error, term()}.

object_db_clean() ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:clean(EsOpts);
        _ ->
            continue
    end.


%% @doc
-spec object_db_search_agg_field(nkdomain:id(), binary(),
                                 nkdomain:search_spec(), SubChilds::boolean()) ->
    {ok, Total::integer(), [{nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_agg_field(Id, Field, Spec, SubChilds) ->
    case nkdomain_store_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_store_es_search:search_agg_field(Id, Field, Spec, SubChilds, EsOpts);
        _ ->
            continue
    end.

