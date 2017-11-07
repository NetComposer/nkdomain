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
-export([object_db_find_obj/1, object_db_search/1, object_db_search_alias/1,
         object_db_search_types/2, object_db_search_all_types/2,
         object_db_search_childs/2, object_db_search_all_childs/2, object_db_search_agg_field/4,
         object_db_delete_all_childs/2, object_db_clean/0]).
-export([service_init/2, service_handle_cast/2, service_handle_info/2]).


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN NKROOT Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: nkdomain:obj_id().
-type type() :: nkdomain:type().
-type path() :: nkdomain:path().





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
    case nkdomain_lib:load(Domain) of
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
-spec object_db_find_obj(nkdomain:id()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

object_db_find_obj(_ObjId) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_types(obj_id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), integer()}]} | {error, term()}.

object_db_search_types(_ObjId, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_all_types(path(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{type(), integer()}]} | {error, term()}.

object_db_search_all_types(_ObjId, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_childs(obj_id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_childs(_ObjId, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_all_childs(path(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_all_childs(_Path, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_alias(nkdomain:alias()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_alias(_Alias) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search(nkdomain:search_spec()) ->
    {ok, Total::integer(), Objs::[map()], map(), Meta::map()} |
    {error, term()}.

object_db_search(_Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_agg_field(nkdomain:id(), binary(),
                                 nkdomain:search_spec(), SubChilds::boolean()) ->
                                    {ok, Total::integer(), [{nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_agg_field(_Id, _Field, _Spec, _SubChilds) ->
    {error, db_not_defined}.


%% @doc Must stop loaded objects
-spec object_db_delete_all_childs(path(), nkdomain:search_spec()) ->
    {ok, Total::integer()} | {error, term()}.

object_db_delete_all_childs(_Path, _Spec) ->
    {error, db_not_defined}.


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_do_active/3 for each 'active' object found
-spec object_db_clean() ->
    ok | {error, term()}.

object_db_clean() ->
    {error, db_not_defined}.


