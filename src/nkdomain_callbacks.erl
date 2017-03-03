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
-module(nkdomain_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([plugin_deps/0, service_init/2, service_handle_cast/2]).
-export([domain_get_types/1, domain_get_module/1, domain_store_base_mapping/0,
         domain_store_mapping/1, domain_load_obj/3]).
-export([object_init/2, object_terminate/3, object_event/3, object_reg_event/4,
         object_reg_down/4, object_start/2, object_stop/3,
         object_handle_call/4, object_handle_cast/3, object_handle_info/3]).
-export([elastic_get_indices/2, elastic_get_mappings/3, elastic_get_aliases/3]).

-define(ES_INDEX, <<"nkobjects_v2">>).
-define(ES_ALIAS, <<"nkobjects">>).
-define(TYPES, [domain, user]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Callbacks: "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.


%% ===================================================================
%% Offered Callbacks
%% ===================================================================

%% @doc Called to get all installed object types
-spec domain_get_types([nkdomain:type()]) ->
    [nkdomain:type()].

domain_get_types(Acc) ->
    Acc ++ ?TYPES.


%% @doc Called to map types to modules
-spec domain_get_module(nkdomain:type()) ->
    module() | unknown.

domain_get_module(domain) -> nkdomain_obj_domain;
domain_get_module(group) -> nkdomain_obj_group;
domain_get_module(user) -> nkdomain_obj_user;
domain_get_module(token) -> nkdomain_obj_token;
domain_get_module(alias) -> nkdomain_obj_alias;
domain_get_module(service) -> nkdomain_obj_service;
domain_get_module(nodeset) -> nkdomain_obj_nodeset;
domain_get_module(_) -> unknown.


%% @doc
domain_store_mapping(domain) ->
    nkdomain_obj_domain:get_store_mapping();
domain_store_mapping(user) ->
    nkdomain_obj_user:get_store_mapping();
domain_store_mapping(Type) ->
    ?LLOG(warning, "Missing mapping for type ~s", [Type]),
    #{}.


%% @doc In-store base mapping
-spec domain_store_base_mapping() -> map().

domain_store_base_mapping() ->
    #{
        obj_id => #{type => keyword},
        domain => #{type => keyword},
        type => #{type => keyword},
        subtype => #{type => keyword},
        description => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        created_by => #{type => keyword},
        created_time => #{type => date},
        parent_id => #{type => keyword},
        enabled => #{type => boolean},
        expires_time => #{type => date},
        destroyed_time => #{type => date},
        destroyed_reason => #{type => keyword},
        aliases => #{type => keyword},
        icon_id => #{type => keyword}
    }.


%% @doc
-spec domain_load_obj(nkservice:id(), nkdomain:type(), nkdomain:obj_id()) ->
    {ok, Meta::map(), Obj::nkdomain:obj()} | not_found | {error, term()}.

domain_load_obj(SrvId, Type, ObjId) ->
    case nkelastic_api:get(SrvId, ?ES_INDEX, Type, ObjId) of
        {ok, Data, Vsn} ->
            {ok, #{es_vsn=>Vsn}, Data};
        {error, object_not_found} ->
            not_found;
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% Object Callbacks
%% ===================================================================

-type type() :: nkdomain:type().
-type session() :: nkdomain_obj:session().


%% @doc Called when a new session starts
-spec object_init(type(), session()) ->
    {ok, session()} | {stop, Reason::term()}.

object_init(Type, _Session) ->
    {stop, {unknown_type, Type}}.


%% @doc Called when the session stops
-spec object_terminate(type(), Reason::term(), session()) ->
    {ok, session()}.

object_terminate(_Type, _Reason, Session) ->
    {ok, Session}.


%% @private
-spec object_start(type(), session()) ->
    {ok, session()} | continue().

object_start(_Type, Session) ->
    {ok, Session}.


%% @private
-spec object_stop(type(), nkservice:error(), session()) ->
    {ok, session()} | continue().

object_stop(_Type, _Reason, Session) ->
    {ok, Session}.


%%  @doc Called when an event is sent
-spec object_event(type(), nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_event(_Type, _Event, Session) ->
    {ok, Session}.


%% @doc Called when an event is sent, for each registered process to the session
-spec object_reg_event(type(), nklib:link(), nkdomain_obj:event(), session()) ->
    {ok, session()} | continue().

object_reg_event(_Type, _Link, _Event, Session) ->
    {ok, Session}.


%% @doc Called when a registered process fails
-spec object_reg_down(type(), nklib:link(), term(), session()) ->
    {ok, session()} | {stop, Reason::term(), session()} | continue().

object_reg_down(_Type, _Link, _Reason, Session) ->
    {stop, registered_down, Session}.


%% @doc
-spec object_handle_call(type(), term(), {pid(), term()}, session()) ->
    {reply, term(), session()} | {noreply, session()} | continue().

object_handle_call(_Type, Msg, _From, Session) ->
    lager:error("Module nkdomain_obj received unexpected call: ~p", [Msg]),
    {noreply, Session}.


%% @doc
-spec object_handle_cast(type(), term(), session()) ->
    {noreply, session()} | continue().

object_handle_cast(_Type, Msg, Session) ->
    lager:error("Module nkdomain_obj received unexpected cast: ~p", [Msg]),
    {noreply, Session}.


%% @doc
-spec object_handle_info(type(), term(), session()) ->
    {noreply, session()} | continue().

object_handle_info(_Type, Msg, Session) ->
    lager:warning("Module nkdomain_obj received unexpected info: ~p", [Msg]),
    {noreply, Session}.






%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @private
plugin_deps() ->
    [nkelastic].


%% @private
service_init(_Service, State) ->
    gen_server:cast(self(), nkdomain_load_domain),
    {ok, State}.


%% @private
service_handle_cast(nkdomain_load_domain, State) ->
    {noreply, State};

service_handle_cast(_Msg, _State) ->
    continue.


%% ===================================================================
%% NkElastic callbacks
%% ===================================================================


%% @private
elastic_get_indices(Acc, Service) ->
    Acc2 = Acc#{
        ?ES_INDEX => #{
            number_of_replicas => 2
        }
    },
    {continue, [Acc2, Service]}.


%% @private
elastic_get_mappings(?ES_INDEX, Acc, #{id:=SrvId}=Service) ->
    Types = SrvId:domain_get_types([]),
    Acc2 = lists:foldl(
        fun(Type, FAcc) ->
            Base = SrvId:domain_store_base_mapping(),
            ObjMap = SrvId:domain_store_mapping(Type),
            FAcc#{Type => maps:merge(ObjMap, Base)}
        end,
        Acc,
        Types),
    {continue, [?ES_INDEX, Acc2, Service]};

elastic_get_mappings(_Index, _Acc, _Service) ->
    continue.


%% @private
elastic_get_aliases(?ES_INDEX, Acc, Service) ->
    {continue, [?ES_INDEX, Acc#{?ES_ALIAS => #{}}, Service]};

elastic_get_aliases(_Index, _Acc, _Service) ->
    continue.