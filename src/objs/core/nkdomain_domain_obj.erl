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

%% @doc Domain Object

-module(nkdomain_domain_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([search/3, search_all/3, search_type/3, search_all_types/3, search_childs/3, search_all_childs/3]).
-export([find_path/2, find_path/3, unload_childs/2]).
-export([object_info/0, object_parse/3, object_es_mapping/0,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_sync_op/3, object_async_op/2, object_enabled/2, object_link_down/2,
         object_handle_info/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Obj Domain "++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
search(SrvId, Id, Spec) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{obj_id=ObjId} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{domain_id=>ObjId},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            nkdomain:search(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_all(SrvId, Id, Spec) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{path=Path} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            nkdomain:search(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_type(SrvId, Id, Spec) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{obj_id=ObjId} ->
            SrvId:object_db_search_types(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_all_types(SrvId, Id, Spec) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{path=Path} ->
            SrvId:object_db_search_all_types(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_childs(SrvId, Id, Spec) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{obj_id=ObjId} ->
            SrvId:object_db_search_childs(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_all_childs(SrvId, Id, Spec) ->
    case nkdomain_lib:find(SrvId, Id) of
        #obj_id_ext{path=Path} ->
            SrvId:object_db_search_all_childs(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds a child object with this path
%% Must be send to a domain that is part of the path (or root to be sure)
-spec find_path(nkservice:id(), binary()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} | {error, term()}.

find_path(Srv, Path) ->
    find_path(Srv, <<"root">>, Path).


%% @doc Finds a child object with this path
%% Must be send to a domain that is part of the path (or root to be sure)
-spec find_path(nkservice:id(), nkdomain:obj_id(), binary()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), pid()} | {error, term()}.

find_path(Srv, Id, Path) ->
    case nkdomain_util:get_parts(Path) of
        {ok, Base, Type, ObjName} ->
            nkdomain_obj:sync_op(Srv, Id, {?MODULE, find_path, Base, Type, ObjName});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
unload_childs(Srv, Id) ->
    nkdomain_obj:sync_op(Srv, Id, {?MODULE, unload_childs}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(session, {
    obj_ids = #{} :: #{nkdomain:obj_id() => #obj_id_ext{}},
    obj_types = #{} :: #{nkdomain:type() => #{nkdomain:obj_name() => nkdomain:obj_id()}},
    master_mon :: reference(),
    service_pid :: pid()
}).


%% @private
object_info() ->
    #{
        type => ?DOMAIN_DOMAIN,
        permanent => true
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        defaults => #{enabled => false}
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        vsn => binary,
        defaults => map,
        '__defaults' => #{vsn => <<"1">>}
    }.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_domain_obj_syntax:api(Cmd, Syntax).



%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_domain_obj_api:cmd(Cmd, Req).


%% @private
object_init(#?STATE{id=ObjIdExt}=State) ->
    Ref = nkdomain_proc:monitor(),
    case ObjIdExt of
        #obj_id_ext{obj_id = <<"root">>} ->
            ok = nkdomain_proc:register(ObjIdExt);
        _ ->
            ok
    end,
    State2 = State#?STATE{session=#session{master_mon=Ref}},
    case register_service(State2) of
        {ok, State3} ->
            {ok, State3};
        error ->
            {error, service_not_available}
    end.


%% @private
object_sync_op({nkdomain_reg_obj, ObjIdExt}, _From, #?STATE{id=Id} = State) ->
    #obj_id_ext{path=DomainPath} = Id,
    #obj_id_ext{obj_id=ObjId, type=Type, path=Path, pid=Pid, obj_name=ObjName} = ObjIdExt,
    case nkdomain_util:get_parts(Type, Path) of
        {ok, DomainPath, ObjName} ->
            case nkdomain_proc:register(ObjIdExt) of
                ok ->
                    ?DEBUG("registering obj ~s", [Path], State),
                    State2 = do_rm_obj(ObjId, State),
                    State3 = do_add_obj(ObjIdExt, State2),
                    State4 = do_event({obj_loaded, Type, ObjId, ObjName, Pid}, State3),
                    #?STATE{is_enabled=Enabled} = State,
                    {reply, {ok, Enabled, self()}, State4};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        _ ->
            {reply, {error, object_path_invalid}, State}
    end;

object_sync_op({?MODULE, find_path, <<>>, ?DOMAIN_DOMAIN, <<>>}, _From,
                #?STATE{id=#obj_id_ext{obj_id = <<"root">>, type = ?DOMAIN_DOMAIN}}=State) ->
    {reply, {ok, ?DOMAIN_DOMAIN, <<"root">>, self()}, State};

object_sync_op({?MODULE, find_path, Base, Type, ObjName}, From, State) ->
    case find_obj(Base, Type, ObjName, State) of
        {ok, ObjId, Pid} ->
            {reply, {ok, Type, ObjId, Pid}, State};
        {subdomain, Pid} ->
            nkdomain_obj:async_op(any, Pid, {?MODULE, find_path, Base, Type, ObjName, From}),
            {noreply, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, unload_childs}, _From, State) ->
    #?STATE{id=#obj_id_ext{path=Path}, session=#session{obj_ids=Objs}} = State,
    ?LLOG(notice, "unloading childs at ~s", [Path], State),
    lists:foreach(
        fun({_ObjId, #obj_id_ext{type=Type, path=ObjPath, pid=Pid}}) ->
            case Type of
                ?DOMAIN_DOMAIN ->
                    ?LLOG(notice, "unloading childs of ~s", [ObjPath], State),
                    unload_childs(any, Pid);
               _ ->
                   ok
            end,
            ?LLOG(notice, "unloading ~s", [Path], State),
            nkdomain_obj:async_op(any, Pid, {unload, normal})
        end,
        maps:to_list(Objs)),
    {reply, ok, State};

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op({?MODULE, find_path, Base, Type, ObjName, From}, State) ->
    case find_obj(Base, Type, ObjName, State) of
        {ok, ObjId, Pid} ->
            gen_server:reply(From, {ok, Type, ObjId, Pid});
        {subdomain, Pid} ->
            nkdomain_obj:async_op(any, Pid, {?MODULE, find_path, Base, Type, ObjName, From});
        {error, Error} ->
            gen_server:reply(From, {error, Error})
    end,
    {noreply, State};

object_async_op(_Op,  _State) ->
    continue.


%% @private
object_enabled(Enabled, State) ->
    send_objs({nkdomain_domain_enabled, Enabled}, State),
    {ok, State}.


%% @private
object_link_down({usage, {?MODULE, obj, ObjId, _Pid}}, State) ->
    State2 = do_rm_obj(ObjId, State),
    {ok, State2};

object_link_down(_Link, State) ->
    {ok, State}.


%% @private
object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, #?STATE{session=#session{service_pid=Pid}}=State) ->
    ?LLOG(warning, "service stopped", [], State),
    send_objs(nkdomain_service_stopped, State),
    nkdomain_obj:do_stop(service_down, State);

object_handle_info({'DOWN', Ref, process, _Pid, _Reason}, #?STATE{session=#session{master_mon=Ref}}=State) ->
    ?LLOG(warning, "master stopped", [], State),
    Ref2 = nkdomain_proc:monitor(),
    #?STATE{session=Session} = State,
    State2 = State#?STATE{session=Session#session{master_mon=Ref2}},
    #session{obj_ids=ObjIds} = Session,
    State3 = do_reg_all(maps:values(ObjIds), State2),
    {noreply, State3};

object_handle_info(_Info, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_add_obj(ObjIdExt, #?STATE{session=Session}=State) ->
    #obj_id_ext{obj_id=ObjId, obj_name=ObjName, type=Type, pid=Pid} = ObjIdExt,
    #session{obj_ids=ObjIds, obj_types=ObjTypes} = Session,
    ObjIds2 = ObjIds#{ObjId => ObjIdExt},
    ObjNames1 = maps:get(Type, ObjTypes, #{}),
    ObjNames2 = ObjNames1#{ObjName => ObjId},
    ObjTypes2 = ObjTypes#{Type => ObjNames2},
    Session2 = Session#session{obj_ids=ObjIds2, obj_types=ObjTypes2},
    State2 = State#?STATE{session=Session2},
    nkdomain_obj:links_add(usage, {?MODULE, obj, ObjId, Pid}, State2).


%% @private
do_rm_obj(ObjId, #?STATE{session=Session}=State) ->
    #session{obj_ids=ObjIds, obj_types=ObjTypes} = Session,
    case maps:find(ObjId, ObjIds) of
        {ok, #obj_id_ext{type=Type, obj_name=ObjName, pid=Pid}} ->
            ObjIds2 = maps:remove(ObjId, ObjIds),
            ObjNames1 = maps:get(Type, ObjTypes, #{}),
            ObjNames2 = maps:remove(ObjName, ObjNames1),
            ObjTypes2 = case map_size(ObjNames2) of
                0 ->
                    maps:remove(Type, ObjTypes);
                _ ->
                    ObjTypes#{Type => ObjNames2}
            end,
            Session2 = Session#session{obj_ids=ObjIds2, obj_types=ObjTypes2},
            State2 = State#?STATE{session=Session2},
            State3 = do_event({obj_unloaded, Type, ObjId}, State2),
            nkdomain_obj:links_remove(usage, {?MODULE, obj, ObjId, Pid}, State3);
        error ->
            State
    end.


%% @private
register_service(#?STATE{srv_id=SrvId, session=Session}=State) ->
    case whereis(SrvId) of
        Pid when is_pid(Pid) ->
            monitor(process, Pid),
            Session2 = Session#session{service_pid=Pid},
            {ok, State#?STATE{session=Session2}};
        _ ->
            error
    end.


%% @doc
find_obj(Base, Type, ObjName, #?STATE{id=Id, session=Session}) ->
    #obj_id_ext{path=DomainPath} = Id,
    #session{obj_ids=ObjIds, obj_types=ObjTypes} = Session,
    case Base of
        DomainPath ->
            ObjNames = maps:get(Type, ObjTypes, #{}),
            case maps:find(ObjName, ObjNames) of
                {ok, ObjId} ->
                    {ok, #obj_id_ext{type=Type, obj_name=ObjName, pid=Pid}} = maps:find(ObjId, ObjIds),
                    {ok, ObjId, Pid};
                error ->
                    %lager:error("NKLOG not found ~p", [{Type, ObjName}]),
                    {error, object_not_found}
            end;
        _ ->
            %lager:error("NKLOG Base: ~s (~s)", [Base, DomainPath]),
            Size = byte_size(DomainPath),
            case Base of
                <<DomainPath:Size/binary, Rest/binary>> ->
                    Dom = get_first_domain(Rest),
                    %lager:warning("NKLOG R1: ~p, D: ~p", [Rest, Dom]),
                    Domains = maps:get(?DOMAIN_DOMAIN, ObjTypes, #{}),
                    case maps:find(Dom, Domains) of
                        {ok, SubDomId} ->
                            {ok, #obj_id_ext{type=?DOMAIN_DOMAIN, obj_id=Dom, pid=Pid}} = maps:find(SubDomId, ObjIds),
                            ?LLOG(notice, "relaying query to ~s", [<<DomainPath/binary, $/, Dom/binary>>]),
                            {subdomain, Pid};
                        error ->
                            {error, object_not_found}
                    end;
                _ ->
                    {error, object_path_invalid}
            end
    end.


%% @private
send_objs(Msg, #?STATE{session=#session{obj_ids=ObjIds}}=State) ->
    lists:foreach(
        fun(#obj_id_ext{path=Path, pid=Pid}) ->
            ?LLOG(notice, "sending ~p to obj ~s", [Msg, Path], State),
            gen_server:cast(Pid, Msg)
        end,
        maps:values(ObjIds)).


%% @private
do_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
get_first_domain(Path) ->
    Path2 = case Path of
        <<$/, Rest/binary>> -> Rest;
        _ -> Path
    end,
    case binary:split(Path2, <<"/">>) of
        [Dom] -> Dom;
        [Dom, _] -> Dom
    end.


%% @private
do_reg_all([], State) ->
    State;

do_reg_all([#obj_id_ext{obj_id=ObjId, pid=Pid}=ObjIdExt|Rest], State) ->
    case nkdomain_proc:register(ObjIdExt) of
        ok ->
            do_reg_all(Rest, State);
        {error, {already_registered, WinnerPid}} ->
            nkdomain_obj:conflict_detected(Pid, WinnerPid),
            do_reg_all(Rest, do_rm_obj(ObjId, State))
    end.



