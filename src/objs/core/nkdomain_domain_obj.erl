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
-export([find_path/2, find_path/3]).
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
search(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{domain_id=>ObjId},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            nkdomain:search(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_all(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            nkdomain:search(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_type(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_db_search_types(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_all_types(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_db_search_all_types(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_childs(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_db_search_childs(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
search_all_childs(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_db_search_all_childs(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds a child object with this path
%% Must be send to a domain that is part of the path (or root to be sure)
find_path(Srv, Path) ->
    find_path(Srv, <<"root">>, Path).


%% @doc Finds a child object with this path
%% Must be send to a domain that is part of the path (or root to be sure)
find_path(Srv, Id, Path) ->
    case nkdomain_util:get_parts(Path) of
        {ok, Base, Type, ObjName} ->
            nkdomain_obj:sync_op(Srv, Id, {?MODULE, find_path, Base, Type, ObjName});
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(session, {
    obj_ids = #{} :: #{nkdomain:obj_id() => {nkdomain:type(), nkdomain:obj_name(), pid()}},
    obj_names = #{} :: #{{nkdomain:type(), nkdomain:obj_name()} => nkdomain:obj_id()},
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
object_init(State) ->
    State2 = State#?STATE{session=#session{}},
    case register_service(State2) of
        {ok, State3} ->
            {ok, State3};
        error ->
            {error, service_not_available}
    end.


object_sync_op({nkdomain_reg_obj, ObjIdExt}, _From, #?STATE{id=Id}=State) ->
    #obj_id_ext{path=DomainPath} = Id,
    #obj_id_ext{obj_id=ObjId, type=Type, path=Path, pid=Pid} = ObjIdExt,
    case nkdomain_util:get_parts(Type, Path) of
        {ok, DomainPath, ObjName} ->
        ?DEBUG("registering obj ~s", [Path], State),
        State2 = do_rm_obj(ObjId, State),
        case do_add_obj(ObjId, Type, ObjName, Pid, State2) of
            {ok, State3} ->
                State4 = do_event({obj_loaded, Type, ObjId, ObjName, Pid}, State3),
                #?STATE{is_enabled=Enabled} = State,
                {reply, {ok, Enabled, self()}, State4};
            {error, Error} ->
                {error, Error}
        end;
    _ ->
        {reply, {error, object_path_invalid}, State}
    end;

object_sync_op({?MODULE, find_path, <<>>, ?DOMAIN_DOMAIN, <<>>}, _From,
                #?STATE{id=#obj_id_ext{obj_id = <<"root">>, type = ?DOMAIN_DOMAIN}}=State) ->
    {reply, {ok, <<"root">>, self()}, State};

object_sync_op({?MODULE, find_path, Base, Type, ObjName}, From, State) ->
    case find_obj(Base, Type, ObjName, State) of
        {ok, ObjId, Pid} ->
            {reply, {ok, ObjId, Pid}, State};
        {subdomain, Pid} ->
            nkdomain_obj:async_op(any, Pid, {?MODULE, find_path, Base, Type, ObjName, From}),
            {noreply, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @doc
object_async_op({?MODULE, find_path, Base, Type, ObjName, From}, State) ->
    case find_obj(Base, Type, ObjName, State) of
        {ok, ObjId, Pid} ->
            gen_server:reply(From, {ok, ObjId, Pid});
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

object_handle_info(_Info, _State) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_add_obj(ObjId, Type, ObjName, Pid, #?STATE{session=Session}=State) ->
    #session{obj_ids=ObjIds, obj_names=ObjNames} = Session,
    case maps:is_key({Type, ObjName}, ObjNames) of
        false ->
            ObjIds2 = ObjIds#{ObjId => {Type, ObjName, Pid}},
            ObjNames2 = ObjNames#{{Type, ObjName} => ObjId},
            Session2 = Session#session{obj_ids=ObjIds2, obj_names=ObjNames2},
            State2 = State#?STATE{session=Session2},
            State3 = nkdomain_obj:links_add(usage, {?MODULE, obj, ObjId, Pid}, State2),
            {ok, State3};
        true ->
            {error, duplicated_object}
    end.


%% @private
do_rm_obj(ObjId, #?STATE{session=Session}=State) ->
    #session{obj_ids=ObjIds, obj_names=ObjNames} = Session,
    case maps:find(ObjId, ObjIds) of
        {ok, {Type, ObjName, Pid}} ->
            ObjIds2 = maps:remove(ObjId, ObjIds),
            ObjNames2 = maps:remove({Type, ObjName}, ObjNames),
            Session2 = Session#session{obj_ids=ObjIds2, obj_names=ObjNames2},
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
    #session{obj_ids=ObjIds, obj_names=ObjNames} = Session,
    case Base of
        DomainPath ->
            case maps:find({Type, ObjName}, ObjNames) of
                {ok, ObjId} ->
                    {ok, {Type, ObjName, Pid}} = maps:find(ObjId, ObjIds),
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
                    case maps:find({?DOMAIN_DOMAIN, Dom}, ObjNames) of
                        {ok, SubDomId} ->
                            {ok, {?DOMAIN_DOMAIN, SubDomName, Pid}} = maps:find(SubDomId, ObjIds),
                            ?LLOG(notice, "relaying query to ~s", [<<DomainPath/binary, $/, Dom/binary>>]),
                            {subdomain, Pid};
                        error ->
                            {error, object_not_found2}
                    end;
                _ ->
                    {error, object_path_invalid2}
            end
    end.


%% @private
send_objs(Msg, #?STATE{session=#session{obj_ids=ObjIds}}=State) ->
    lists:foreach(
        fun({_ObjId, {Type, ObjName, Pid}}) ->
            ?LLOG(notice, "sending ~p to obj ~s/~s", [Msg, Type, ObjName], State),
            gen_server:cast(Pid, Msg)
        end,
        maps:to_list(ObjIds)).


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
