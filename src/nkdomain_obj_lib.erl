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


%% @doc Basic Obj utilities


-module(nkdomain_obj_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([find/2, load/3, create/3]).
-export([make_obj/3, make_and_create/4]).
-export([unload/4, sync_op/5, async_op/5, link_to_obj/4, unlink_to_obj/4]).
-export([get_node/1, register/3, register/4, link_to_parent/4]).
-export([find_loaded/1, call/2, call/3, cast/2, info/2]).
-export([send_event/3, send_event/4, send_event/5]).
-export_type([make_and_create_reply/0]).


-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-define(DEF_SYNC_CALL, 5000).


%% ===================================================================
%% Types
%% ===================================================================


%%-type make_opts() ::
%%    #{
%%        obj_id => binary(),
%%        obj_name => binary(),
%%        name => binary(),
%%        parent => binary(),
%%        created_by => binary(),
%%        expires_time => nklib_util:m_timestamp(),
%%        referred_id => nkdomain:obj_id(),
%%        active => boolean(),
%%        description => binary(),
%%        aliases => [binary()],
%%        type_obj => map()
%%    }.


-type make_and_create_reply() ::
    #{
        obj_id => nkdomain:obj_id(),
        path => nkdomain:path(),
        unknown_fields => [binary()]
    }.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Adds type, obj_id, parent_id, path, created_time
-spec make_obj(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain:obj()} | {error, term()}.

make_obj(Srv, ObjName, Obj) ->
    #{parent_id:=Parent, type:=Type} = Obj,
    case find(Srv, Parent) of
        #obj_id_ext{obj_id=ParentId, path=ParentPath} ->
            Type2 = to_bin(Type),
            UUID = nklib_util:luid(),
            ObjId = case Obj of
                #{obj_id:=ObjId0} ->
                    to_bin(ObjId0);
                _ when Type2 == ?DOMAIN_TOKEN ->
                    UUID;
                _ ->
                    <<Type2/binary, $-, UUID/binary>>
            end,
            case do_make_name(UUID, ObjName) of
                {ok, Name1} ->
                    Name2 = case Type2 of
                        ?DOMAIN_DOMAIN ->
                            Name1;
                        _ ->
                            <<Type2/binary, "s/", Name1/binary>>
                    end,
                    BasePath = case ParentPath of
                        <<"/">> -> <<>>;
                        _ -> ParentPath
                    end,
                    Obj2 = Obj#{
                        obj_id => ObjId,
                        type => Type2,
                        parent_id => ParentId,
                        path => <<BasePath/binary, $/, Name2/binary>>,
                        created_time => nkdomain_util:timestamp()
                    },
                    {ok, Obj2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, {could_not_load_parent, Parent}};
        {error, Error} ->
            {error, Error}
    end.





%% @private
do_make_name(UUID, <<>>) ->
    {ok, binary:part(UUID, 0, 7)};

do_make_name(_UUID, Name) ->
    {ok, nkdomain_util:name(Name)}.


%%%% @private
%%do_make_obj([], _Type, Acc) ->
%%    Acc;
%%
%%do_make_obj([{Key, Val}|Rest], Type, Acc) ->
%%    case Key of
%%        _ when Key==created_by; Key==referred_id; Key==description; Key==aliases; Key==active;
%%               Key==name; Key==subtype; Key==expires_time ->
%%            do_make_obj(Rest, Type, [{Key, Val}|Acc]);
%%        type_obj ->
%%            do_make_obj(Rest, Type, [{Type, Val}|Acc]);
%%        _ ->
%%            do_make_obj(Rest, Type, Acc)
%%    end.



%% @doc
-spec make_and_create(nkservice:id(),nkdomain:name(), nkdomain:obj(), nkdomain:load_opts()) ->
    {ok, make_and_create_reply(), pid()} | {error, term()}.

make_and_create(Srv, ObjName, Obj, Opts) ->
    case make_obj(Srv, ObjName, Obj) of
        {ok, Obj2} ->
            %% lager:warning("Obj: ~p", [Obj]),
            CreateMeta1 = maps:with([meta, usage_link, event_link], Opts),
            CreateMeta2 = case maps:is_key(obj_id, Obj) orelse ObjName /= <<>> of
                true ->
                    CreateMeta1;
                false ->
                    CreateMeta1#{skip_path_check=>true}
            end,
            case create(Srv, Obj2, CreateMeta2) of
                {#obj_id_ext{obj_id=ObjId, path=Path, type=Type, pid=Pid}, []} ->
                    {ok, _, ObjName2} = nkdomain_util:get_parts(Type, Path),
                    {ok, #{obj_id=>ObjId, path=>Path, obj_name=>ObjName2}, Pid};
                {#obj_id_ext{obj_id=ObjId, path=Path, type=Type, pid=Pid}, UnknownFields} ->
                    {ok, _, ObjName2} = nkdomain_util:get_parts(Type, Path),
                    {ok, #{obj_id=>ObjId, path=>Path, obj_name=>ObjName2, unknown_fields=>UnknownFields}, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates a new object
-spec create(nkservice:id(), map(), nkdomain:load_opts()) ->
    {#obj_id_ext{}, UnknownFields::[binary()]} | {error, term()}.

create(Srv, #{obj_id:=ObjId}=Obj, Meta) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:object_parse(SrvId, load, Obj) of
                {ok, #{obj_id:=ObjId, path:=Path, type:=Type}=Obj2, UnknownFields} ->
                    % We know type is valid here
                    Ext = #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path},
                    do_create(Ext, Obj2, Meta, UnknownFields);
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @private
do_create(#obj_id_ext{srv_id=SrvId}=Ext, #{parent_id:=ParentId}=Obj, Meta, UnknownFields) ->
    case load(SrvId, ParentId, #{}) of
        #obj_id_ext{pid=ParentPid} ->
            case nkdomain_obj:create_child(ParentPid, Obj, Meta) of
                {ok, ObjPid} ->
                    {Ext#obj_id_ext{pid=ObjPid}, UnknownFields};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, {could_not_load_parent, ParentId}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Finds and object from UUID or Path, in memory and disk
-spec find(nkservice:id(), nkdomain:obj_id()|nkdomain:path()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

find(Srv, IdOrPath) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case find_loaded(IdOrPath) of
                #obj_id_ext{}=ObjIdExt ->
                    ObjIdExt#obj_id_ext{srv_id=SrvId};
                not_found ->
                    case SrvId:object_store_find_obj(SrvId, IdOrPath) of
                        {ok, Type, ObjId, Path} ->
                            #obj_id_ext{srv_id=SrvId, type=Type, obj_id=ObjId, path=Path};
                        {error, Error} ->
                            {error, Error}
                    end
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @doc Finds an objects's pid or loads it from storage
-spec load(nkservice:id(), nkdomain:obj_id()|nkdomain:path(), nkdomain:load_opts()) ->
    #obj_id_ext{} | {error, object_not_found|term()}.

load(Srv, IdOrPath, Meta) ->
    case find(Srv, IdOrPath) of
        #obj_id_ext{pid=Pid}=ObjIdExt when is_pid(Pid) ->
            case Meta of
                #{register:=Link} ->
                    nkdomain_obj:register(Pid, Link);
                _ ->
                    ok
            end,
            case maps:find(usage_link, Meta) of
                {ok, {Id1, Tag1}} ->
                    ok = nkdomain_obj:link(Pid, usage, Id1, Tag1);
                error ->
                    ok
            end,
            case maps:find(event_link, Meta) of
                {ok, {Id2, Tag2}} ->
                    ok = nkdomain_obj:link(Pid, event, Id2, Tag2);
                error ->
                    ok
            end,
            ObjIdExt;
        #obj_id_ext{obj_id = <<"root">>, path = <<"/">>}=ObjIdExt ->
            case nkdomain_obj:start(ObjIdExt, Meta) of
                {ok, Pid} ->
                    ObjIdExt#obj_id_ext{pid=Pid};
                {error, Error} ->
                    {error, Error}
            end;
        #obj_id_ext{type=Type, path=Path}=ObjIdExt ->
            {ok, Base, _Name} = nkdomain_util:get_parts(Type, Path),
            do_load(ObjIdExt, Base, Meta);
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_load(#obj_id_ext{srv_id=SrvId}=ObjIdExt, BasePath,  Meta) ->
    LoadPath = maps:get(load_path, Meta, []),
    case lists:member(BasePath, LoadPath) of
        true ->
            {error, object_parent_conflict};
        false ->
            case load(SrvId, BasePath, #{load_path=>[BasePath|LoadPath]}) of
                #obj_id_ext{pid=ParentPid} when is_pid(ParentPid) ->
                    case nkdomain_obj:load_child(ParentPid, ObjIdExt, Meta) of
                        {ok, ObjPid} ->
                            ObjIdExt#obj_id_ext{pid=ObjPid};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, object_not_found} ->
                    {error, {could_not_load_parent, BasePath}};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
unload(Srv, Id, Reason, NotFound) ->
    case find(Srv, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:unload(Pid, Reason);
        #obj_id_ext{} ->
            {error, object_not_started};
        {error, object_not_found} ->
            {error, NotFound};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
sync_op(Srv, Id, Type, Msg, NotFound) ->
    case load(Srv, Id, #{}) of
        #obj_id_ext{type=Type, pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:sync_op(Pid, Msg);
        #obj_id_ext{} ->
            {error, invalid_object};
        {error, object_not_found} ->
            {error, NotFound};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
async_op(Srv, Id, Type, Msg, NotFound) ->
    case load(Srv, Id, #{}) of
        #obj_id_ext{type=Type, pid=Pid} when is_pid(Pid) ->
            nkdomain_obj:async_op(Pid, Msg);
        #obj_id_ext{} ->
            {error, invalid_object};
        {error, object_not_found} ->
            {error, NotFound};
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% Private
%% ===================================================================

%% @private
get_node(ObjId) ->
    case nkdist:get_vnode(nkdomain, ObjId, #{}) of
        {ok, Node, _Idx} ->
            {ok, Node};
        {error, Error} ->
            {error, Error}
    end.


%%%% @private
%%reserve(ObjId) ->
%%    reserve(ObjId, 5).
%%
%%
%%%% @private
%%reserve(_ObjId, 0) ->
%%    {error, could_not_reserve};
%%
%%reserve(ObjId, Tries) ->
%%    case nkdist_reg:reserve(nkdomain, ObjId) of
%%        ok ->
%%            ok;
%%        {error, _} ->
%%            lager:info("NkDOMAIN: Could not reserve ~s, retrying", [ObjId]),
%%            timer:sleep(1000),
%%            reserve(ObjId, Tries-1)
%%    end.
%%
%%
%%%% @private
%%unreserve(ObjId) ->
%%    lager:error("UNReserved ~p (~p)", [ObjId, self()]),
%%    nkdist_reg:unreserve(nkdomain, ObjId).


%% @private
register(Type, ObjId, Path) ->
    do_register(Type, ObjId, Path, #{sync=>true}).


%% @private
register(Type, ObjId, Path, Pid) ->
    do_register(Type, ObjId, Path, #{sync=>true, replace_pid=>Pid}).


%% @private
do_register(Type, ObjId, Path, Opts) ->
    case nkdist_reg:register(proc, nkdomain, ObjId, Opts#{meta=>{Type, path, Path}}) of
        ok ->
            nkdist_reg:register(reg, nkdomain, Path, Opts#{meta=>{Type, obj_id, ObjId}});
        {error, Error} ->
            {error, Error}
    end.


%% @private Links a child to its parent
link_to_parent(Parent, Type, Name, ChildId) ->
    % Parent will receive {received_link, {nkdomain_child, ChildId}} (does nothing)
    % If parent dies, child receives {sent_link_down, {nkdomain_child, ChildId}}
    % Id child dies, parent receives {received_link_down, {nkdomain_child, ChildId}}
    ok = nkdist_reg:link(nkdomain, Parent, {nkdomain_child, Type, Name, ChildId}).


%% @private Links a child to its parent
%% Destination will receive {received_link, {Type, Tag}}
%% On failure, orig receive {sent_link_down, {Type, Tag}},
%% destination receives {received_link_down, {Type, Tag}}

link_to_obj(Type, OrigPid, DestPid, Tag) when is_pid(OrigPid) ->
    nkdist_reg:link_pid(OrigPid, DestPid, {Type, Tag});

link_to_obj(Type, OrigId, DestPid, Tag) ->
    case find_loaded(OrigId) of
        #obj_id_ext{pid=OrigPid} ->
            link_to_obj(Type, OrigPid, DestPid, Tag);
        not_found ->
            {error, destination_not_found}
    end.


%% @private Removes a previous link
%% Destination will receive {removed_link, {Type, Tag}}

unlink_to_obj(Type, OrigPid, DestPid, Tag) when is_pid(OrigPid) ->
    nkdist_reg:unlink_pid(OrigPid, DestPid, {Type, Tag});

unlink_to_obj(Type, OrigId, DestPid, Tag) ->
    case find_loaded(OrigId) of
        #obj_id_ext{pid=OrigPid} ->
            unlink_to_obj(Type, OrigPid, DestPid, Tag);
        not_found ->
            {error, destination_not_found}
    end.



%% @private
find_loaded(IdOrPath) when is_binary(IdOrPath) ->
    case nkdist_reg:find(nkdomain, IdOrPath) of
        {ok, {Type, path, Path}, Pid} ->
            #obj_id_ext{type=Type, obj_id=IdOrPath, path=Path, pid=Pid};
        {ok, {Type, obj_id, ObjId}, Pid} ->
            #obj_id_ext{type=Type, obj_id=ObjId, path=IdOrPath, pid=Pid};
        _ ->
            not_found
    end;

find_loaded(ObjId) ->
    find_loaded(nklib_util:to_binary(ObjId)).


%% @private
call(Id, Msg) ->
    call(Id, Msg, ?DEF_SYNC_CALL).


%% @private
call(Pid, Msg, Timeout) when is_pid(Pid) ->
    nkservice_util:call(Pid, Msg, Timeout);

call(Id, Msg, Timeout) ->
    call(Id, Msg, Timeout, 5).


%% @private
call(_Id, _Msg, _Timeout, 0) ->
    {error, process_not_found};

call(Id, Msg, Timeout, Tries) ->
    case find_loaded(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            case nkservice_util:call(Pid, Msg, Timeout) of
                {error, process_not_found} ->
                    timer:sleep(250),
                    call(Id, Msg, Timeout, Tries-1);
                Other ->
                    Other
            end;
        not_found ->
            {error, object_not_found}
    end.


%% @private
cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);

cast(Id, Msg) ->
    case find_loaded(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            cast(Pid, Msg);
        not_found ->
            {error, object_not_found}
    end.


%% @private
info(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;

info(Id, Msg) ->
    case find_loaded(Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            info(Pid, Msg);
        not_found ->
            {error, object_not_found}
    end.


%% @private
send_event(EvType, Body, #?NKOBJ{obj_id=ObjId, path=Path}=Session) ->
    send_event(EvType, ObjId, Path, Body, Session).


%% @private
send_event(EvType, ObjId, Body, #?NKOBJ{path=Path}=Session) ->
    send_event(EvType, ObjId, Path, Body, Session).


%% @private
send_event(EvType, ObjId, ObjPath, Body, #?NKOBJ{srv_id=SrvId, type=Type}=Session) ->
    Event = #nkevent{
        srv_id = SrvId,
        class = ?DOMAIN_EVENT_CLASS,
        subclass = Type,
        type = nklib_util:to_binary(EvType),
        obj_id = ObjId,
        domain = ObjPath,
        body = Body
    },
    lager:info("Domain EVENT sent to listeners: ~p", [Event]),
    send_direct_event(Event, Session),
    nkevent:send(Event),
    {ok, Session}.


%% @private
send_direct_event(#nkevent{type=Type, body=Body}=Event, #?NKOBJ{meta=Meta}) ->
    case Meta of
        #{session_events:=Events, session_id:=ConnId} ->
            case lists:member(Type, Events) of
                true ->
                    Event2 = case Meta of
                        #{session_events_body:=Body2} ->
                            Event#nkevent{body=maps:merge(Body, Body2)};
                        _ ->
                            Event
                    end,
                    nkapi_server:event(ConnId, Event2);
                false ->
                    ok
            end;
        _ ->
            ok
    end.


%% @private
to_bin(T) -> nklib_util:to_binary(T).