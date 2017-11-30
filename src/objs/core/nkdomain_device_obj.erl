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

%% @doc Device Object

-module(nkdomain_device_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-export([object_info/0, object_admin_info/0, object_api_syntax/2, object_api_cmd/2, object_send_event/2]).
-export([object_es_mapping/0, object_parse/2, object_db_get_filter/2]).
-export([object_execute/5, object_schema/1, object_query/3, object_mutation/3]).
-export([object_init/1, object_sync_op/3, object_link_down/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Device "++Txt, Args)).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(session, {
    session_id :: nkdomain:obj_id(),
    user_id :: nkdomain:user_id()
}).



%% @private
object_info() ->
    #{
        type => ?DOMAIN_DEVICE,
        schema_type => 'Device'
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 100
    }.


%% @doc
object_schema(Type) ->
    nkdomain_device_obj_schema:object_schema(Type).


%% @doc
object_execute(Field, ObjIdExt, #{?DOMAIN_DEVICE:=Device}, Args, _Ctx) ->
    nkdomain_device_obj_schema:object_execute(Field, ObjIdExt, Device, Args).


%% @doc
object_query(QueryName, Params, Ctx) ->
    nkdomain_device_obj_schema:object_query(QueryName, Params, Ctx).


%% @doc
object_mutation(MutationName, Params, Ctx) ->
    nkdomain_device_obj_schema:object_mutation(MutationName, Params, Ctx).



%% @private
object_parse(_Type, _Obj) ->
    #{
        device_uuid => binary,
        sso_device_ids => {list, binary}
    }.



%% @private
object_es_mapping() ->
    #{
        device_uuid => #{type => keyword},
        sso_device_ids => #{type => keyword}
    }.


%% @private
object_db_get_filter(nkelastic, {find_device_uuid, Domain, UUID}) ->
    case nkdomain_store_es_util:get_path(Domain) of
        {ok, DomainPath} ->
            Filters = [
                {path, subdir, DomainPath},
                {[?DOMAIN_DEVICE, ".device_uuid"], eq, nklib_util:to_binary(UUID)}
            ],
            {ok, {Filters, #{}}};
        {error, Error} ->
            {error, Error}
    end;

object_db_get_filter(nkelastic, QueryType) ->
    {error, {unknown_query, QueryType}};

object_db_get_filter(Backend, _) ->
    {error, {unknown_backend, Backend}}.



% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_device_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_send_event(Event, State) ->
    nkdomain_device_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_device_obj_cmd:cmd(Cmd, Req).


%% @private
object_init(State) ->
    Session = #session{},
    {ok, State#obj_state{session=Session}}.


%% @private
object_sync_op({?MODULE, Op}, From, State) ->
    sync_op(Op, From, State);

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_link_down({usage, {session, SessId, _Pid}}, State) ->
    #obj_state{session=Session} = State,
    case Session of
        #session{session_id=SessId} ->
            Session2 = Session#session{session_id=undefined, user_id=undefined},
            State2 = State#obj_state{session=Session2},
            ?LLOG(notice, "detached sessions ~s", [SessId], State),
            {ok, State2};
        _ ->
            {ok, State}
    end;

object_link_down(_Link, _State) ->
    continue.

%% ===================================================================
%% Internal
%% ===================================================================


sync_op({attach_session, UserId, SessId, Pid}, _From, State) ->
    #obj_state{session=Session} = State,
    case Session of
        #session{session_id=undefined} ->
            % We monitor the session and are alive while the session is
            State2 = nkdomain_obj:links_add(usage, {session, SessId, Pid}, State),
            Session2 = Session#session{session_id=SessId, user_id=UserId},
            State3 = State2#obj_state{session=Session2},
            ?LLOG(notice, "attached user ~s (~s)", [UserId, SessId], State3),
            {reply, ok, State3};
        _ ->
            {reply, {error, existing_session}, State}
    end;

sync_op(get_registered_user, _From, #obj_state{session=Session}=State) ->
    case Session of
        #session{session_id=undefined} ->
            {reply, {error, no_session}, State};
        #session{user_id=UserId} ->
            {reply, {ok, UserId}, State}
    end;

sync_op(find_sso, _From, #obj_state{obj=Obj}=State) ->
    #{?DOMAIN_DEVICE:=DeviceObj} = Obj,
    SsoDevices = maps:get(sso_device_ids, DeviceObj, []),
    case do_find_sso(SsoDevices) of
        {ok, User} ->
            {reply, {ok, User}, State};
        error ->
            {reply, {error, no_session}, State}
    end.


%% @private
do_find_sso([]) ->
    error;

do_find_sso([DeviceId|Rest]) ->
    case nkdomain_device:get_registered_user(DeviceId) of
        {ok, User} ->
            {ok, User};
        _ ->
            do_find_sso(Rest)
    end.