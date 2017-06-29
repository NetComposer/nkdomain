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
-module(nkdomain_obj_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([event/2, status/2, search_syntax/1, get_name/1]).
-export([send_event/3, send_event/4, send_event/5]).
-export([call_type/3]).
-export([link_to_api_server/4, unlink_from_api_server/2]).
-export([get_obj_session/1, set_obj_session/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Sends an event inside an object process to all registered
%% Calls SrvId:object_event() and SrvId:object_reg_event(), that
%% normally call send_event/N here

event(Event, #?STATE{srv_id=SrvId, event_links=Links}=State) ->
    Fun = fun(Link, Data, Acc) ->
        {ok, Acc2} = SrvId:object_reg_event(Link, Data, Event, Acc),
        Acc2
    end,
    State2 = nklib_links:fold_values(Fun, State, Links),
    {ok, State3} = SrvId:object_event(Event, State2),
    State3.


%% @doc Sends events inside an object process directly to the event server
%% If the obj has session_events, they are sent directly to the session also
send_event(EvType, Body, #?STATE{id=#obj_id_ext{obj_id=ObjId, path=Path}}=State) ->
    send_event(EvType, ObjId, Path, Body, State).


%% @private
send_event(EvType, ObjId, Body, #?STATE{id=#obj_id_ext{path=Path}}=State) ->
    send_event(EvType, ObjId, Path, Body, State).


%% @private
send_event(EvType, ObjId, ObjPath, Body, #?STATE{id=#obj_id_ext{srv_id=SrvId, type=Type}}=State) ->
    Event = #nkevent{
        srv_id = SrvId,
        class = ?DOMAIN_EVENT_CLASS,
        subclass = Type,
        type = nklib_util:to_binary(EvType),
        obj_id = ObjId,
        domain = ObjPath,
        body = Body
    },
    ?DEBUG("event sent to listeners: ~p", [Event], State),
    send_session_event(Event, State),
    nkevent:send(Event),
    {ok, State}.


%% @private
send_session_event(#nkevent{type=Type}=Event, State) ->
    #?STATE{srv_id=SrvId, session_events=Events, session_id=Id} = State,
    case lists:member(Type, Events) of
        true ->
            SrvId:object_session_event(Id, Event, State);
        false ->
            ok
    end.


%% @doc
status(Status, #?STATE{status=Status}=State) ->
    State;

status(Status, State) ->
    State2 = State#?STATE{status=Status},
    event({status, Status}, State2).


%% @doc
search_syntax(Base) ->
    Base#{
        from => {integer, 0, none},
        size => {integer, 0, none},
        sort => {list, binary},
        fields => {list, binary},
        filters => map,
        simple_query => binary,
        simple_query_opts =>
            #{
                fields => {list, binary},
                default_operator => {atom, ['OR', 'AND']}
            }
    }.


%% @doc
get_name(#?STATE{id=#obj_id_ext{type=Type, obj_id=ObjId, path=Path}, obj=Obj}) ->
    {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
    #{
        obj_id => ObjId,
        obj_name => ObjName,
        path => Path,
        name => maps:get(name, Obj, ObjName),
        description => maps:get(description, Obj, <<>>),
        icon_id => maps:get(icon_id, Obj, <<>>)
    }.


%% @private
call_type(Fun, Args, Type) ->
    case nkdomain_all_types:get_module(Type) of
        undefined ->
            ok;
        Module ->
            case erlang:function_exported(Module, Fun, length(Args)) of
                true ->
                    case apply(Module, Fun, Args) of
                        continue ->
                            ok;
                        Other ->
                            Other
                    end;
                false ->
                    ok
            end
    end.


%% @doc
link_to_api_server(Module, ApiMod, ApiPid, State) ->
    % Stop the API Server if we fail abruptly
    ok = ApiMod:register(ApiPid, {nkdomain_stop, Module, self()}),
    % Monitor the API server, reduce usage count if it fails
    nkdomain_obj:links_add(usage, {nkdomain_api_server, ApiPid}, State).


%% @doc
unlink_from_api_server(Module, State) ->
    nkdomain_obj:links_iter(
        usage,
        fun
            ({nkdomain_api_server, ApiPid}, _Acc) ->
                nkapi_server:unregister(ApiPid, {nkdomain_stop, Module, self()});
            (_, _Acc) ->
                ok
        end,
        ok,
        State),
    State.


%% @doc
get_obj_session(#?STATE{session=Session}) ->
    Session.


%% @doc
set_obj_session(Session, State) ->
    State#?STATE{session=Session}.




