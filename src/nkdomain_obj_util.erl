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
-export([link_server_api/3, unlink_server_api/2]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Sends an event inside an object process to all registered
%% Calls SrvId:object_event() and SrvId:object_reg_event(), that
%% normally call send_event/N here

event(Event, #?STATE{event_links=Links}=State) ->
    {ok, #?STATE{}=State2} = do_event(Links, Event, State),
    State2.


%% @private
do_event([], Event, #?STATE{srv_id=SrvId}=State) ->
    {ok, #?STATE{}} = SrvId:object_event(Event, State);

do_event([Link|Rest], Event, #?STATE{srv_id=SrvId}=State) ->
    {ok, State2} = SrvId:object_reg_event(Link, Event, State),
    do_event(Rest, Event,  State2).


%% @doc Sends events inside an object process directly to the event server
%% If the obj has session_events, they are sent directly to the session also
send_event(EvType, Body, #?STATE{obj_id=ObjId, path=Path}=State) ->
    send_event(EvType, ObjId, Path, Body, State).


%% @private
send_event(EvType, ObjId, Body, #?STATE{path=Path}=State) ->
    send_event(EvType, ObjId, Path, Body, State).


%% @private
send_event(EvType, ObjId, ObjPath, Body, #?STATE{srv_id=SrvId, type=Type}=State) ->
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
    send_direct_event(Event, State),
    nkevent:send(Event),
    {ok, State}.


%% @private
send_direct_event(#nkevent{type=Type, body=Body}=Event, #?STATE{meta=Meta}) ->
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
get_name(#?STATE{type=Type, obj_id=ObjId, path=Path, obj=Obj}) ->
    {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
    #{
        obj_id => ObjId,
        obj_name => ObjName,
        path => Path,
        name => maps:get(name, Obj, ObjName),
        description => maps:get(description, Obj, <<>>),
        icon_id => maps:get(icon_id, Obj, <<>>),
        icon_content_type => maps:get(icon_content_type, Obj, <<>>)
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
link_server_api(Module, ApiPid, State) ->
    % Stop the API Server if we fail abruptly
    ok = nkapi_server:register(ApiPid, {nkdomain_stop, Module, self()}),
    % Monitor the API server, reduce usage count if it fails
    nkdomain_obj:links_add(usage, {nkdomain_api_server, ApiPid}, State).


%% @doc
unlink_server_api(Module, State) ->
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






