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
-export([call_type/3]).
-include("nkdomain.hrl").



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
event(Event, #obj_session{link_events=Links}=Session) ->
    {ok, #obj_session{}=Session2} = do_event(Links, Event, Session),
    Session2.


%% @private
do_event([], Event, #obj_session{srv_id=SrvId}=Session) ->
    {ok, #obj_session{}} = SrvId:object_event(Event, Session);

do_event([Link|Rest], Event, #obj_session{srv_id=SrvId}=Session) ->
    {ok, Session2} = SrvId:object_reg_event(Link, Event, Session),
    do_event(Rest, Event,  Session2).


%% @doc
status(Status, #obj_session{status=Status}=Session) ->
    Session;

status(Status, Session) ->
    Session2 = Session#obj_session{status=Status},
    event({status, Status}, Session2).


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
get_name(#obj_session{type=Type, obj_id=ObjId, path=Path, obj=Obj}) ->
    {ok, _, ObjName} = nkdomain_util:get_parts(Type, Path),
    #{
        obj_id => ObjId,
        obj_name => ObjName,
        name => maps:get(name, Obj, ObjName),
        description => maps:get(description, Obj, <<>>),
        icon_id => maps:get(icon_id, Obj, <<>>)
    }.


%% @private
call_type(Fun, Args, Type) ->
    case nkdomain_types:get_module(Type) of
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


