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
-module(nkdomain_admin_detail).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([element_action/5]).


-include("nkdomain.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(TYPE_VIEW, <<"domain_detail_type_view">>).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
element_action([?TYPE_VIEW, Type], updated, Value, Updates, Session) ->
    #{
        <<"obj_id">> := ObjId,
        <<"value">> := ObjValue
    } = Value,
    {ok, Mod} = get_view_mod(Type, Session),
    case Mod:element_updated(ObjId, ObjValue, Session) of
        {ok, Update} ->
            #admin_session{srv_id=SrvId} = Session,
            case nkdomain:update(SrvId, ObjId, Update) of
                {ok, _} ->
                    {ok, Updates, Session};
                {error, Error} ->
                    ?LLOG(warning, "Object update error: ~p", [Error]),
                    {ok, Updates, Session}
            end
    end;

element_action(_Elements, _Action, _Value, Updates, Session) ->
    {ok, Updates, Session}.


%% ===================================================================
%% Util
%% ===================================================================


get_view_mod(Type, #admin_session{srv_id=SrvId}) ->
    case SrvId:object_admin_info(Type) of
        #{type_view_mod:=Mod} ->
            {ok, Mod};
        _ ->
            not_found
    end.
