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
-export([selected_type/4]).


-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args, Session),
    lager:Type("NkDOMAN Admin (~s) " ++ Txt, [Session#admin_session.session_id|Args])).


%% ===================================================================
%% Callbacks
%% ===================================================================

%% @doc
element_action([?ADMIN_OBJ_TYPE, Path, Type], selected, _Value, Updates, Session) ->
    selected_type(Type, Path, Updates, Session);

element_action([?ADMIN_OBJ_ID, _SrvId, ObjId, Type, Path], selected, _Value, Updates, Session) ->
    selected_obj(ObjId, Type, Path, Updates, Session);

element_action([?ADMIN_TYPE_VIEW, Type], updated, Value, Updates, Session) ->
    #{
        <<"obj_id">> := ObjId,
        <<"value">> := ObjValue
    } = Value,
    {ok, Mod} = nkdomain_admin_util:get_type_view_mod(Type, Session),
    case Mod:element_updated(ObjId, ObjValue, Session) of
        {ok, Update} ->
            case nkdomain:update(ObjId, Update) of
                {ok, _} ->
                    {ok, Updates, Session};
                {error, Error} ->
                    ?LLOG(warning, "Object update error: ~p", [Error], Session),
                    {ok, Updates, Session}
            end
    end;

element_action([?ADMIN_TYPE_VIEW|_Type], enable, #{<<"ids">>:=Ids}, Updates, Session) ->
    {Updates2, Session2} = type_view_enable(true, Ids, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_TYPE_VIEW|_Type], disable, #{<<"ids">>:=Ids}, Updates, Session) ->
    {Updates2, Session2} = type_view_enable(false, Ids, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_TYPE_VIEW|_Type], delete, #{<<"ids">>:=Ids}, Updates, Session) ->
    {Updates2, Session2} = type_view_delete(Ids, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_TYPE_VIEW|Type], new, _Value, Updates, Session) ->
    {Updates2, Session2} = type_view_new(Type, Updates, Session),
    {ok, Updates2, Session2};

element_action([<<"domain_detail_form">>, <<"user">>, <<"messages">>], selected, _Value, Updates, Session) ->
    Opts = #{table_id => <<"domain_detail_form__user__messages__table">>, header => <<"MESSAGES">>},
    {Table, _Session2} = nkchat_message_obj_type_view:subview(Opts, Session),
    Update = #{
        id => <<"domain_detail_form__user__messages__table_body">>,
        class => webix_ui,
        value => Table
    },
    {ok, [Update|Updates], Session};

element_action(_Elements, _Action, _Value, Updates, Session) ->
    {ok, Updates, Session}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
selected_type(Type, Path, Updates, Session) ->
    Class = nkdomain_util:class(Type),
    Path2 = nkdomain_util:append(Path, Class),
    case nkdomain_admin_util:get_type_info(Type, Session) of
        {true, #{type_view_mod:=Mod}} ->
            {Detail, Session2} = Mod:view(Session),
            {Updates3, Session3} = nkadmin_util:update_detail(Path2, Detail, Updates, Session2),
            {ok, Updates3, Session3};
        _ ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {Updates2, Session2} = nkadmin_util:update_detail(Path2, #{}, Updates, Session),
            {ok, Updates2, Session2}
    end.


%% @doc
selected_obj(ObjId, Type, Path, Updates, Session) ->
    case nkdomain_admin_util:get_obj_view_mod(Type, Session) of
        {ok, Mod} ->
            case nkdomain_lib:load(ObjId) of
                #obj_id_ext{}=ObjIdExt ->
                    {Detail, Session2} = Mod:view(ObjIdExt, Session),
                    {Updates3, Session3} = nkadmin_util:update_detail(Path, Detail, Updates, Session2),
                    {ok, Updates3, Session3};
                {error, Error} ->
                    ?LLOG(notice, "error loading object ~s: ~p", [Path, Error], Session),
                    {error, object_load_error, Session}
            end;
        not_found ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {Updates2, Session2} = nkadmin_util:update_detail(Path, #{}, Updates, Session),
            {ok, Updates2, Session2}
    end.



%% ===================================================================
%% Util
%% ===================================================================




%% ===================================================================
%% Type View
%% ===================================================================

%% @private
type_view_enable(_Enable, [], Updates, Session) ->
    {Updates, Session};

type_view_enable(Enable, [Id|Rest], Updates, Session) ->
    case nkdomain:enable(Id, Enable) of
        ok ->
            ?LLOG(info, "object enabled (~p): ~s", [Enable, Id], Session),
            ok;
        {error, Error} ->
            ?LLOG(warning, "could not enable ~s: ~p", [Id, Error], Session)
    end,
    type_view_enable(Enable, Rest, Updates, Session).


%% @private
type_view_delete([], Updates, Session) ->
    {Updates, Session};

type_view_delete([Id|Rest], Updates, Session) ->
    case nkdomain:delete(Id) of
        ok ->
            ok;
        {error, Error} ->
            ?LLOG(warning, "could not delete ~s: ~p", [Id, Error], Session)
    end,
    type_view_delete( Rest, Updates, Session).


%% @private
type_view_new(_Type, Updates, Session) ->
    {Updates, Session}.
