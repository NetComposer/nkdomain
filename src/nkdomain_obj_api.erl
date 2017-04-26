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

-module(nkdomain_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api/5]).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
api('', Cmd, Data, Type, State) ->
    case Cmd of
        get ->
            cmd_get(Type, Data, State);
        enable ->
            cmd_enable(Type, Data, State);
        delete ->
            cmd_delete(Type, Data, State);
        wait_for_save ->
            cmd_wait_for_save(Type, Data, State);
        update ->
            cmd_update(Type, Data, State);
        _ ->
            {error, not_implemented, State}
    end;

api(_Sub, _Cmd, _Data, _Type, State) ->
    {error, not_implemented, State}.



%% ===================================================================
%% Private
%% ===================================================================


%% @doc
cmd_get(Type, Data, #{srv_id:=SrvId}=State) ->
    case get_id(Type, Data, State) of
        {ok, Id} ->
            case nkdomain_obj_lib:load(SrvId, Id, #{}) of
                #obj_id_ext{pid=Pid} ->
                    case nkdomain_obj:get_session(Pid) of
                        {ok, #obj_session{obj=Obj, is_enabled=Enabled}} ->
                            {ok, Obj#{'_is_enabled'=>Enabled}, State};
                        {error, Error} ->
                            {error, Error, State}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @doc
cmd_delete(Type, Data, #{srv_id:=SrvId}=State) ->
    case get_id(Type, Data, State) of
        {ok, Id} ->
            case cmd_delete_childs(Data, SrvId, Id) of
                {ok, Num} ->
                    case nkdomain:delete(SrvId, Id) of
                        ok ->
                            {ok, #{deleted=>Num+1}, State};
                        {error, Error} ->
                            {error, Error, State}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @private
cmd_delete_childs(#{delete_childs:=true}, SrvId, Id) ->
    nkdomain_store:delete_all_childs(SrvId, Id);

cmd_delete_childs(_Data, _SrvId, _Id) ->
    {ok, 0}.


%% @doc
cmd_update(Type, Data, #{srv_id:=SrvId}=State) ->
    case get_id(Type, Data, State) of
        {ok, Id} ->
            case nkdomain:update(SrvId, Id, Data) of
                ok ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @doc
cmd_enable(Type, #{enable:=Enable}=Data, #{srv_id:=SrvId}=State) ->
    case get_id(Type, Data, State) of
        {ok, Id} ->
            case nkdomain:enable(SrvId, Id, Enable) of
                ok ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @doc
cmd_wait_for_save(Type, Data, #{srv_id:=SrvId}=State) ->
    Time = maps:get(time, Data, 5000),
    case get_id(Type, Data, State) of
        {ok, Id} ->
            case nkdomain_obj_lib:find(SrvId, Id) of
                #obj_id_ext{pid=Pid} when is_pid(Pid) ->
                    case nkdomain_obj:wait_for_save(Pid, Time) of
                        ok ->
                            {ok, #{}, State};
                        {error, Error} ->
                            {error, Error, State}
                    end;
                #obj_session{} ->
                    {error, object_not_loaded, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.

%% @doc
get_id(Type, Data, State) ->
    nkdomain_api_util:get_id(Type, id, Data, State).
