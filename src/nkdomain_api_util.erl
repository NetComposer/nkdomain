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

-module(nkdomain_api_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd_common/4, cmd_get/3, cmd_create/3, cmd_delete/3, cmd_update/3]).
-export([search/2, getid/3, add_id/3]).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").



%% ===================================================================
%% Public
%% ===================================================================



%%%% @doc Add mandatory fields to syntax
%%-spec add_mandatory([atom()], module(), map()) ->
%%    map().
%%
%%add_mandatory(Fields, Module, Base) ->
%%    Fields2 = [list_to_binary([to_bin(Module), $., to_bin(F)]) || F<- Fields],
%%    Mandatory1 = maps:get('__mandatory', Base, []),
%%    Mandatory2 = Fields2 ++ Mandatory1,
%%    Base#{'__mandatory' => Mandatory2}.


%% @doc
cmd_common(Type, Cmd, Data, State) ->
    case Cmd of
        get ->
            cmd_get(Type, Data, State);
        create ->
            cmd_create(Type, Data, State);
        delete ->
            cmd_delete(Type, Data, State);
        update ->
            cmd_update(Type, Data, State);
        _ ->
            {error, not_implemented, State}
    end.


%% @doc
cmd_get(Type, Data, #{srv_id:=SrvId}=State) ->
    case getid(Type, Data, State) of
        {ok, Id} ->
            case nkdomain_obj_lib:load(SrvId, Id, #{}) of
                #obj_id_ext{pid=Pid} ->
                    case nkdomain_obj:get_session(Pid) of
                        {ok, #obj_session{obj=Obj}} ->
                            {ok, Obj, State};
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
cmd_create(Type, Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_obj_lib:create(SrvId, Data#{type=>Type}, #{}) of
        #obj_id_ext{obj_id=ObjId, path=Path} ->
            {ok, #{obj_id=>ObjId, path=>Path}, State};
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc
cmd_delete(Type, Data, #{srv_id:=SrvId}=State) ->
    case getid(Type, Data, State) of
        {ok, Id} ->
            Reason = maps:get(reason, Data, api_delete),
            case nkdomain_obj_lib:load(SrvId, Id, #{}) of
                #obj_id_ext{obj_id=ObjId} ->
                    case nkdomain_obj:delete(ObjId, Reason) of
                        ok ->
                            {ok, #{}, State};
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
cmd_update(Type, Data, #{srv_id:=SrvId}=State) ->
    case getid(Type, Data, State) of
        {ok, Id} ->
            case nkdomain_obj_lib:load(SrvId, Id, #{}) of
                #obj_id_ext{obj_id=ObjId} ->
                    case nkdomain_obj:update(ObjId, Data) of
                        ok ->
                            {ok, #{}, State};
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
search({ok, Total, List}, State) ->
    Data = #{
        total => Total,
        data =>
            lists:map(
                fun({Type, ObjId, Path}) -> #{type=>Type, obj_id=>ObjId, path=>Path} end,
                List)
    },
    {ok, Data, State};

search({error, Error}, State) ->
    {error, Error, State}.


%% @doc
getid(_Type, #{id:=Id}, _State) ->
    {ok, Id};

getid(Type, _Data, #{nkdomain_obj_ids:=ObjIds}=State) ->
    case maps:find(Type, ObjIds) of
        {ok, Id} ->
            {ok, Id};
        error ->
            lager:error("OI: ~s ~p", [Type, ObjIds]),
            {error, missing_id, State}
    end;

getid(_Type, _Data, State) ->
    {error, missing_id, State}.


%% @doc Adds 'logged in' information to the state
add_id(Type, Id, State) ->
    ObjIds1 = maps:get(nkdomain_obj_ids, State, #{}),
    ObjIds2 = ObjIds1#{Type => Id},
    ?ADD_TO_API_SESSION(nkdomain_obj_ids, ObjIds2, State).

