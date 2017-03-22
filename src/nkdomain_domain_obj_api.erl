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

%% @doc User Object API
-module(nkdomain_domain_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include("nkdomain.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd('', get_types, #{id:=Id}, #{srv_id:=SrvId}=State) ->
    case nkdomain_domain_obj:get_types(SrvId, Id) of
        {ok, N, List} ->
           {ok, #{total=>N, data=>maps:from_list(List)}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', get_all_types, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_domain_obj:get_all_types(SrvId, Id) of
        {ok, N, List} ->
            {ok, #{total=>N, data=>maps:from_list(List)}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', get_childs, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_domain_obj:get_childs(SrvId, Id, Data) of
        {ok, Reply} ->
            {ok, Reply, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', get_all_childs, #{id:=Id}=Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_domain_obj:get_all_childs(SrvId, Id, Data) of
        {ok, Reply} ->
            {ok, Reply, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', delete, Data, State) ->
    nkdomain_util:api_delete(Data, State);

cmd('', update, Data, State) ->
    nkdomain_util:api_update(Data, State);

cmd('', Cmd, Data, State) ->
    nkdomain_util:api_common(?DOMAIN_DOMAIN, Cmd, Data, State);

cmd(_Sub, _Cmd, Data, State) ->
    {error, not_implemented, State}.
