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
cmd('', create, Data, State) ->
    #{obj_name:=Name, description:=Desc} = Data,
    #{srv_id:=SrvId, domain:=Domain} = State,
    case nkdomain_domain_obj:create(SrvId, Domain, Name, Desc) of
        {ok, ObjId, Path, _Pid} ->
            {ok, #{obj_id=>ObjId, path=>Path}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', find_types, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    case nkdomain_domain_obj:find_types(SrvId, Id, Data) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>maps:from_list(List)}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', find_all_types, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    case nkdomain_domain_obj:find_all_types(SrvId, Id, Data) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>maps:from_list(List)}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', find_childs, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    Search = nkdomain_domain_obj:find_childs(SrvId, Id, Data),
    nkdomain_api_util:search(Search, State);

cmd('', find_all_childs, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    Search = nkdomain_domain_obj:find_all_childs(SrvId, Id, Data),
    nkdomain_api_util:search(Search, State);

cmd(Sub, Cmd, Data, State) ->
    nkdomain_obj_api:api(Sub, Cmd, Data, ?DOMAIN_DOMAIN, State).


%% ===================================================================
%% Internal
%% ===================================================================

get_domain(#{id:=Id}, _SrvId) -> Id;
get_domain(_, SrvId) -> nkdomain_util:get_service_domain(SrvId).
