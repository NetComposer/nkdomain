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
cmd('', get_types, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    case nkdomain_domain_obj:get_types(SrvId, Id) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>maps:from_list(List)}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', get_all_types, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    case nkdomain_domain_obj:get_all_types(SrvId, Id) of
        {ok, Total, List} ->
            {ok, #{total=>Total, data=>maps:from_list(List)}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', get_childs, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    Search = nkdomain_domain_obj:get_childs(SrvId, Id, Data),
    nkdomain_util:api_search(Search, State);

cmd('', get_all_childs, Data, #{srv_id:=SrvId}=State) ->
    Id = get_domain(Data, SrvId),
    Search = nkdomain_domain_obj:get_all_childs(SrvId, Id, Data),
    nkdomain_util:api_search(Search, State);

cmd('', Cmd, Data, State) ->
    nkdomain_util:api_cmd_common(?DOMAIN_DOMAIN, Cmd, Data, State);

cmd(_Sub, _Cmd, _Data, State) ->
    {error, not_implemented, State}.


%% ===================================================================
%% Internal
%% ===================================================================

get_domain(#{id:=Id}, _SrvId) -> Id;
get_domain(_, SrvId) -> nkdomain_util:get_service_domain(SrvId).
