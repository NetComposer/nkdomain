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

-export([cmd/3]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

%% ===================================================================
%% API
%% ===================================================================

cmd(<<"check_name">>, #nkreq{data=#{name:=Name}}, State) ->
    {ok, #{name=>nkdomain_util:name(Name)}, State};

cmd(<<"find">>, #nkreq{data=Data, srv_id=SrvId}, State) ->
    case get_domain(Data, State) of
        {ok, Id} ->
            case nkdomain_domain_obj:find(SrvId, Id, Data) of
                {ok, Total, List, _Meta} ->
                    {ok, #{total=>Total, data=>List}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(<<"find_all">>, #nkreq{data=Data, srv_id=SrvId}, State) ->
    case get_domain(Data, State) of
        {ok, Id} ->
            case nkdomain_domain_obj:find_all(SrvId, Id, Data) of
                {ok, Total, List, _Meta} ->
                    {ok, #{total=>Total, data=>List}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(<<"find_types">>, #nkreq{data=Data, srv_id=SrvId}, State) ->
    case get_domain(Data, State) of
        {ok, Id} ->
            case nkdomain_domain_obj:find_types(SrvId, Id, Data) of
                {ok, Total, List} ->
                    {ok, #{total=>Total, data=>maps:from_list(List)}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(<<"find_all_types">>, #nkreq{data=Data, srv_id=SrvId}, State) ->
    case get_domain(Data, State) of
        {ok, Id} ->
            case nkdomain_domain_obj:find_all_types(SrvId, Id, Data) of
                {ok, Total, List} ->
                    {ok, #{total=>Total, data=>maps:from_list(List)}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(<<"find_childs">>, #nkreq{data=Data, srv_id=SrvId}, State) ->
    case get_domain(Data, State) of
        {ok, Id} ->
            Search = nkdomain_domain_obj:find_childs(SrvId, Id, Data),
            nkdomain_api_util:search(Search, State);
        Error ->
            Error
    end;

cmd(<<"find_all_childs">>, #nkreq{data=Data, srv_id=SrvId}, State) ->
    case get_domain(Data, State) of
        {ok, Id} ->
            Search = nkdomain_domain_obj:find_all_childs(SrvId, Id, Data),
            nkdomain_api_util:search(Search, State);
        Error ->
            Error
    end;

cmd(Cmd, Req, State) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_DOMAIN, Req, State).


%% ===================================================================
%% Internal
%% ===================================================================

get_domain(Data, State) ->
    nkdomain_api_util:get_id(?DOMAIN_DOMAIN, Data,  State).
