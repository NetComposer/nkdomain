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
-module(nkdomain_domain_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

%% ===================================================================
%% API
%% ===================================================================

cmd(<<"check_name">>, #nkreq{data=#{name:=Name}}) ->
    {ok, #{name=>nkdomain_util:name(Name)}};

cmd(<<"find">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_domain_obj:search(Id, Data) of
                {ok, Total, List, _Meta} ->
                    {ok, #{total=>Total, data=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

cmd(<<"find_all">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_domain_obj:search_all(Id, Data) of
                {ok, Total, List, _Meta} ->
                    {ok, #{total=>Total, data=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

cmd(<<"find_types">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_domain_obj:search_type(Id, Data) of
                {ok, Total, List, _Meta} ->
                    {ok, #{total=>Total, data=>maps:from_list(List)}};
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

cmd(<<"find_all_types">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_domain_obj:search_all_types(Id, Data) of
                {ok, Total, List, _Meta} ->
                    {ok, #{total=>Total, data=>maps:from_list(List)}};
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

cmd(<<"find_childs">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            Search = nkdomain_domain_obj:search_childs(Id, Data),
            nkdomain_api_util:search(Search);
        Error ->
            Error
    end;

cmd(<<"find_all_childs">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            Search = nkdomain_domain_obj:search_all_childs(Id, Data),
            nkdomain_api_util:search(Search);
        Error ->
            Error
    end;

cmd(<<"unload_childs">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            nkdomain_domain_obj:unload_childs(Id);
        Error ->
            Error
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:api(Cmd, ?DOMAIN_DOMAIN, Req).


%% ===================================================================
%% Internal
%% ===================================================================

get_domain(Data, Req) ->
    nkdomain_api_util:get_id(?DOMAIN_DOMAIN, Data, Req).
