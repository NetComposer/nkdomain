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
            case nkdomain_db:find(Id) of
                #obj_id_ext{obj_id=ObjId} ->
                    Filters1 = maps:get(filters, Data, #{}),
                    Filters2 = Filters1#{domain_id=>ObjId},
                    Data2 = maps:remove(id, Data#{filters=>Filters2}),
                    {ok, EsOpts} = nkdomain_store_es_util:get_opts(),
                    case nkdomain_store_es_search:search(Data2, EsOpts) of
                        {ok, Total, List, _Aggs, _Meta} ->
                            {ok, #{total=>Total, data=>List}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

cmd(<<"find_all">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_db:find(Id) of
                #obj_id_ext{path=Path} ->
                    Filters1 = maps:get(filters, Data, #{}),
                    Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
                    Data2 = maps:remove(id, Data#{filters=>Filters2}),
                    {ok, EsOpts} = nkdomain_store_es_util:get_opts(),
                    case nkdomain_store_es_search:search(Data2, EsOpts) of
                        {ok, Total, List, _Aggs, _Meta} ->
                            {ok, #{total=>Total, data=>List}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

%%cmd(<<"find_types">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            case nkdomain_db:aggs({type, Id, #{}}) of
%%                {ok, Total, List} ->
%%                    {ok, #{total=>Total, data=>maps:from_list(List)}};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        Error ->
%%            Error
%%    end;
%%
%%cmd(<<"find_all_types">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            case nkdomain_db:aggs({type, Id, #{deep=>true}}) of
%%                {ok, Total, List} ->
%%                    {ok, #{total=>Total, data=>maps:from_list(List)}};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        Error ->
%%            Error
%%    end;
%%
%%cmd(<<"find_childs">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            Search = nkdomain_db:search({paths, Id, #{}}),
%%            nkdomain_api_util:search(Search);
%%        Error ->
%%            Error
%%    end;
%%
%%cmd(<<"find_all_childs">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            Search = nkdomain_db:search({paths, Id, #{deep=>true}}),
%%            nkdomain_api_util:search(Search);
%%        Error ->
%%            Error
%%    end;

cmd(<<"unload_childs">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            nkdomain_domain_obj:unload_childs(Id);
        Error ->
            Error
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:cmd(Cmd, ?DOMAIN_DOMAIN, Req).


%% ===================================================================
%% Internal
%% ===================================================================

get_domain(Data, Req) ->
    nkdomain_api_util:get_id(?DOMAIN_DOMAIN, Data, Req).
