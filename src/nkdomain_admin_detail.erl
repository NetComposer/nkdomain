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
-export([get_data/3]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_data(<<"domain_detail_user_table">>, Spec, State) ->
    Start = maps:get(start, Spec, 0),
    Size = case maps:find('end', Spec) of
        {ok, End} when End > Start -> End-Start;
        _ -> 100
    end,
    Filter = maps:get(filter, Spec, #{}),
    Sort = case maps:get(sort, Spec, #{}) of
        #{
            id := SortId,
            dir := SortDir
        } ->
            {SortId, SortDir};
        _ ->
            undefined
    end,
    case nkdomain_user_obj_ui:table_data(Start, Size, Filter, Sort, State) of
        {ok, Total, Data} ->
            Reply = #{
                total_count => Total,
                pos => Start,
                data => Data
            },
            {ok, Reply, State};
        {error, Error} ->
            ?LLOG(warning, "error getting query: ~p", [Error]),
            {ok, #{total_count=>0, pos=>0, data=>[]}, State}
    end;

get_data(_ElementId, _Data, _State) ->
    continue.

