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

-export([search/2, get_id/3, get_id/4, add_id/3]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
search({ok, Total, List}, Req) ->
    Data = #{
        total => Total,
        data =>
            lists:map(
                fun({Type, ObjId, Path}) -> #{type=>Type, obj_id=>ObjId, path=>Path} end,
                List)
    },
    {ok, Data, Req};

search({error, Error}, Req) ->
    {error, Error, Req}.


%% @doc
get_id(Type, Data, State) ->
    get_id(Type, id, Data, State).


%% @doc
get_id(Type, Field, Data, State) ->
    case maps:find(Field, Data) of
        {ok, Id} ->
            {ok, Id};
        error ->
            ObjIds = maps:get(nkdomain_obj_ids, State, #{}),
            case maps:find(Type, ObjIds) of
                {ok, Id} ->
                    {ok, Id};
                error ->
                    % lager:error("OI: ~s ~p", [Type, ObjIds]),
                    {error, {missing_field, Field}, State}
            end
    end.


%% @doc Adds 'logged in' information to the state
add_id(Type, Id, State) ->
    ObjIds1 = maps:get(nkdomain_obj_ids, State, #{}),
    ObjIds2 = ObjIds1#{Type => Id},
    State#{nkdomain_obj_ids => ObjIds2}.



