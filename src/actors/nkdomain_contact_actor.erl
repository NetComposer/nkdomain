%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain Contact Actor





-module(nkdomain_contact_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkservice_actor).

-export([config/0, parse/3]).
-export([parse_post_check/1]).
-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Plugin: "++Txt, Args)).

-include("nkdomain.hrl").
%%-include_lib("nkservice/include/nkservice_actor.hrl").
%%-include_lib("nkpacket/include/nkpacket.hrl").



%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Actor behaviour
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CONTACTS,
        group => ?GROUP_CORE,
        versions => [?GROUP_CORE_V1A1],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => [ct],
        filter_fields => [
            <<"spec.name">>,
            <<"spec.surname">>,
            <<"spec.normalizedName">>,
            <<"spec.normalizedSurname">>,
            <<"spec.birthTime">>,
            <<"spec.gender">>,
            <<"spec.timezone">>,
            <<"spec.phone.phone">>
        ],
        sort_fields => [
            <<"spec.normalizedName">>,
            <<"spec.normalizedSurname">>,
            <<"spec.gender">>,
            <<"spec.birthTime">>,
            <<"spec.timezone">>
        ],
        field_type => #{
            <<"spec.timezone">> => integer,
            <<"spec.phone.phone">> => array
        }
    }.


%% @doc
parse(_SrvId, _Actor, _ApiReq) ->
    Syntax = #{
        <<"name">> => binary,
        <<"surname">> => binary,
        <<"birthTime">> => date_3339,
        <<"gender">> => {binary, [<<"M">>, <<"F">>]},
        <<"timezone">> => integer,
        <<"url">> => {list,
            #{
                <<"type">> => binary,
                <<"url">> => binary,
                <<"meta">> => map,
                '__mandatory' => [<<"url">>]
            }},
        <<"phone">> => {list,
            #{
                <<"type">> => binary,
                <<"phone">> => binary,
                <<"meta">> => map,
                '__mandatory' => [<<"phone">>]
            }},
        <<"email">> => {list,
            #{
                <<"type">> => binary,
                <<"email">> => binary,
                <<"meta">> => map,
                '__mandatory' => [<<"email">>]
            }},
        <<"im">> => {list,
            #{
                <<"type">> => binary,
                <<"im">> => binary,
                <<"meta">> => map,
                '__mandatory' => [<<"im">>]
            }},
        <<"address">> => {list,
            #{
                <<"type">> => binary,
                <<"street">> => binary,
                <<"code">> => binary,
                <<"city">> => binary,
                <<"province">> => binary,
                <<"state">> => binary,
                <<"country">> => binary,
                <<"meta">> => map
            }},
        <<"pubkey">> => {list,
            #{
                <<"type">> => binary,
                <<"key">> => binary,
                <<"meta">> => map,
                '__mandatory' => [<<"key">>]
            }},
        <<"profile">> => {list,
            #{
                <<"type">> => binary,
                <<"startTime">> => date_3339,
                <<"stopTime">> => date_3339,
                <<"data">> => map,
                <<"meta">> => map,
                '__mandatory' => [<<"data">>]
            }},
        % It is always calculated
        <<"normalizedName">> => binary,
        <<"normalizedSurname">> => binary,
        '__post_check' => fun ?MODULE:parse_post_check/1
    },
    {syntax, #{<<"spec">>=>Syntax}}.



%% ===================================================================
%% Internal
%% ===================================================================


parse_post_check(List) ->
    Map1 = maps:from_list(List),
    Name = maps:get(<<"name">>, Map1, <<>>),
    SurName = maps:get(<<"surname">>, Map1, <<>>),
    Map2 = Map1#{
        <<"normalizedName">> => nklib_parse:normalize(Name),
        <<"normalizedSurname">> => nklib_parse:normalize(SurName)
    },
    {ok, maps:to_list(Map2)}.


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).