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

%% @doc Elasticsearch Event plugin
-module(nkdomain_event_es_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_opts/0, get_index_opts/0]).
-export([db_init/2, db_search/2, db_save/2]).

-export([print_template/0]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Event ES "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_opts() ->
    case get_index_opts() of
        {ok, _IndexOpts, EsOpts} ->
            {ok, EsOpts};
        _ ->
            not_found
    end.


%% @doc
get_index_opts() ->
    case ?CALL_NKROOT(config_nkdomain_event_es, []) of
        {nkdomain_event_es, IndexOpts, EsOpts} ->
            IndexOpts2 = IndexOpts#{
                'mapper.dynamic' => false
            },
            {ok, IndexOpts2, EsOpts};
        _ ->
            not_found
    end.


print_template() ->
    {ok, #{index:=Index}=E} = get_opts(),
    nkelastic:get_template(Index, E).



%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc ES base base mapping
base_mappings() ->
    #{
        vsn => #{type => keyword},
        node => #{type => keyword},
        class => #{type => keyword},
        subclass => #{type => keyword},
        type => #{type => keyword},
        obj_id => #{type => keyword},
        domain => #{type => keyword},
        time => #{type => date}
    }.


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc
db_init(IndexOpts, #{index:=Index}=EsOpts) ->
    IndexOpts2 = IndexOpts#{
        mappings => #{
            events => #{
                properties => base_mappings(),
                dynamic => true
            }
        }
    },
    case nkelastic:create_template(Index, IndexOpts2, EsOpts) of
        {ok, _} ->
            ok;
        {error, Error} ->
            {error, {create_index, Error}}
    end.


%% @doc
db_search(Spec, EsOpts) ->
    #{index:=Temp} = EsOpts,
    Index = <<Temp/binary, $*>>,
    case nkelastic_search:query(Spec) of
        {ok, Query} ->
            case nkelastic:search(Query, EsOpts#{index:=Index}) of
                {ok, N, List, _Aggs, Meta} ->
                    Data = lists:map(
                        fun(#{<<"_id">>:=ObjId}=D) ->
                            Source = maps:get(<<"_source">>, D, #{}),
                            Source#{<<"obj_id">>=>ObjId}
                        end,
                        List),
                    {ok, N, Data, Meta};
                {error, {search_error, _}} ->
                    {ok, 0, [], #{}, #{error=>search_error}};
                {error, Error} ->
                    ?LLOG(notice, "Error calling search (~p): ~p", [Query, Error]),
                    {ok, 0, [], #{}, #{}}
            end;
        {error, Error} ->
            ?LLOG(warning, "query error ~p: ~p", [Spec, Error]),
            {error, Error}
    end.



%% @doc
db_save(#nkevent{} = Event, EsOpts) ->
    #nkevent{
        class = Class,
        subclass = SubClass,
        type = Type,
        domain = Domain,
        obj_id = ObjId,
        body = Body
    } = Event,
    Now = nkdomain_util:timestamp(),
    BodyField = list_to_binary([Class, $/, SubClass, $/, Type]),
    EventId = nklib_util:luid(),
    Obj = #{
        vsn => 1,
        node => to_bin(node()),
        class => Class,
        subclass => SubClass,
        type => Type,
        obj_id => ObjId,
        domain => Domain,
        time => Now,
        BodyField => Body
    },
    {{Y, M, D}, _} = nklib_util:timestamp_to_gmt(Now div 1000),
    Date = io_lib:format("~p-~2..0B-~2..0B", [Y,M,D]),
    #{index:=Temp} = EsOpts,
    Index = list_to_binary([Temp, $-, Date]),
    Obj2 = case nklib_json:encode(Obj) of
        error ->
            Obj#{BodyField => #{json => nklib_util:to_binary(Body)}};
        Json ->
            Json
    end,
    case nkelastic:put(EventId, Obj2, EsOpts#{index:=Index}) of
        {ok, Meta} ->
            {ok, EventId, Meta};
        {error, Error} ->
            {error, Error}
    end.








%% ===================================================================
%% Public
%% ===================================================================


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
