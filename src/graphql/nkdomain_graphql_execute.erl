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

%% @doc Execute callback for GraphQL
-module(nkdomain_graphql_execute).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([execute/4]).
-export([get_value/2, get_value/3, get_time/3, get_map/2, get_object/2]).

%-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

%% ===================================================================
%% GraphQL Object callback
%% ===================================================================


%% @doc
%% Functions that read actors from disk return {nkdomain, {actor, _, _}}
%% (calling nkdomain_graphql_schema:make_object/2)
execute(SrvId, Field, {nkdomain, {actor, Type, Actor}}=Obj, Args) ->
    case Field of
        <<"id">> -> get_value(<<"uid">>, maps:get(<<"metadata">>, Actor));
        <<"type">> -> {ok, to_bin(Type)};
        <<"apiVersion">> -> get_value(<<"apiVersion">>, Actor);
        <<"kind">> -> get_value(<<"kind">>, Actor);
        <<"spec">> -> get_object(SrvId, {spec, Type, Actor});
        <<"metadata">> -> get_object(SrvId, {metadata, Actor});
        <<"data">> -> get_object(SrvId, {data, Actor});
        <<"status">> -> get_object(SrvId, {status, Type, Actor});
        _ -> actor_execute(SrvId, Type, Field, Obj, Args)
    end;

execute(SrvId, Field, {nkdomain, {spec, Type, _Spec, _Actor}}=Obj, Args) ->
    actor_execute(SrvId, Type, Field, Obj, Args);

execute(SrvId, Field, {nkdomain, {metadata, Meta}}, Args) ->
    case Field of
        <<"uid">> -> get_value(<<"uid">>, Meta);
        <<"name">> -> get_value(<<"name">>, Meta);
        <<"domain">> ->  get_value(<<"domain">>, Meta);
        <<"resourceVersion">> -> get_value(<<"resourceVersion">>, Meta);
        <<"generation">> -> get_value(<<"generation">>, Meta);
        <<"creationTime">> -> get_time(<<"creationTime">>, Meta, Args);
        <<"updateTime">> -> get_time(<<"updateTime">>, Meta, Args);
        <<"description">> -> get_value(<<"description">>, Meta);
        <<"selfLink">> -> get_value(<<"selfLink">>, Meta);
        <<"isEnabled">> -> get_value(<<"isEnabled">>, Meta, true);
        <<"expiresTime">> -> get_time(<<"expiresTime">>, Meta, Args);
        <<"subtype">> -> get_value(<<"subtype">>, Meta);
        <<"isInAlarm">> -> get_value(<<"isInAlarm">>, Meta, true);
        <<"alarms">> -> get_alarms(SrvId, Meta);
        <<"labels">> -> get_map(<<"labels">>, Meta);
        <<"annotations">> -> get_map(<<"annotations">>, Meta);
        <<"links">> -> get_map(<<"links">>, Meta);
        <<"fts">> -> get_map(<<"fts">>, Meta);
        _ -> continue
    end;

execute(SrvId, Field, {nkdomain, {status, Type, Status}}=Obj, Args) ->
    case Field of
        <<"isActivated">> -> get_value(<<"isActivated">>, Status);
        _ -> actor_execute(SrvId, Type, Field, Obj, Args)
    end;

execute(_SrvId, Field, {nkdomain, {field, core, alarms, Alarm}}, Args) ->
    case Field of
        _ when Field==<<"class">>; Field==<<"code">>; Field==<<"message">> ->
            get_value(Field, Alarm);
        <<"lastTime">> ->
            get_time(Field, Alarm, Args);
        <<"meta">> ->
            get_map(Field, Alarm)
    end;

execute(SrvId, Field, {nkdomain, {field, Type, _Sub, _Data}}=Obj, Args) ->
    actor_execute(SrvId, Type, Field, Obj, Args);

execute(_SrvId, Field, {nkdomain, {map, {K, V}}}, _Args) ->
    case Field of
        <<"key">> -> {ok, K};
        <<"value">> when is_list(V)-> {ok, [{ok, to_bin(T)} || T <- V]};
        <<"value">> -> {ok, to_bin(V)};
        _ -> continue
    end.


%% @private
actor_execute(SrvId, Type, Field, Obj, Args) ->
    case catch nkservice_graphql_plugin:get_config(SrvId, Type) of
        #{module:=Mod} ->
            case erlang:function_exported(Mod, execute, 5) of
                true ->
                    Mod:execute(SrvId, Field, Obj, #{}, Args);
                false ->
                    continue
            end;
        _ ->
            continue
    end.


%% @doc
get_value(Field, Map) ->
    get_value(Field, Map, null).

%% @doc
get_value(Field, Map, Default) ->
    {ok, maps:get(Field, Map, Default)}.


%% @doc
get_time(_Field, null, _) ->
    {ok, null};

get_time(Field, Obj, #{<<"format">>:={enum, <<"RFC3339">>}}) ->
    {ok, maps:get(Field, Obj, null)};

get_time(Field, Obj, #{<<"format">>:={enum, <<"UNIXTIME">>}}) ->
    case maps:get(Field, Obj, null) of
        null ->
            {ok, null};
        Time1 ->
            case nklib_date:to_epoch(Time1, msecs) of
                {ok, Time2} ->
                    {ok, to_bin(Time2)};
                error ->
                    {error, date_invalid}
            end
    end;

get_time(Field, Obj, _Args) ->
    get_time(Field, Obj, #{<<"format">>=>{enum, <<"RFC3339">>}}).


%% @doc
get_map(Key, Obj) ->
    Map = maps:get(Key, Obj, #{}),
    nkdomain_graphql_schema:make_object(any, {map, Map}).


%% @doc
get_object(SrvId, Obj) ->
    nkdomain_graphql_schema:make_object(SrvId, Obj).


%% @private
get_alarms(SrvId, Meta) ->
    List1 = lists:sort(maps:to_list(maps:get(<<"alarms">>, Meta, #{}))),
    List2 = [Body#{<<"class">>=>Class} || {Class, Body} <- List1],
    get_object(SrvId, {field, core, alarms, List2}).


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).

