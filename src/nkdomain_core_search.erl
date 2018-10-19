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

%% @doc NkDomain core search facility

-module(nkdomain_core_search).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([search/2, type_search/4, search_params_id/4, do_delete/2]).
-export_type([search_spec/0]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-type search_spec() ::
    #{
        apiVersion => nkdomain_api:api_vsn(),
        kind => nkdomain_api:kind(),
        domain => nkdomain_api:domain(),
        deep => boolean(),
        from => pos_integer(),
        size => pos_integer(),
        totals => boolean(),
        filter => nkservice_actor_search:filter(),
        sort => [nkservice_actor_search:sort_spec()]
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Generic search for 'search' API
search(SrvId, #{verb:=Verb, domain:=Domain, body:=Body}) ->
    case search_spec(SrvId, Domain, Body) of
        {ok, Group, Type, Kind, Spec} ->
            case Verb of
                list ->
                    case do_search_actors(SrvId, Group, Type, Spec) of
                        {ok, ActorList, Meta} ->
                            nkdomain_api_lib:make_actors_list(SrvId, undefined, Kind,
                                                       ActorList, Meta);
                        {error, Error} ->
                            {error, Error}
                    end;
                deletecollection ->
                    case do_search_ids(SrvId, Group, Type, Spec) of
                        {ok, ActorIds, _Meta} ->
                            do_delete(SrvId, ActorIds)
                    end;
                _ ->
                    {error, api_invalid}
            end;
        {error, Error} ->
            {error, Error}
    end;

search(_SrvId, _ApiReq) ->
    {error, api_invalid}.


%% @doc Type search
type_search(SrvId, ActorId, Config, Params) ->
    #actor_id{domain=Domain, group=Group, resource=Res} = ActorId,
    try
        Spec2 = parse_params(maps:to_list(Params), #{}),
        Spec3 = do_parse(SrvId, Domain, Spec2, Config),
        do_search_actors(SrvId, Group, Res, Spec3)
    catch
        throw:Throw ->
            Throw
    end.


%% @private
search_params_id(SrvId, ActorId, Config, Params) ->
    #actor_id{domain=Domain, group=Group, resource=Res} = ActorId,
    try
        Spec2 = parse_params(maps:to_list(Params), #{}),
        Spec3 = do_parse(SrvId, Domain, Spec2, Config),
        do_search_ids(SrvId, Group, Res, Spec3)
    catch
        throw:Throw ->
            Throw
    end.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
search_spec(SrvId, Domain, Body) ->
    try
        Syntax = #{
            <<"apiGroup">> => binary,
            <<"kind">> => binary,
            '__allow_unknown' => true
        },
        case nklib_syntax:parse(Body, Syntax) of
            {ok, #{<<"apiGroup">>:=Group, <<"kind">>:=Kind}=Body2, _} ->
                % Since we have full spec, get config and specific fields
                case nkdomain_actor_util:find_resource(SrvId, Group, Kind) of
                    {camel, Type} ->
                        {ok, Config} = nkdomain_actor_util:get_config(SrvId, Group, Type),
                        Body3 = maps:without([<<"apiGroup">>, <<"kind">>], Body2),
                        Spec = do_parse(SrvId, Domain, Body3, Config),
                        {ok, Group, Type, Kind, Spec};
                    _ ->
                        throw({error, {kind_unknown, Kind}})
                end;
            {ok, #{<<"apiGroup">>:=Group}=Body2, _} ->
                {Group, _} = nkdomain_api_lib:get_group_vsn(Group),
                Body3 = maps:without([<<"apiGroup">>], Body2),
                Spec = do_parse(SrvId, Domain, Body3, base_config()),
                {ok, Group, all, <<"Actor">>, Spec};
            {ok, #{<<"kind">>:=_}, _} ->
                {error, {field_missing, <<"apiGroup">>}};
            {ok, Body2, _} ->
                Spec = do_parse(SrvId, Domain, Body2, base_config()),
                {ok, all, all, <<"Actor">>, Spec};
            {error, Error} ->
                {error, Error}
        end
    catch
        throw:Throw ->
            Throw
    end.


%% @private
base_config() ->
    #{
        filter_fields => nkdomain_actor:filter_fields(),
        sort_fields => nkdomain_actor:sort_fields(),
        field_type => nkdomain_actor:field_type(),
        field_trans => nkdomain_actor:field_trans()
    }.


%% @doc Type search
do_search_actors(SrvId, Group, Type, Spec) ->
    Query = {service_search_actors, Group, Type, Spec},
    nkservice_actor_db:search(SrvId, Query).


%% @private
do_search_ids(SrvId, Group, Type, Spec) ->
    Query = {service_search_actors_type_id, Group, Type, Spec},
    nkservice_actor_db:search(SrvId, Query).


%% @doc
do_delete(SrvId, ActorIds) ->
    UIDs = [UID || #actor_id{uid=UID} <- ActorIds],
    case nkservice_actor_db:delete_multi(SrvId, UIDs) of
        {ok, ActorIds2, _Meta} ->
            UIDs2 = lists:reverse([UID || #actor_id{uid=UID} <- ActorIds2]),
            case UIDs == UIDs2 of
                true ->
                    ok;
                false ->
                    lager:error("NKLOG DELETECOLLECTION\n~p\n~p", [UIDs, UIDs2])
            end,
            {status, {actors_deleted, length(UIDs2)}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
%% Supported: from, size, deep, totals, labels, fts, links, sort
%% Throws error
do_parse(_SrvId, Domain, Body, Config) ->
    SearchOpts = maps:with([filter_fields, sort_fields, field_type, field_trans], Config),
    Body2 = case is_map(Body) of
        true ->
            Body#{domain => Domain};
        false when Body == <<>> ->
            #{domain => Domain};
        false ->
            throw({body_invalid, Body})
    end,
    Spec = case nkservice_actor_search:parse(Body2, SearchOpts) of
        {ok, BodySpec} ->
            BodySpec;
        {error, Error1} ->
            throw({error, Error1})
    end,
    case maps:get(sort, Spec, []) of
        [] ->
            Spec#{sort => [#{field=><<"metadata.updateTime">>, order=>desc}]};
        _ ->
            Spec
    end.


%% @private
parse_params([], Spec) ->
    Spec;

parse_params([{Key, Val}|Rest], Spec)
        when Key==from; Key==size; Key==deep; Key==totals ->
    parse_params(Rest, Spec#{Key => Val});

parse_params([{labelSelector, Labels}|Rest], Spec) ->
    Labels2 = binary:split(Labels, <<$,>>, [global]),
    Spec2 = parse_labels(Labels2, Spec),
    parse_params(Rest, Spec2);

parse_params([{fts, Fts}|Rest], Spec) ->
    Fts2 = binary:split(Fts, <<$,>>, [global]),
    Spec2 = parse_fts(Fts2, Spec),
    parse_params(Rest, Spec2);

parse_params([{linkedTo, Links}|Rest], Spec) ->
    Links2 = binary:split(Links, <<$,>>, [global]),
    Spec2 = parse_links(Links2, Spec),
    parse_params(Rest, Spec2);

parse_params([{sort, Sort}|Rest], Spec) ->
    Sort2 = binary:split(Sort, <<$,>>, [global]),
    Sort3 = parse_sort(Sort2, []),
    parse_params(Rest, Spec#{sort=>Sort3});

parse_params([{fieldSelector, Value}|Rest], Spec) ->
    Values2 = binary:split(Value, <<$,>>, [global]),
    Spec2 = parse_extra_fields(Values2, Spec),
    parse_params(Rest, Spec2);

parse_params([_|Rest], Spec) ->
    parse_params(Rest, Spec).


%% @private
parse_labels([], Spec) ->
    Spec;

parse_labels([Field|Rest], Spec) ->
    {Field2, Op, Value2} = case binary:split(Field, <<":">>) of
        [K0, V0] ->
            {<<"metadata.labels.", K0/binary>>, eq, V0};
        [K0] ->
            {<<"metadata.labels.", K0/binary>>, exists, true}
    end,
    Spec2 = add_and_filter(#{field=>Field2, op=>Op, value=>Value2}, Spec),
    parse_labels(Rest, Spec2).


parse_links([], Spec) ->
    Spec;

parse_links([Field|Rest], Spec) ->
    {Field2, Op, Value2} = case binary:split(Field, <<":">>) of
        [Type, UID] ->
            {<<"metadata.links.", Type/binary>>, eq, UID};
        [Type] ->
            {<<"metadata.links.", Type/binary>>, exists, true}
    end,
    Spec2 = add_and_filter(#{field=>Field2, op=>Op, value=>Value2}, Spec),
    parse_links(Rest, Spec2).


%% @private
parse_fts([], Spec) ->
    Spec;

parse_fts([Field|Rest], Spec) ->
    {Field2, Val2} = case binary:split(Field, <<":">>) of
        [F0, V0] ->
            {<<"metadata.fts.", F0/binary>>, V0};
        [V0] ->
            {<<"metadata.fts.*">>, V0}
    end,
    {Op, Val3} = case binary:split(Val2, <<"*">>) of
        [V1, <<>>] ->
            {prefix, V1};
        _ ->
            {eq, Val2}
    end,
    Spec2 = add_and_filter(#{field=>Field2, op=>Op, value=>Val3}, Spec),
    parse_fts(Rest, Spec2).


%% @private
parse_sort([], Acc) ->
    lists:reverse(Acc);

parse_sort([Field|Rest], Acc) ->
    {Order, Field2} = case Field of
        <<"asc:", F2/binary>> ->
            {asc, F2};
        <<"desc:", F2/binary>> ->
            {desc, F2};
        _ ->
            {desc, Field}
    end,
    Sort = #{field=>Field2, order=>Order},
    parse_sort(Rest, [Sort|Acc]).


%% @private
parse_extra_fields([], Spec) ->
    Spec;


parse_extra_fields([Value|Rest], Spec) ->
    {Field, Value2} = case binary:split(Value, <<":">>) of
        [P1, P2] ->
            {P1, P2};
        [P1] ->
            {P1, <<>>}
    end,
    Spec2 = parse_field_op(Field, Value2, Spec),
    parse_extra_fields(Rest, Spec2).


%% @private
add_and_filter(Filter, Spec) ->
    SpecFilter = maps:get(filter, Spec, #{}),
    And1 = maps:get('and', SpecFilter, []),
    And2 = [Filter|And1],
    SpecFilter2 = SpecFilter#{'and' => And2},
    Spec#{filter => SpecFilter2}.


%% @private
parse_field_op(Field, Value, Spec) ->
    Filter = case binary:split(to_bin(Value), <<":">>) of
        [Op, Value2] ->
            Op2 = case catch binary_to_existing_atom(Op, latin1) of
                eq -> eq;
                ne -> ne;
                gt -> gt;
                gte -> gte;
                lt -> lt;
                lte -> lte;
                prefix -> prefix;
                _ -> throw({error, {field_op, Op}})
            end,
            #{field=>Field, op=>Op2, value=>Value2};
        [<<>>] ->
            #{field=>Field, op=>exists, value=>true};
        [Value2] ->
            #{field=>Field, op=>eq, value=>Value2}
    end,
    add_and_filter(Filter, Spec).


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
