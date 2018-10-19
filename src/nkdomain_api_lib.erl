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

%% @doc NkDomain API processing
-module(nkdomain_api_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse_api_request/1]).
-export([get_group_vsn/1, api_path_to_actor_path/1]).
-export([remove_vsn/1, process_links/2]).
-export([api_group_list/1, api_groups/1, api_resources_list/2, make_actors_list/5]).

-include("nkdomain.hrl").
-include("nkdomain_api.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").


%% ===================================================================
%% Utilities
%% ===================================================================


%% @doc
api_group_list(SrvId) ->
    #{
        <<"kind">> => <<"APIGroupList">>,
        <<"apiVersion">> => <<"v1">>,
        <<"groups">> => api_groups(SrvId)
    }.


%% @doc
api_groups(SrvId) ->
    Groups = nkdomain_lib:nkdomain_get_api_groups(SrvId),
    lists:map(
        fun({Name, [Pref|_]=Versions}) ->
            #{
                <<"name">> => Name,
                <<"versions">> => lists:map(
                    fun(Vsn) ->
                        #{
                            <<"groupVersion">> => <<Name/binary, $/, Vsn/binary>>,
                            <<"version">> => Vsn
                        }
                    end,
                    Versions),
                <<"preferredVersion">> => #{
                    <<"groupVersion">> => <<Name/binary, $/, Pref/binary>>,
                    <<"version">> => Pref
                }
            }
        end,
        maps:to_list(Groups)).


%% @doc
api_resources_list(API, Resources) ->
    #{
        <<"kind">> => <<"APIResourceList">>,
        <<"groupVersion">> => API,
        <<"resources">> => Resources
    }.


%% @doc Generates a API actors list
make_actors_list(SrvId, Vsn, ListKind, ActorList, Meta) ->
    Items = lists:map(
        fun(Actor) ->
            case nkdomain_api:actor_to_external(SrvId, Actor, Vsn) of
                {ok, ApiActor} ->
                    ApiActor;
                {error, Error} ->
                    throw({error, Error})
            end
        end,
        ActorList),
    Size = maps:get(size, Meta),
    Total = maps:get(total, Meta, undefined),
    Result = api_list(ListKind, Items, Size, Total),
    {ok, Result}.



%% @private
api_list(Camel, Items, Size, Total) ->
    Meta1 = #{<<"size">> => Size},
    Meta2 =  case Total of
        undefined ->
            Meta1;
        _ ->
            Meta1#{<<"total">> => Total}
    end,
    #{
        <<"apiVersion">> => <<"v1">>,
        <<"items">> => Items,
        <<"kind">> => <<Camel/binary, "List">>,
        <<"metadata">> => Meta2
        %% See https://kubernetes.io/docs/reference/api-concepts/
        %% <<"continue">>
        %% <<"resourceVersion">,
        %% <<"selfLink">>
    }.




%%%% @doc
%%add_common_fields(SrvId, Actor) ->
%%    #actor{id=#actor_id{domain=Domain, group=Group, resource=Res}} = Actor,
%%    {ok, #{camel:=Kind}} = nkdomain_actor_util:get_config(SrvId, Group, Res),
%%    add_common_fields(Actor, Domain, Kind).
%%
%%
%%%% @doc
%%add_common_fields(Actor, Domain, Kind) ->
%%    #actor{data=Data, metadata=Meta} = Actor,
%%    Data2 = Data#{<<"kind">> => Kind},
%%    Meta2 = Meta#{<<"domain">> => Domain},
%%    Actor#actor{data=Data2, metadata=Meta2}.



%% @doc Generates an actor path (/domain/group/resource/name) from /apis...
-spec api_path_to_actor_path(term()) ->
    binary().

api_path_to_actor_path(Id) ->
    case binary:split(to_bin(Id), <<"/">>, [global]) of
        [<<>>, <<"apis">>, Group, _Vsn, <<"domains">>, Domain, Resource, Name] ->
            api_to_actor_path(Domain, Group, Resource, Name);
        [<<>>, <<"apis">>, Group, _Vsn, Resource, Name] ->
            api_to_actor_path(?ROOT_DOMAIN, Group, Resource, Name);
        _ ->
            % Must be a UID
            Id
    end.


%% @private Generates an actor path (/domain/group/resource/name) from api parts
api_to_actor_path(Domain, Group, Resource, Name) ->
    list_to_binary([$/, Domain, $/, Group, $/, Resource, $/, Name]).


%% @doc
get_group_vsn(ApiVsn) ->
    case binary:split(ApiVsn, <<"/">>) of
        [Group, Vsn] ->
            {Group, Vsn};
        [Group] ->
            {Group, <<>>};
        _ ->
            error
    end.


%%%% @doc Called to generate the API Group for an object
%%-spec get_api_group(nkservice:id(), nkdomain_api:group()|undefined,
%%    nkservice_actor:actor()) ->
%%    Group::binary().
%%
%%get_api_group(SrvId, Group, Actor) ->
%%    to_bin(?CALL_SRV(SrvId, nkdomain_actor_to_api_group, [SrvId, Group, Actor])).


%% @private
remove_vsn(Group) ->
    [_ | Rest] = lists:reverse(binary:split(Group, <<"/">>, [global])),
    nklib_util:bjoin(lists:reverse(Rest), $/).


%% @private
process_links(SrvId, #actor{id=ActorId, metadata=Meta}=Actor) ->
    #actor_id{domain=Domain}= ActorId,
    case nkdomain_domain:get_domain_data(SrvId, Domain) of
        {ok, _DomSrvId, DomUID} ->
            Links1 = maps:get(<<"links">>, Meta, #{}),
            Links2 = case Links1 of
                #{?RES_CORE_DOMAINS:=DomUID} ->
                    Links1;
                _ ->
                    Links1#{?RES_CORE_DOMAINS => DomUID}
            end,
            Links3 = expand_links(SrvId, maps:to_list(Links2), #{}),
            Meta2 = Meta#{<<"links">> => Links3},
            {ok, Actor#actor{metadata = Meta2}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
expand_links(_SrvId, [], Acc) ->
    Acc;

expand_links(SrvId, [{Type, Id}|Rest], Acc) ->
    Path = nkdomain_api_lib:api_path_to_actor_path(Id),
    expand_links(SrvId, Rest, Acc#{Type => Path}).


%% @doc Parses an API and makes sure group and vsn are available,
%% from api itself or body's apiVersion
-spec parse_api_request(map()) ->
    {ok, nkdomain_api:request()} | {error, nkservice:msg()}.

parse_api_request(Api) ->
    case nklib_syntax:parse(Api, api_syntax()) of
        {ok, #{group:=_, vsn:=_}=Api2, _} ->
            {ok, Api2};
        {ok, #{group:=_}, _} ->
            {error, {field_missing, <<"vsn">>}};
        {ok, #{vsn:=_}, _} ->
            {error, {field_missing, <<"group">>}};
        {ok, Api2, _} ->
            Body = maps:get(body, Api2, #{}),
            case
                is_map(Body) andalso
                nklib_syntax:parse(Body, #{apiVersion=>binary})
            of
                {ok, #{apiVersion:=ApiVsn}, _} ->
                    case get_group_vsn(ApiVsn) of
                        {Group, Vsn} ->
                            {ok, Api2#{group=>Group, vsn=>Vsn}};
                        error ->
                            {error, {field_missing, <<"group">>}}
                    end;
                _ ->
                    {error, {field_missing, <<"group">>}}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
api_syntax() ->
    #{
        verb => atom,
        group => binary,
        vsn => binary,
        domain => binary,
        resource => binary,
        name => binary,
        subresource => {list, binary},
        params => map,
        body => [map, binary],
        auth => map,
        meta => map,
        callback => atom,
        '__defaults' => #{
            verb => get,
            subresource => [],
            params => #{},
            body => #{},
            auth => #{},
            meta => #{}
        }
    }.


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).