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

%% @doc NkDomain library for nkdomain_callbacks
-module( nkdomain_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([nkdomain_get_api_groups/1, nkdomain_get_api_resources/3]).
-export([get_paths/2, get_api_resources/3]).
-export([core_api_event/2, launch_auto_activated/1, get_auto_activated/1]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkpacket/include/nkpacket.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN LIB: "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
nkdomain_get_api_groups(SrvId) ->
    ?CALL_SRV(SrvId, nkdomain_api_get_groups, [#{}]).


%% @private
nkdomain_get_api_resources(SrvId, Group, Vsn) ->
    ?CALL_SRV(SrvId, nkdomain_api_get_resources, [SrvId, Group, Vsn]).





%% ===================================================================
%% Callback support
%% ===================================================================


%% @private
get_paths(SrvId, Acc) ->
    [
        <<"/apis">>,
        <<"/apis-ws">>,
        <<"/openapi">>,
        <<"/graphql">>,
        get_api_paths(SrvId),
        Acc
    ].


%% @private
get_api_paths(SrvId) ->
    lists:foldl(
        fun(Map, Acc) ->
            case Map of
                #{<<"versions">>:=Versions} ->
                    GV1 = [<<"/apis/", V/binary>> || #{<<"groupVersion">>:=V} <- Versions],
                    GV2 = [nkdomain_api_lib:remove_vsn(G) || G <-GV1],
                    Acc++GV1++GV2;
                _ ->
                    Acc
            end
        end,
        [],
        nkdomain_api_lib:api_groups(SrvId)).



%% @private TODO vsn processing!
get_api_resources(SrvId, Group, _Vsn) ->
    case catch nkdomain_plugin:get_resources(SrvId, Group) of
        Resources when is_list(Resources) ->
            lists:map(
                fun(Resource) ->
                    {ok, Config} = nkdomain_actor_util:get_config(SrvId, Group, Resource),
                    #{
                        singular := Singular,
                        camel := Kind,
                        verbs := Verbs,
                        short_names := ShortNames
                    } = Config,
                    #{
                        <<"name">> => Resource,
                        <<"singularName">> => Singular,
                        <<"kind">> => Kind,
                        <<"verbs">> => Verbs,
                        <<"shortNames">> => ShortNames
                    }
                end,
                Resources);
        _ ->
            undefined
    end.


%% @doc
core_api_event(Event, #actor_st{srv=SrvId, actor=Actor}=ActorSt) ->
    Reason = case Event of
        created ->
            <<"ActorCreated">>;
        {updated, _UpdActor} ->
            <<"ActorUpdated">>;
        deleted ->
            <<"ActorDeleted">>
    end,
    Body = case Event of
        deleted ->
            #{};
        _ ->
            case nkdomain_api:actor_to_external(SrvId, Actor) of
                {ok, ApiActor} ->
                    #{<<"actor">> => ApiActor};
                {error, _} ->
                    #{}
            end
    end,
    ApiEvent = #{reason => Reason, body => Body},
    nkdomain_actor_util:api_event(ApiEvent, ActorSt).


%% @doc Perform an activation of objects in database belonging to actors
%% having auto_activate=true and not yet activated
%% Actors that are expired will be deleted on activation
%% Size must be big enough so that last record has a newer date than first record
%% (so it cannot be 1)
-spec launch_auto_activated(nkservice:id()) ->
    [#actor_id{}].

launch_auto_activated(SrvId) ->
    ClassTypes = get_auto_activated(SrvId),
    OrSpec = [#{field=><<"group+resource">>, value=>ClassType} || ClassType <- ClassTypes],
    launch_auto_activated(SrvId, <<>>, 5, OrSpec, []).


%% @private
launch_auto_activated(SrvId, From, Size, OrSpec, Acc) ->
    Spec = #{
        deep => true,
        totals => false,
        filter => #{
            'or' => OrSpec,
            'and' => [#{field=><<"metadata.updateTime">>, op=>gte, value=>From}]
        },
        sort => [#{field=><<"metadata.updateTime">>, order=>asc}],
        size => Size
    },
    case nkservice_actor:search_ids(SrvId, Spec, #{}) of
        {ok, ActorIds, #{last_updated:=Last}} ->
            Acc2 = activate_actors(SrvId, ActorIds, Acc),
            case length(ActorIds) >= Size of
                true when Last > From ->
                    launch_auto_activated(SrvId, Last, Size, OrSpec, Acc2);
                true ->
                    ?LLOG(warning, "too many records for auto activation: ~p", [From]),
                    {error, too_many_records};
                false ->
                    Acc2
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
activate_actors(_SrvId, [], Acc) ->
    Acc;

activate_actors(SrvId, [ActorId|Rest], Acc) ->
    #actor_id{group=Group, resource=Res, name=Name} = ActorId,
    Acc2 = case lists:member(ActorId, Acc) of
        true ->
            % lager:error("NKLOG SKIPPING:~p", [Name]),
            Acc;
        false ->
            case nkservice_actor_db:is_activated({SrvId, ActorId}) of
                {true, _} ->
                    ?LLOG(debug, "actor ~s/~s/~s/~s already activated",
                          [SrvId, Group, Res, Name]),
                    Acc;
                false ->
                    case nkservice_actor:activate({SrvId, ActorId}) of
                        {ok, _, _} ->
                            ?LLOG(notice, "activated actor ~s/~s/~s/~s",
                                  [SrvId, Group, Res, Name]);
                        {error, Error} ->
                            ?LLOG(warning, "could not activate actor ~s/~s/~s/~s: ~p",
                                  [SrvId, Group, Res, Name, Error])
                    end,
                    [ActorId|Acc]
            end
    end,
    activate_actors(SrvId, Rest, Acc2).



%% @private
-spec get_auto_activated(nkservice:id()) ->
    [binary()].

get_auto_activated(SrvId) ->
    lists:foldl(
        fun(Group, Acc1) ->
            lists:foldl(
                fun(Type, Acc2) ->
                    case nkdomain_actor_util:get_config(SrvId, Group, Type) of
                        {ok, #{auto_activate:=true}} ->
                            [<<Group/binary, $+, Type/binary>>|Acc2];
                        _ ->
                            Acc2
                    end
                end,
                Acc1,
                nkdomain_plugin:get_resources(SrvId, Group))
        end,
        [],
        nkdomain_plugin:get_groups(SrvId)).





%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).