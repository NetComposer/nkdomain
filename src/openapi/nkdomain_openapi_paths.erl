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

%% @doc NkDomain OpenAPI support
%% https://swagger.io/docs/specification/about/

-module(nkdomain_openapi_paths).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([paths/5]).
-import(nkdomain_openapi_util, [
            ok_response/1, created_response/1, updated_response/1,
            ref_parameter/1, ref_schema/1]).

-include("nkdomain.hrl").

%% ===================================================================
%% Public
%% ===================================================================


%% @doc
paths(SrvId, Group, Vsn, Resource, Opts) ->
    {ok, Config} = nkdomain_actor_util:get_config(SrvId, Group, Resource),
    Opts2 = maps:merge(Config, Opts#{vsn=>to_bin(Vsn)}),
    Map1 = make_actor_paths(Opts2),
    Map2 = case Opts of
        #{subpaths:=SubPathsSpec} ->
            % Subpaths include the actor name
            maps:fold(
                fun(SubPath, Spec, Acc) ->
                    make_actor_subpath(to_bin(SubPath), Spec, Opts2, Acc)
                end,
                Map1,
                SubPathsSpec);
        _ ->
            Map1
    end,
    Map3 = case Opts of
        #{subresources:=SubResSpec} ->
            % Subresources doesn't include the actor name
            % They refer to a resource group, not an specific one
            maps:fold(
                fun(SubPath, Spec, Acc) ->
                    make_actor_subresource(to_bin(SubPath), Spec, Opts2, Acc)
                end,
                Map2,
                SubResSpec);
        _ ->
            Map2
    end,
    Map3.


%% ===================================================================
%% Actor Paths
%% ===================================================================


make_actor_paths(Spec) ->
    #{verbs:=Verbs} = Spec,
    Allowed = [create, list, deletecollection, get, update, delete],
    VerbList = [{Verb, #{}} || Verb <-Verbs, lists:member(Verb, Allowed)],
    VerbMap = maps:from_list(VerbList),
    Spec2 = maps:merge(VerbMap, Spec),
    Resource1 = #{},
    Resource2 = case maps:find(create, Spec2) of
        {ok, Create} ->
            Resource1#{post => actor_path(create, Create, Spec2)};
        error ->
            Resource1
    end,
    Resource3 = case maps:find(list, Spec2) of
        {ok, List} ->
            Resource2#{get => actor_path(list, List, Spec2)};
        error ->
            Resource2
    end,
    Resource4 = case maps:find(deletecollection, Spec2) of
        {ok, DeleteCollection} ->
            Resource3#{delete => actor_path(deletecollection, DeleteCollection, Spec2)};
        error ->
            Resource3
    end,
    DomainResource4 = Resource4#{
        parameters => [
            ref_parameter("common.v1.ParamDomainName")
        ]
    },
    Actor1 = #{
        parameters => [
            ref_parameter("common.v1.ParamActorName")
        ]
    },
    Actor2 = case maps:find(get, Spec2) of
        {ok, Get} ->
            Actor1#{get => actor_path(get, Get, Spec2)};
        error ->
            Actor1
    end,
    Actor3 = case maps:find(update, Spec2) of
        {ok, Update} ->
            Actor2#{put => actor_path(update, Update, Spec2)};
        error ->
            Actor2
    end,
    Actor4 = case maps:find(delete, Spec2) of
        {ok, Delete} ->
            Actor3#{delete => actor_path(delete, Delete, Spec2)};
        error ->
            Actor3
    end,
    DomainActor4 = Actor4#{
        parameters => [
            ref_parameter("common.v1.ParamActorName"),
            ref_parameter("common.v1.ParamDomainName")
        ]
    },
    #{group:=Group, vsn:=Vsn, resource:=Resource} = Spec2,
    #{

        % /api/core/v1/users
        path(Group, Vsn, Resource) => Resource4,

        % /api/core/v1/domains/domain1/users
        path_d(Group, Vsn, Resource) => DomainResource4,

        % /api/core/v1/users/user1
        path_a(Group, Vsn, Resource) => Actor4,

        % /api/core/v1/domains/domain1/users/user1
        path_d_a(Group, Vsn, Resource) => DomainActor4
    }.


%% @private
actor_path(Verb, Spec, Opts) ->
    #{group:=Group, vsn:=Vsn, camel:=Kind} = Opts,
    DefDesc = actor_default_description(Verb, Kind),
    Map1 = #{
        description => maps:get(description, Spec, DefDesc),
        tags => actor_path_tags(Group, Vsn),
        parameters => actor_path_params(Verb, Spec),
        responses => actor_default_responses(Verb, Group, Vsn, Kind)
    },
    Map2 = case actor_default_request(Verb, Group, Vsn, Kind) of
        none ->
            Map1;
        RequestBody ->
            Map1#{requestBody => RequestBody}
    end,
    Map3 = case maps:get(callbacks, Spec, none) of
        none ->
            Map2;
        CBs ->
            Map2#{callbacks => CBs}
    end,
    Map3.


%% @private
actor_default_params(create) ->
    #{
        activate => <<"common.v1.ParamActivate">>,
        ttl => <<"common.v1.ParamActorTTL">>
    };

actor_default_params(list) ->
    #{
        from => <<"common.v1.ParamFrom">>,
        size => <<"common.v1.ParamSize">>,
        sort => <<"common.v1.ParamSort">>,
        fieldSelector => <<"common.v1.ParamFieldSelector">>,
        labelSelector => <<"common.v1.ParamLabelSelector">>,
        linkedTo => <<"common.v1.ParamLinkedTo">>,
        fts => <<"common.v1.ParamFts">>,
        deep => <<"common.v1.ParamDeep">>,
        watch => <<"common.v1.ParamWatch">>,
        resourceVersion => <<"common.v1.ParamResourceVersion">>,
        totals => <<"common.v1.ParamTotals">>
    };

%% @private
actor_default_params(deletecollection) ->
    #{
        deep => <<"common.v1.ParamDeep">>,
        labelSelector => <<"common.v1.ParamLabelSelector">>,
        linkedTo => <<"common.v1.ParamLinkedTo">>,
        totals => <<"common.v1.ParamTotals">>
    };

%% @private
actor_default_params(get) ->
    #{
        activate => <<"common.v1.ParamActivate">>,
        ttl => <<"common.v1.ParamActorTTL">>,
        watch => <<"common.v1.ParamWatch">>,
        resourceVersion => <<"common.v1.ParamResourceVersion">>
    };

actor_default_params(update) ->
    #{
    };

actor_default_params(delete) ->
    #{
        cascade => <<"common.v1.ParamCascade">>
    }.


%% @private
actor_path_params(Verb, Spec) ->
    DefParams = actor_default_params(Verb),
    Params = maps:get(parameters, Spec, #{}),
    Params2 = maps:merge(DefParams, Params),
    actor_path_params(Params2).


%% @private
actor_path_params(Params) ->
    [ref_parameter(Value) || Value <-maps:values(Params)].


%% @private
actor_default_description(create, Kind) ->
    <<"Create a ", Kind/binary, ". Netcomposer verb is 'create'.">>;

actor_default_description(list, Kind) ->
    <<"
        List or watch objects of Kind ", Kind/binary, ".
        Netcomposer verb is 'list' or 'watch' if that option is used.
    ">>;

actor_default_description(deletecollection, Kind) ->
    <<"
        Deletes a collection of resources of type ", Kind/binary, ".
        Netcomposer verb is 'deletecollection'.
    ">>;

actor_default_description(get, Kind) ->
    <<"Gets a ", Kind/binary, ". Netcomposer verb is 'get'.">>;

actor_default_description(update, Kind) ->
    <<"Updates or creates a ", Kind/binary, ". Netcomposer verb is 'update'.">>;

actor_default_description(delete, Kind) ->
    <<"Deletes a ", Kind/binary, ". Netcomposer verb is 'delete'.">>.


%% @private
actor_default_request(create, Group, Vsn, Kind) ->
    #{
        required => true,
        content => #{
            'application/json' => #{
                schema => ref_schema([Group, $., Vsn, $., Kind])
            },
            'application/yaml' => #{
                schema => ref_schema([Group, $., Vsn, $., Kind])
            }
        }
    };

actor_default_request(update, Group, Vsn, Kind) ->
    #{
        required => true,
        content => #{
            'application/json' => #{
                schema => ref_schema([Group, $., Vsn, $., Kind])
            },
            'application/yaml' => #{
                schema => ref_schema([Group, $., Vsn, $., Kind])
            }
        }
    };

actor_default_request(_Verb, _Group, _Vsn, _Kind) ->
    none.


%% @private
actor_default_responses(create, Group, Vsn, Kind) ->
    created_response([Group, $., Vsn, $., Kind]);

actor_default_responses(list, Group, Vsn, Kind) ->
    ok_response([Group, $., Vsn, $., Kind, "List"]);

actor_default_responses(deletecollection, _Group, _Vsn, _Kind) ->
    ok_response("common.v1.Status");

actor_default_responses(get, Group, Vsn, Kind) ->
    ok_response([Group, $., Vsn, $., Kind]);

actor_default_responses(update, Group, Vsn, Kind) ->
    updated_response([Group, $., Vsn, $., Kind]);

actor_default_responses(delete, _Group, _Vsn, _Kind) ->
    ok_response("common.v1.Status").


%%%% @private
%%actor_default_callbacks(create) ->
%%    #{onEvent => cb_on_event()};
%%
%%actor_default_callbacks(update) ->
%%    #{onEvent => cb_on_event()};
%%
%%actor_default_callbacks(_Verb) ->
%%    #{}.


%% @private
actor_path_tags(Group, Vsn) ->
    [
        Group,
        <<Group/binary, "_", Vsn/binary>>
    ].


%%%% @private
%%cb_on_event() ->
%%    #{
%%        '{$request.body.metadata.eventsCallbackUrl}' => #{
%%            post => #{
%%                requestBody => #{
%%                    description => <<"Fired Event.">>,
%%                    content => #{
%%                        'application/json' => #{
%%                            schema => ref_schema("core.v1a1.Event")
%%                        }
%%                    }
%%                },
%%                responses => #{
%%                    '200' => #{
%%                        description => <<"OK">>
%%                    }
%%                }
%%            }
%%        }
%%    }.


%% ===================================================================
%% Actor subpath
%% ===================================================================


make_actor_subpath(SubPath, Spec, Opts, Map) ->
    Actor1 = #{
        parameters => [
            ref_parameter("common.v1.ParamActorName")
        ]
    },
    Actor2 = case maps:find(get, Spec) of
        {ok, Get} ->
            Actor1#{get => actor_subpath(get, Get, Opts)};
        error ->
            Actor1
    end,
    Actor3 = case maps:find(create, Spec) of
        {ok, Create} ->
            Actor2#{post => actor_subpath(create, Create, Opts)};
        error ->
            Actor2
    end,
    Actor4 = case maps:find(update, Spec) of
        {ok, Update} ->
            Actor3#{put => actor_subpath(update, Update, Opts)};
        error ->
            Actor3
    end,
    Actor5 = case maps:find(delete, Spec) of
        {ok, Delete} ->
            Actor4#{delete => actor_subpath(delete, Delete, Opts)};
        error ->
            Actor4
    end,
    DomainActor5 = Actor5#{
        parameters => [
            ref_parameter("common.v1.ParamActorName"),
            ref_parameter("common.v1.ParamDomainName")
        ]
    },
    #{group:=Group, vsn:=Vsn, resource:=Resource} = Opts,
    Map#{
        path_a_s(Group, Vsn, Resource, SubPath) => Actor4,
        path_d_a_s(Group, Vsn, Resource, SubPath) => DomainActor5
    }.


make_actor_subresource(SubResource, Spec, Opts, Map) ->
    Resource1 = #{},
    Resource2 = case maps:find(create, Spec) of
        {ok, Create} ->
            Resource1#{post => actor_subpath(create, Create, Opts)};
        error ->
            Resource1
    end,
    Resource3 = case maps:find(list, Spec) of
        {ok, List} ->
            Resource2#{get => actor_subpath(list, List, Opts)};
        error ->
            Resource2
    end,
    Resource4 = case maps:find(deletecollection, Spec) of
        {ok, DeleteCollection} ->
            Resource3#{delete => actor_subpath(deletecollection, DeleteCollection, Opts)};
        error ->
            Resource3
    end,
    DomainResource4 = Resource4#{
        parameters => [
            ref_parameter("common.v1.ParamDomainName")
        ]
    },
    #{group:=Group, vsn:=Vsn, resource:=Resource} = Opts,
    Map#{
        path_s(Group, Vsn, Resource, SubResource) => Resource4,
        path_d_s(Group, Vsn, Resource, SubResource) => DomainResource4
    }.


%% @doc
actor_subpath(Verb, Spec, Opts) ->
    #{group:=Group, camel:=Kind, vsn:=Vsn}=Opts,
    Responses = case maps:find(responses, Spec) of
        {ok, default} ->
            actor_default_responses(Verb, Group, Vsn, Kind);
        {ok, SpecResponses} ->
            SpecResponses;
        error ->
            ok_response("common.v1.Status")
    end,
    Map1 = #{
        description => maps:get(description, Spec, <<>>),
        tags => actor_path_tags(Group, Vsn),
        parameters => actor_path_params(maps:get(parameters, Spec, #{})),
        responses => Responses
    },
    Map2 = case Spec of
        #{callbacks:=CBs} ->
            Map1#{callbacks => CBs};
        _ ->
            Map1
    end,
    Map3 = case maps:find(request_schema, Spec) of
        {ok, Schema} ->
            Map2#{
                requestBody => #{
                    required => true,
                    content => #{
                        'application/json' => #{
                            schema => ref_schema(Schema)
                        },
                        'application/yaml' => #{
                            schema => ref_schema(Schema)
                        }
                    }
                }
            };
        error ->
            case maps:find(request, Spec) of
                {ok, ReqBody} ->
                    Map2#{requestBody => ReqBody};
                error ->
                    Map2
            end
    end,
    Map3.



%% ===================================================================
%% Utilities
%% ===================================================================


%% @private
path(Group, Vsn, Resource) ->
    <<"/apis/", Group/binary, "/", Vsn/binary, "/", Resource/binary>>.


%% @private
path_d(Group, Vsn, Resource) ->
    <<"/apis/", Group/binary, "/", Vsn/binary, "/domains/{domainName}/", Resource/binary>>.


%% @private
path_a(Group, Vsn, Resource) ->
    <<"/apis/", Group/binary, "/", Vsn/binary, "/", Resource/binary, "/{actorName}">>.


%% @private
path_d_a(Group, Vsn, Resource) ->
    <<"/apis/", Group/binary, "/", Vsn/binary, "/domains/{domainName}/", Resource/binary, "/{actorName}">>.


%% @private Subpath: with actor name
path_a_s(Group, Vsn, Resource, Subresource) ->
    <<(path_a(Group, Vsn, Resource))/binary, "/", Subresource/binary>>.


%% @private Subpath: with actor name (and domain)
path_d_a_s(Group, Vsn, Resource, Subresource) ->
    <<(path_d_a(Group, Vsn, Resource))/binary, "/", Subresource/binary>>.


%% @private Subresource: without actor name
path_s(Group, Vsn, Resource, Subresource) ->
    <<(path(Group, Vsn, Resource))/binary, "/", Subresource/binary>>.


%% @private Subresource: without actor name (with domain)
path_d_s(Group, Vsn, Resource, Subresource) ->
    <<(path_d(Group, Vsn, Resource))/binary, "/", Subresource/binary>>.




%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
