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

%% @doc NkDomain main module
-module(nkdomain_graphql_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([execute/4]).
-export([object_schema_scalars/0, object_schema_enums/0, object_schema_interfaces/0,
         object_schema_types/0, object_schema_inputs/0, object_schema_queries/0, object_schema_mutations/0]).
-export([object_query/3]).
-export([object_fields/0]).

-include("nkdomain.hrl").
-include("nkdomain_graphql.hrl").

%% ===================================================================
%% GraphQL Object callback
%% ===================================================================


%% @doc Called from GraphQL to extract fields on any type
execute(Ctx, {#obj_id_ext{obj_id=ObjId, type=Type}=ObjIdExt, Obj}, Field, Args) ->
    case object_execute(Field, ObjIdExt, Obj, Args, Ctx) of
        {ok, Res} ->
            {ok, Res};
        unknown ->
            case nkdomain_reg:get_type_module(Type) of
                undefined ->
                    {error, unknown_type};
                Module ->
                    case erlang:function_exported(Module, object_execute, 5) of
                        true ->
                            Res = Module:object_execute(Field, ObjIdExt, Obj, Args, Ctx),
                            lager:notice("NKLOG RES: ~p", [Res]),
                            Res;
                        false ->
                            lager:error("NKLOG No execute module ~p", [Module]),
                            null
                    end
            end;
        {error, Error} ->
            lager:warning("NKLOG Obj execute error (~p, ~p): ~p", [ObjId, Field, Error]),
            {error, Error}
    end;

execute(_Ctx, #search_results{objects=Objects}, <<"objects">>, _) ->
    {ok, [{ok, Obj} || Obj <-Objects]};

execute(_Ctx, #search_results{total_count=TotalCount}, <<"totalCount">>, _) ->
    {ok, TotalCount};

execute(_Ctx, #search_results{page_info=PageInfo}, <<"pageInfo">>, _) ->
    {ok, PageInfo};

execute(_Ctx, #search_results{cursor=Cursor}, <<"cursor">>, _) ->
    {ok, Cursor};

execute(_Ctx, #page_info{has_next_page=Next}, <<"hasNextPage">>, _) ->
    {ok, Next};

execute(_Ctx, #page_info{has_previous_page=Previous}, <<"hasPreviousPage">>, _) ->
    {ok, Previous};

execute(_Ctx, Data, Field, _Args) ->
    lager:error("Invalid execute at ~p: ~p, ~p", [?MODULE, Data, Field]),
    error(invalid_execute).




%% ===================================================================
%% Common Schema
%% ===================================================================


%% @doc Generates new scalars
-spec object_schema_scalars() ->
    #{nkdomain_graphql:schema_type() => #{comment => string()}}.

object_schema_scalars() ->
    #{
        'Cursor' => #{comment=>"Pagination cursor"},
        'UnixTime' => #{comment=>"Standard milisecond-resolution unix time"}
    }.


%% @doc Generates new enums
-spec object_schema_enums() ->
    #{nkdomain_graphql:schema_type() => #{opts => [atom()], comment => string()}}.

object_schema_enums() ->
    #{
        objectType => #{
            opts => nkdomain_reg:get_all_schema_types(),
            comment => "Object Types"
        },
        objectSortByField => #{
            opts => [domainId, createdById, createdTime, enabled, expiresTime,
                     name, objName, path, srvId, type]
        },
        filterOp => #{
            opts => ['AND', 'OR', 'NOT'],
            comment => "Operation mode for a filter"
        },
        sortOrder => #{
            opts => [asc, desc]
        }
    }.


%% @doc Generates new types
-spec object_schema_types() ->
    #{
        nkdomain_graphql:schema_type() => #{
            fields => nkdomain_graphql:schema_fields(),
            comment => string(),
            is_object => boolean(),         % Generates an Object instance
            is_connection => boolean()      % Generates specific connection types
        }
    }.

object_schema_types() ->
    #{
        'SearchResult' => #{
            fields => #{
                objects => {list_no_null, 'Object', #{comment => "My Objects"}},
                pageInfo => {no_null, 'PageInfo'},
                totalCount => int
            }
        },
        'PageInfo' => #{
            fields => #{
                hasNextPage => {no_null, boolean},
                hasPreviousPage => {no_null, boolean}
            }
        }
    }.


%% @doc Generates new inputs
-spec object_schema_inputs() ->
    #{
        nkdomain_graphql:schema_type() => #{
            fields => nkdomain_graphql:schema_fields(),
            comment => string()
        }
    }.

object_schema_inputs() ->
    #{
        objectFilterId => #{
            fields => #{
                eq => string,
                values => {list, string},
                exists => bool
            }
        },
        objectFilterType => #{
            fields => #{
                eq => objectType,
                values => {list, objectType}
            }
        },
        objectFilterKeyword => #{
            fields => #{
                eq => string,
                values => {list, string},
                gt => string,
                gte => string,
                lt => string,
                lte => string,
                prefix => string,
                exists => bool
            }
        },
        objectFilterText => #{
            fields => #{
                eq => string,
                prefix => string,
                wordsAndPrefix => string,
                fuzzy => string
            }
        },
        objectFilterPath => #{
            fields => #{
                eq => string,
                values => {list, string},
                gt => string,
                gte => string,
                lt => string,
                lte => string,
                childsOf => string,
                exists => bool
            }
        },
        objectFilterInt => #{
            fields => #{
                values => {list, int},
                eq => int,
                ne => int,
                gt => int,
                gte => int,
                lt => int,
                lte => int,
                exists => bool
            }
        },
        objectFilterBoolean => #{
            fields => #{
                eq => boolean,
                exists => bool
            }
        },
        objectFilter => #{
            fields => object_fields_filter(),
            comment => "Filter values to sort on"
        },
        objectSortBy => #{
            fields => #{
                field => {no_null, objectSortByField},
                sortOrder => {sortOrder, #{default => <<"asc">>}}
            },
            comment => "Fields to sort on"
        }
    }.


%% @doc Generates new interfaces
-spec object_schema_interfaces() ->
    #{
        nkdomain_graphql:schema_type() => #{
            fields => nkdomain_graphql:schema_fields(),
            comment => string()
        }
    }.

object_schema_interfaces() ->
    #{
        'Node' => #{
            fields => #{id => {no_null, id}},
            comment => "Relay Modern Node Interface"
        },
        'Object'=> #{
            fields => object_fields(),
            comment => "Standard NetComposer Object"
        }
    }.


%% @doc Generates new queries
-spec object_schema_queries() ->
    #{nkdomain_graphql:query_name() => nkdomain_graphql:field_value()}.

object_schema_queries() ->
    #{
        node => {'Node', #{
                     params => #{id => {no_null, id}},
                     comment => "Relay Modern specification Node fetcher"
                 }},
        allObjects => {'SearchResult', #{
                           params => #{
                               filter => {list, objectFilter, #{default => "[]"}},
                               sort => {list, objectSortBy},
                               from => {int, #{default => 0}},
                               size => {int, #{default => 10}}
                           }}}
    }.


%% @doc Generates new mutations
-spec object_schema_mutations() ->
    #{
        nkdomain_graphql:mutation_name() => #{
            inputs => nkdomain_graphql:schema_fields(),
            outputs => nkdomain_graphql:schema_fields(),
            comment => string()
        }
    }.

object_schema_mutations() ->
    #{
    }.



%% ===================================================================
%% Queries implementations
%% ===================================================================

%% @doc
object_query(<<"node">>, #{<<"id">>:=Id}, _Ctx) ->
    case nkdomain_lib:read(Id) of
        {ok, #obj_id_ext{}=ObjIdExt, Obj} ->
            {ok, {ObjIdExt, Obj}};
        {error, Error} ->
            {error, Error}
    end;

object_query(<<"allObjects">>, Params, Ctx) ->
    nkdomain_graphql_util:search(Params, Ctx).


%% ===================================================================
%% Queries execute
%% ===================================================================


%% @doc GraphQL execute
-spec object_execute(binary(), #obj_id_ext{}, map(), map(), any()) ->
    {ok, term()} | {error, term()} | unknown.

object_execute(Field, _ObjIdExt, Obj, _Args, _Ctx) ->
    case Field of
        <<"aliases">> -> {ok, maps:get(aliases, Obj, [])};
        <<"createdBy">> -> nkdomain_graphql_util:get_obj(maps:get(created_by, Obj));
        <<"createdById">> -> {ok, maps:get(created_by, Obj)};
        <<"createdTime">> -> {ok, maps:get(created_time, Obj, null)};
        <<"description">> -> {ok, maps:get(description, Obj, null)};
        <<"destroyed">> -> {ok, maps:get(destroyed, Obj, false)};
        <<"destroyedCode">> -> {ok, maps:get(destroyed_code, Obj, null)};
        <<"destroyedReason">> -> {ok, maps:get(destroyed_reason, Obj, null)};
        <<"destroyedTime">> -> {ok, maps:get(destroyed_time, Obj, null)};
        <<"domain">> -> nkdomain_graphql_util:get_obj(maps:get(domain_id, Obj));
        <<"domainId">> -> {ok, maps:get(domain_id, Obj)};
        <<"enabled">> -> {ok, maps:get(enabled, Obj, true)};
        <<"expiresTime">> -> {ok, maps:get(expires_time, Obj, null)};
        <<"icon">> -> nkdomain_graphql_util:get_obj(maps:get(icon_id, Obj, null));
        <<"iconId">> -> {ok, maps:get(icon_id, Obj, null)};
        <<"id">> -> {ok, maps:get(obj_id, Obj)};
        <<"name">> -> {ok, maps:get(name, Obj, null)};
        <<"objId">> -> {ok, maps:get(obj_id, Obj)};
        <<"objName">> -> {ok, maps:get(obj_name, Obj)};
        <<"path">> -> {ok, maps:get(path, Obj)};
        <<"srvId">> -> {ok, maps:get(srv_id, Obj, null)};
        <<"subtypes">> -> {ok, maps:get(subtype, Obj, [])};
        <<"tags">> -> {ok, maps:get(tags, Obj, [])};
        <<"type">> -> nkdomain_graphql_util:get_type(Obj);
        <<"updatedBy">> -> nkdomain_graphql_util:get_obj(maps:get(updated_by, Obj, null));
        <<"updatedById">> -> {ok, maps:get(updated_by, Obj, null)};
        <<"updatedTime">> -> {ok, maps:get(updated_time, Obj, null)};
        <<"vsn">> -> {ok, maps:get(vsn, Obj, null)};
        _ -> unknown
    end.




%% ===================================================================
%% Internal
%% ===================================================================


%% @private
object_fields() ->
    #{
        aliases => {list, string, #{comment => "List of object aliases"}},
        createdBy => {no_null, 'User', #{comment => "User that created the object"}},
        createdById => {no_null, string, #{comment => "UserId that created the object"}},
        createdTime => {no_null, time, #{comment => "Object creation time"}},
        description => {string, #{comment => "User-related description"}},
        destroyed => {boolean, #{comment => "True if the object is destroyed"}},
        destroyedCode => {string, #{comment => "Destruction reason code"}},
        destroyedReason => {string, #{comment => "Destruction reason text"}},
        destroyedTime => {time, #{comment => "Destruction time"}},
        domain => {no_null, 'Domain', #{comment => "Domain this object belongs to"}},
        domainId => {no_null, string, #{comment => "DomainId this object belongs to"}},
        enabled => {no_null, boolean, #{comment => "False if object is disabled"}},
        expiresTime => {time, #{comment => "Time this object will expire"}},
        icon => {'File', #{comment => "Object File icon"}},
        iconId => {string, #{comment => "Object FileId icon"}},
        id => {no_null, id, #{comment=>"Main ID fetcher"}},
        name => {string, #{comment => "Object's user-related name"}},
        objId => {no_null, string, #{comment => "Object's ID"}},
        objName => {no_null, string, #{comment => "Object's short name"}},
        path => {no_null, string, #{comment => "Object's directory path"}},
        srvId => {string, #{comment => "Object's service"}},
        subtypes => {list, string, #{comment => "Object's subtypes"}},
        tags => {list, string, #{comment => "Object's tags"}},
        type => {no_null, objectType, #{comment => "Object's type"}},
        updatedBy => {no_null, 'User', #{comment => "User that updated the object"}},
        updatedById => {no_null, string, #{comment => "UserId that updated the object"}},
        updatedTime => {no_null, time, #{comment => "Object updation time"}},
        vsn => {string, #{comment => "Object's current version"}}
    }.


%% @private
object_fields_filter() ->
    #{
        op => {filterOp, #{comment => "Operation Type"}},
        aliases => {objectFilterKeyword, #{comment => "Object has an alias"}},
        createdById => {objectFilterId, #{comment => "Objects created by this user"}},
        createdTime => {objectFilterInt, #{comment => "Object creation time"}},
        description => {objectFilterText, #{comment => "Words in description"}},
        destroyed => {objectFilterBoolean, #{comment => "Filter by destroyed objects"}},
        destroyedTime => {objectFilterInt, #{comment => "Destruction time"}},
        domainId => {objectFilterId, #{comment => "Filter objects belonging to this domain"}},
        enabled => {objectFilterBoolean, #{comment => "Filter enabled or disabled objects"}},
        expiresTime => {objectFilterInt, #{comment => "Time this object will expire"}},
        hasIcon => {objectFilterBoolean, #{comment => "Objects having an icon"}},
        iconId => {objectFilterId, #{comment => "Objects hanving this iconId"}},
        name => {objectFilterText, #{comment => "Words in name"}},
        objId => {objectFilterId, #{comment => "Object's ID"}},
        objName => {objectFilterKeyword, #{comment => "Object's with this short name"}},
        path => {objectFilterPath, #{comment => "Filter on this path"}},
        srvId => {objectFilterId, #{comment => "Object's service"}},
        subTypes => {list, objectFilterId, #{comment => "Object's subtypes"}},
        tags => {list, objectFilterId, #{comment => "Object's tags"}},
        type => {objectFilterType, #{comment => "Object's type"}},
        updatedById => {objectFilterId, #{comment => "User that updated the object"}},
        updatedTime => {objectFilterInt, #{comment => "Object updation time"}},
        vsn => {objectFilterKeyword, #{comment => "Object's current version"}}
    }.


%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).


