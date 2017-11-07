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
    {ok, Previous}.





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
        sortOrder => #{
            opts => [asc, desc]
        },
        'RangeOperators' => #{
            opts => [gt, gte, eq, lte, lt],
            comment => "Range operators for queries"
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
                totalCount => int,
                cursor => 'Cursor'
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
        'Range' => #{
            fields => #{
                value => int,
                min => int,
                max => int,
                operator => 'RangeOperators'
            },
            comment => "A value to filter against, or a min and a max value"
        },
        objectFilterId => #{
            fields => #{
                eq => string
            }
        },
        objectFilterString => #{
            fields => #{
                eq => string,
                gt => string,
                gte => string,
                lt => string,
                lte => string,
                prefix => string
            }
        },
        objectFilterInt => #{
            fields => #{
                eq => int,
                gt => int,
                gte => int,
                lt => int,
                lte => int
            }
        },
        objectFilterBoolean => #{
            fields => #{
                eq => boolean
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


object_query(<<"node">>, #{<<"id">>:=Id}, _Ctx) ->
    case nkdomain_lib:read(Id) of
        {ok, #obj_id_ext{}=ObjIdExt, Obj} ->
            {ok, {ObjIdExt, Obj}};
        {error, Error} ->
            {error, Error}
    end;

object_query(<<"allObjects">>, Params, _Ctx) ->
    #{
        <<"from">> := From,
        <<"size">> := Size,
        <<"filter">> := _Filter,
        <<"sort">> := PSort
    } = Params,
    lager:error("NKLOG Params ~p", [Params]),
    Spec1 = #{
        from => From,
        size => Size
    },
    Spec2 = case PSort of
        null ->
            Spec1;
        _ ->
            Spec1#{sort=> [
                <<Order/binary, $:, (to_bin(camel_to_erl(Field)))/binary>>
                || #{<<"field">>:={enum, Field}, <<"sortOrder">>:={enum, Order}} <- PSort
            ]}

    end,
    lager:error("Spec1: ~p", [Spec2]),


    case nkdomain:search(Spec2#{fields=>[]}) of
        {ok, Total, Data, _Meta} ->
            Data2 = lists:foldl(
                fun(#{<<"obj_id">>:=ObjId}, Acc) ->
                    {ok, ObjIdExt, Obj} = nkdomain_lib:read(ObjId),
                    % lager:error("NKLOG OBJ ~p", [Obj]),
                    [{ObjIdExt, Obj}|Acc]
                end,
                [],
                Data),
            Result = #search_results{
                objects = Data2,
                total_count = Total,
                page_info = #page_info{
                    has_next_page = false,
                    has_previous_page = false
                },
                cursor = <<>>
            },
            {ok, Result};
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% Queries execute
%% ===================================================================


%% @doc GraphQL execute
-spec object_execute(binary(), #obj_id_ext{}, map(), map(), any()) ->
    {ok, term()} | {error, term()} | unknown.

object_execute(Field, _ObjIdExt, Obj, _Args, _Ctx) ->
    case Field of
        <<"aliases">> -> {ok, maps:get(aliases, Obj, [])};
        <<"createdBy">> -> get_obj(maps:get(created_by, Obj));
        <<"createdById">> -> {ok, maps:get(created_by, Obj)};
        <<"createdTime">> -> {ok, maps:get(created_time, Obj, null)};
        <<"description">> -> {ok, maps:get(description, Obj, null)};
        <<"destroyed">> -> {ok, maps:get(destroyed, Obj, false)};
        <<"destroyedCode">> -> {ok, maps:get(destroyed_code, Obj, null)};
        <<"destroyedReason">> -> {ok, maps:get(destroyed_reason, Obj, null)};
        <<"destroyedTime">> -> {ok, maps:get(destroyed_time, Obj, null)};
        <<"domain">> -> get_obj(case maps:get(domain_id, Obj) of <<>> -> <<"root">>; O -> O end);
        <<"domainId">> -> {ok, maps:get(domain_id, Obj)};
        <<"enabled">> -> {ok, maps:get(enabled, Obj, true)};
        <<"expiresTime">> -> {ok, maps:get(expires_time, Obj, null)};
        <<"icon">> -> get_obj(maps:get(icon_id, Obj, null));
        <<"iconId">> -> {ok, maps:get(icon_id, Obj, null)};
        <<"id">> -> {ok, maps:get(obj_id, Obj)};
        <<"name">> -> {ok, maps:get(name, Obj, null)};
        <<"objId">> -> {ok, maps:get(obj_id, Obj)};
        <<"objName">> -> {ok, maps:get(obj_name, Obj)};
        <<"path">> -> {ok, maps:get(path, Obj)};
        <<"srvId">> -> {ok, maps:get(srv_id, Obj, null)};
        <<"subtypes">> -> {ok, maps:get(subtype, Obj, [])};
        <<"tags">> -> {ok, maps:get(tags, Obj, [])};
        <<"type">> -> get_type(Obj);
        <<"updatedBy">> -> get_obj(maps:get(updated_by, Obj, null));
        <<"updatedById">> -> {ok, maps:get(updated_by, Obj, null)};
        <<"updatedTime">> -> {ok, maps:get(updated_time, Obj, null)};
        <<"vsn">> -> {ok, maps:get(vsn, Obj, null)};
        _ -> unknown
    end.


%% @private
get_obj(null) ->
    {ok, null};
get_obj(ObjId) ->
    case nkdomain_lib:read(ObjId) of
        {ok, #obj_id_ext{}=ObjIdExt, Obj} ->
            {ok, {ObjIdExt, Obj}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_type(#{type:=Type}) ->
    Module = nkdomain_reg:get_type_module(Type),
    case Module:object_info() of
        #{schema_type:=SchemaType} ->
            {ok, nklib_util:to_binary(SchemaType)};
        _ ->
            lager:error("NKLOG Unknown type ~p", [Type]),
            {error, unknown_type}
    end;

get_type(Obj) ->
    lager:error("NKLOG Unknown type ~p", [Obj]),
    {error, unknown_type}.


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
        aliases => {objectFilterString, #{comment => "Object has an alias"}},
        createdById => {objectFilterId, #{comment => "Objects created by this user"}},
        createdTime => {objectFilterInt, #{comment => "Object creation time"}},
        description => {objectFilterString, #{comment => "Words in description"}},
        destroyed => {objectFilterBoolean, #{comment => "Filter by destroyed objects"}},
        destroyedTime => {objectFilterInt, #{comment => "Destruction time"}},
        domainId => {objectFilterId, #{comment => "Filter objects belonging to this domain"}},
        enabled => {objectFilterBoolean, #{comment => "Filter enabled or disabled objects"}},
        expiresTime => {objectFilterInt, #{comment => "Time this object will expire"}},
        has_icon => {objectFilterBoolean, #{comment => "Objects having an icon"}},
        iconId => {objectFilterId, #{comment => "Objects hanving this iconId"}},
        name => {objectFilterString, #{comment => "Words in name"}},
        objId => {objectFilterId, #{comment => "Object's ID"}},
        objName => {objectFilterString, #{comment => "Object's with this short name"}},
        path => {objectFilterString, #{comment => "Filter on this path"}},
        srvId => {objectFilterId, #{comment => "Object's service"}},
        subtypes => {list, objectFilterId, #{comment => "Object's subtypes"}},
        tags => {list, objectFilterId, #{comment => "Object's tags"}},
        type => {objectFilterId, #{comment => "Object's type"}},
        updatedById => {objectFilterId, #{comment => "User that updated the object"}},
        updatedTime => {objectFilterInt, #{comment => "Object updation time"}},
        vsn => {objectFilterString, #{comment => "Object's current version"}}
    }.


%% @private
camel_to_erl(<<"createdById">>) -> created_by;
camel_to_erl(<<"createdTime">>) -> created_time;
camel_to_erl(<<"destroyedCode">>) -> destroyed_code;
camel_to_erl(<<"destroyedReason">>) -> destroyed_reason;
camel_to_erl(<<"destroyedTime">>) -> destroyed_time;
camel_to_erl(<<"domainId">>) -> domain_id;
camel_to_erl(<<"expiresTime">>) -> expires_time;
camel_to_erl(<<"iconId">>) -> icon_id;
camel_to_erl(<<"objId">>) -> obj_id;
camel_to_erl(<<"objName">>) -> obj_name;
camel_to_erl(<<"srvId">>) -> srv_id;
camel_to_erl(<<"updatedById">>) -> updated_by;
camel_to_erl(<<"updatedTime">>) -> updated_time;
camel_to_erl(Erl) -> binary_to_existing_atom(Erl, utf8).



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).


