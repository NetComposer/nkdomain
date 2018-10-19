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

-module(nkdomain_openapi_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([filter_parameters/4, sort_parameter/1, filter_parameter/1]).
-export([ok_response/1, created_response/1, updated_response/1,
         list_response/0, binary_response/0]).
-export([make_ref/3, make_ref/4]).
-export([ref_schema/1, ref_parameter/1, ref_response/1, unident_descriptions/1]).
-export([get_apis_tags/1]).

-include("nkdomain.hrl").

%% ===================================================================
%% Utilities
%% ===================================================================


%% @doc Gets allowed filter and sort fields starting with 'spec'
filter_parameters(SrvId, Group, _Vsn, Type) ->
    {ok, Config} = nkdomain_actor_util:get_config(SrvId, Group, Type),
    #{filter_fields:=Filter, sort_fields:=Sort} = Config,
    Filter2 = [F || F <- Filter, is_filter_param(F)],
    Sort2 = [F || F <- Sort, is_filter_param(F)],
    {Filter2, Sort2}.


%% @private
is_filter_param(<<"spec.", _/binary>>) -> true;
is_filter_param(<<"status.", _/binary>>) -> true;
is_filter_param(_) -> false.


%% @doc Generates a 'sort' parameter
sort_parameter(Fields) ->
    Fields2 = lists:usort([
        <<"metadata.name">>,
        <<"metadata.domain">>,
        <<"metadata.subtype">>,
        <<"metadata.creationTime">>,
        <<"metadata.updateTime">>,
        <<"metadata.expiresTime">>,
        <<"metadata.isEnabled">>,
        <<"metadata.isInAlarm">>
        | Fields
    ]),
    Fields3 = << <<"            * ", Field/binary, "\n">>  || Field <- Fields2>>,
    #{
        name => sort,
        description => <<"
            Fields to sort on. It can be a field or a list fields. Default order is 'asc',
            it can be switched to 'desc' as a field prefix. Allowed to be used
            as sort indices are:\n\n",
            Fields3/binary, "

            **Example**: `sort=metadata.subtype,desc:metadata.creationTime`
            Default is 'desc:metadata.creationTime'
            ">>,
        in => query,
        schema=> #{
            type => string
        }
    }.


%% @doc Generates a 'fieldSelector' parameter
filter_parameter(Fields) ->
    Fields2 = lists:usort([
        <<"metadata.uid">>,
        <<"metadata.name">>,
        <<"metadata.domain">>,
        <<"metadata.subtype">>,
        <<"metadata.resourceVersion">>,
        <<"metadata.generation (integer)">>,
        <<"metadata.creationTime">>,
        <<"metadata.updateTime">>,
        <<"metadata.expiresTime">>,
        <<"metadata.isEnabled (boolean)">>,
        <<"metadata.isInAlarm (boolean)">>
        | Fields
    ]),
    Fields3 = << <<"            * ", Field/binary, "\n">>  || Field <- Fields2>>,
    #{
        name => fieldSelector,
        description => <<"
            Fields to filter on. It can be a field or a list fields. It can include a value or
            not (and it would find actors having this label with any value).
            Allowed fields to be used as sort indices are:\n\n",
            Fields3/binary, "

            **Example**: `filterSelector=metadata.name:name1,metadata.domain:domain1`

            A small query language is available supporting the following operations:

            * eq
            * ne
            * gt
            * gte
            * lt
            * lte
            * exits
            * prefix

            **Example**: `filterSelector=metadata.generarion:gte:1,metadata.subtype`
            ">>,
        in => query,
        schema => #{
            type => string
        }
    }.


%% @private
ok_response(Schema) ->
    Base = error_responses(),
    Base#{
        '200' => #{
            description => <<"OK">>,
            content => #{
                'application/json' => #{
                    schema => ref_schema(Schema)
                }
            }
        }
    }.


%% @private
created_response(Schema) ->
    Base = error_responses(),
    Base#{
        '201' => #{
            description => <<"Created">>,
            content => #{
                'application/json' => #{
                    schema => ref_schema(Schema)
                }
            }
        }
    }.


%% @private
updated_response(Schema) ->
    maps:merge(ok_response(Schema), created_response(Schema)).


%% @private
list_response() ->
    Base = error_responses(),
    Base #{
        '200' => #{
            description => <<"OK">>,
            content => #{
                'application/json' => #{
                    schema => #{
                        oneOf => [
                            ref_schema("common.v1.ListMeta"),
                            ref_schema("common.v1.Status")
                        ]
                    }
                }
            }
        }
    }.


%% @private
binary_response() ->
    Base = error_responses(),
    Base#{
        '200' => #{
            description => <<"OK">>,
            content => #{
                '*/*' => #{
                    schema => #{
                        type => string,
                        format => binary
                    }
                }
            }
        }
    }.


%% @private
error_responses() ->
    #{
        '400' => ref_response("common.v1.Response400"),
        '401' => ref_response("common.v1.Response401"),
        '404' => ref_response("common.v1.Response404"),
        '405' => ref_response("common.v1.Response405"),
        '409' => ref_response("common.v1.Response409"),
        '422' => ref_response("common.v1.Response422"),
        '429' => ref_response("common.v1.Response429"),
        '500' => ref_response("common.v1.Response500")
    }.


%% @private
ref_schema(Id) ->
    #{
       '$ref' => <<"#/components/schemas/io.netc.api.", (list_to_binary([Id]))/binary>>
    }.


%% @private
ref_parameter(Id) ->
    #{
        '$ref' => <<"#/components/parameters/io.netc.api.", (list_to_binary([Id]))/binary>>
    }.


%% @private
ref_response(Id) ->
    #{
        '$ref' => <<"#/components/responses/io.netc.api.", (list_to_binary([Id]))/binary>>
    }.


%% @private
make_ref(Group, Vsn, Kind) ->
    list_to_binary([<<"io.netc.api.">>, Group, $., Vsn, $., Kind]).


%% @private
make_ref(Group, Vsn, Kind, SubKind) ->
    list_to_binary([make_ref(Group, Vsn, Kind), SubKind]).


%% @doc
unident_descriptions(Map) ->
    List1 = maps:to_list(Map),
    List2 = lists:map(
        fun({Id, Data}) ->
            Data2 = case Id of
                description when is_list(Data); is_binary(Data) ->
                    unident(Data);
                _ when is_map(Data) ->
                    unident_descriptions(Data);
                _ ->
                    Data
            end,
            {Id, Data2}
        end,
        List1),
    maps:from_list(List2).


%% @doc Takes the first line of a binary with spaces and removes
%% that number of spaces of all lines
unident(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [global]),
    case unident_find_spaces1(Lines) of
        0 ->
            Binary;
        Spaces ->
            Lines2 = [
                case byte_size(Line) of
                    Size when Size >= Spaces ->
                        binary_part(Line, Spaces, Size-Spaces);
                    _ ->
                        Line
                end
                || Line <- Lines
            ],
            nklib_util:bjoin(Lines2, <<"\n">>)
    end.


%% @private
unident_find_spaces1([<<>>|Rest]) ->
    unident_find_spaces1(Rest);

unident_find_spaces1([<<32, Rest/binary>>|_]) ->
    unident_find_spaces2(Rest, 1);

unident_find_spaces1(_) ->
    0.


%% @private
unident_find_spaces2(<<32, Rest/binary>>, Count) ->
    unident_find_spaces2(Rest, Count+1);

unident_find_spaces2(_, Count) ->
    Count.


%% @private
get_apis_tags(SrvId) ->
    Groups = nkdomain_lib:nkdomain_get_api_groups(SrvId),
    get_apis_tags(maps:to_list(Groups), [], [], []).


%% @private
get_apis_tags([], ApisA, ApisB, Tags) ->
    {lists:usort(ApisA), lists:usort(ApisB), lists:usort(Tags)};

get_apis_tags([{Group, VsnList}|Rest], ApisA, ApisB, Tags) ->
    {ApisA2, ApisB2, Tags2} = get_api_tags_versions(Group, VsnList, ApisA, ApisB, Tags),
    get_apis_tags(Rest, ApisA2, ApisB2, Tags2).


%% @private
get_api_tags_versions(_Name, [], ApisA, ApisB, Tags) ->
    {ApisA, ApisB, Tags};


get_api_tags_versions(Name, [Vsn|Rest], ApisA, ApisB, Tags) ->
    ApisA2 = [<<"/apis/", Name/binary>> | ApisA],
    ApisB2 = [<<"/apis/", Name/binary, $/, Vsn/binary>> | ApisB],
    Tags2 = [Name, <<Name/binary, $_, Vsn/binary>> | Tags],
    get_api_tags_versions(Name, Rest, ApisA2, ApisB2, Tags2).



%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).
