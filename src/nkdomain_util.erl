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

-module(nkdomain_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([make_path/3, is_path/1, get_parts/1, get_parts/2, class/1, type/1, name/1, field/2, fields/2]).
-export([s_type/1]).
-export([append/2, get_srv_id/1, add_destroyed/3]).
-export([timestamp/0]).
-export_type([error/0]).

-type error() ::
    atom() |
    {atom(), term()} |
    {atom(), list(), list()}.

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").

-define(ICON_TYPES, [<<"image/jpeg">>, <<"image/png">>, <<"image/gif">>]).

%% ===================================================================
%% Public
%% ===================================================================

%% @doc 
make_path(Base, Type, ObjName) ->
    Class = case to_bin(Type) of
        ?DOMAIN_DOMAIN ->
            <<>>;
        _ ->
            class(Type)
    end,
    case Base of
        <<"/">> ->
            <<$/, Class/binary, $/, ObjName/binary>>;
        _ ->
            <<Base/binary, $/, Class/binary, $/, ObjName/binary>>
    end.


%% @doc Normalizes a path
%% Valid paths either start with / or has '@' or '.'
-spec is_path(list()|binary()) ->
    {true|false, nkdomain:path()}.

is_path(<<"/", _/binary>>=Path) ->
    {true, Path};

is_path(Path) when is_binary(Path) ->
    case binary:split(Path, <<"@">>) of
        [Name, Path1] ->
            Path2 = binary:split(Path1, <<".">>, [global]),
            Path3 = nklib_util:bjoin(lists:reverse(Path2), <<"/">>),
            {true, <<"/", Path3/binary, "/", Name/binary>>};
        _ ->
            {false, Path}
    end;

is_path(Path) ->
    is_path(to_bin(Path)).



%% @doc
%% /domain/users/user1 -> {ok, <<"/domain">>, <<"user1">>}
-spec get_parts(nkdomain:path()) ->
    {ok, Base::nkdomain:path(), Type::binary(), Name::binary()} | {error, term()}.

get_parts(Path) ->
    case is_path(Path) of
        {true, Path2} ->
            case lists:reverse(binary:split(Path2, <<"/">>, [global])) of
                [<<>>, <<>>] ->
                    {ok, <<>>, ?DOMAIN_DOMAIN, <<>>};
                [<<>>|_] ->
                    {error, {object_path_invalid, Path2}};
                [ObjName, Class|Parts] ->
                    ObjName2 = name(ObjName),
                    case type(Class) of
                        {ok, Type} ->
                            case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                                <<>> ->
                                    {ok, <<"/">>, Type, ObjName2};
                                Base ->
                                    {ok, Base, Type, ObjName2}
                            end;
                        error ->
                            case nklib_util:bjoin(lists:reverse([Class|Parts]), <<"/">>) of
                                <<>> ->
                                    {ok, <<"/">>, ?DOMAIN_DOMAIN, ObjName2};
                                Base ->
                                    {ok, Base, ?DOMAIN_DOMAIN, ObjName2}
                            end
                    end;
                _ ->
                    {error, {object_path_invalid, Path2}}
            end;
        {false, Other} ->
            {error, {object_path_invalid, Other}}
    end.



%% @doc
%% /domain/users/user1 -> {ok, <<"/domain">>, <<"user1">>}
-spec get_parts(nkdomain:type(), nkdomain:path()) ->
    {ok, Base::nkdomain:path(), Name::binary()} | {error, term()}.

get_parts(?DOMAIN_DOMAIN, <<"/">>) ->
    {ok, <<"/">>, <<>>};

get_parts(Type, Path) when is_binary(Type) ->
    case is_path(Path) of
        {true, Path2} ->
            case lists:reverse(binary:split(Path2, <<"/">>, [global])) of
                [<<>>|_] ->
                    {error, {object_path_invalid, Path2}};
                [ObjName|Parts] when Type==?DOMAIN_DOMAIN ->
                    ObjName2 = name(ObjName),
                    case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                        <<>> ->
                            {ok, <<"/">>, ObjName2};
                        Base ->
                            {ok, Base, ObjName2}
                    end;
                [ObjName, Class|Parts] ->
                    case class(Type) of
                         Class ->
                            case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                                <<>> ->
                                    {ok, <<"/">>, name(ObjName)};
                                Base ->
                                    {ok, Base, name(ObjName)}
                            end;
                        _ ->
                            {error, {object_path_invalid, Path2}}
                    end;
                _ ->
                    {error, {object_path_invalid, Path2}}
            end;
        {false, Other} ->
            {error, {object_path_invalid, Other}}
    end;

get_parts(Type, Path) ->
    get_parts(to_bin(Type), Path).


%% @private
class(Type) ->
    Type2 = to_bin(Type),
    Size = byte_size(Type2),
    case binary:at(Type2, Size-1) of
        $y ->
            <<Type2:(Size-1)/binary, "ies">>;
        _ ->
            <<Type2/binary, "s">>
    end.


%% @private
type(Class) ->
    Size = byte_size(Class),
    case Size > 3 andalso binary:part(Class, Size, -3) of
        <<"ies">> ->
            {ok, <<(binary:part(Class, 0, byte_size(Class)-3))/binary, $y>>};
        _ when Size > 1 ->
            case binary:part(Class, Size, -1) of
                <<"s">> ->
                    {ok, binary:part(Class, 0, byte_size(Class)-1)};
                _ ->
                    error
            end;
        _ ->
            error
    end.


%% @private
name(Name) ->
    nklib_parse:normalize(Name, #{space=>$_, allowed=>[$+, $-, $., $_]}).


%% @private
get_srv_id(Obj) ->
    Srv = case Obj of
        #{srv_id:=SrvId0} -> SrvId0;
        #{<<"srv_id">>:=SrvId0} -> SrvId0
    end,
    case catch nklib_util:to_existing_atom(Srv) of
        {'EXIT', _} ->
            lager:warning("NkDOMAIN: loading object for unknown domain '~s'", [Srv]),
            ?NKROOT;
        SrvId ->
            SrvId
    end.


%% @private A Type with uppercase in the first letter
s_type(Type) ->
    <<First, Rest/binary>> = to_bin(Type),
    <<(nklib_util:to_upper(<<First>>))/binary, Rest/binary>>.


%% @doc
append(PathA, PathB) ->
    PathA1 = case to_bin(PathA) of
        <<"/">> -> <<>>;
        P1 -> P1
    end,
    PathB2 = case to_bin(PathB) of
        <<"/", Rest/binary>> -> Rest;
        P2 -> P2
    end,
    <<PathA1/binary, $/, PathB2/binary>>.


%% @doc
field(Type, Field) ->
    list_to_binary([Type, $., Field]).

%% @doc
fields(Type, Fields) ->
    [field(Type, Field) || Field <- Fields].


%% @doc
add_destroyed(SrvId, Reason, Obj) ->
    {Code, Txt} = nkservice_util:error(SrvId, Reason),
    Obj2 = maps:remove(active, Obj),
    ?ADD_TO_OBJ(
        #{
            destroyed_time => nkdomain_util:timestamp(),
            destroyed_code => Code,
            destroyed_reason => Txt
        }, Obj2).




%% @private
%%timestamp() ->
%%    R = 1494109379823,
%%    (nklib_util:m_timestamp() -R + 1490000000000) div 1000 * 1000.

%% @private
timestamp() ->
    nklib_util:m_timestamp().



%% @private
to_bin(Term) -> nklib_util:to_binary(Term).

