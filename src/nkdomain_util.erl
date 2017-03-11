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

-export([is_path/1, get_parts/2, error_code/2, add_mandatory/3]).
-export_type([error/0]).

-type error() ::
    atom() |
    {atom(), term()} |
    {atom(), list(), list()}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Normalizes a path
%% Valid paths either start with / or has '@' or '.'
-spec is_path(list()|binary()) ->
    {true, nkdomain:path()} | false.

is_path(Path) when is_list(Path) ->
    is_path(list_to_binary(Path));

is_path(<<"/", _/binary>>=Path) ->
    {true, Path};
is_path(Path) ->
    case binary:split(nklib_util:to_binary(Path), <<"@">>) of
        [Name, Path1] ->
            Path2 = binary:split(Path1, <<".">>, [global]),
            Path3 = nklib_util:bjoin(lists:reverse(Path2), <<"/">>),
            {true, <<"/", Path3/binary, "/", Name/binary>>};
        [Path1] ->
            case binary:split(Path1, <<".">>, [global]) of
                [_] ->
                    false;
                Path2 ->
                    Path3 = nklib_util:bjoin(lists:reverse(Path2), <<"/">>),
                    {true, <<"/", Path3/binary>>}
            end
    end.


%% @doc
%% /domain/users/user1 -> {ok, <<"/domain">>, <<"users/user1">>
-spec get_parts(module(), nkdomain:path()) ->
    {ok, Base::nkdomain:path(), Name::binary()} | {error, term()}.

get_parts(Module, Path) ->
    case is_path(Path) of
        {true, Path2} ->
            case lists:reverse(binary:split(Path2, <<"/">>, [global])) of
                [<<>>|_] ->
                    {error, invalid_name};
                [ObjName|Parts] when Module==nkdomain_domain ->
                    case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                        <<>> ->
                            {ok, <<"/">>, ObjName};
                        Base ->
                            {ok, Base, ObjName}
                    end;
                [ObjName, Class|Parts] ->
                    case catch Module:object_get_desc() of
                        #{type:=Type} ->
                            case <<Type/binary, "s">> of
                                 Class ->
                                    case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                                        <<>> ->
                                            {ok, <<"/">>, <<Class/binary, $/, ObjName/binary>>};
                                        Base ->
                                            {ok, Base, <<Class/binary, $/, ObjName/binary>>}
                                    end;
                                _ ->
                                    {error, invalid_path}
                            end;
                        _ ->
                            {error, invalid_path}
                    end;
                _ ->
                    {error, invalid_path}
            end;
        false ->
            {error, invalid_path}
    end.



%% @doc
-spec error_code(nkservice:id(), error()) ->
    {atom(), binary()}.

error_code(_SrvId, Term) when is_atom(Term) ->
    {Term, to_bin(Term)};

error_code(_SrvId, {Key, Term}) when is_atom(Key) ->
    {Key, list_to_binary([to_bin(Key), <<": ">>, to_bin(Term)])};

error_code(_SrvId, {Key, Fmt, Args}) when is_atom(Key), is_list(Fmt), is_list(Args) ->
    try list_to_binary(io_lib:format(Fmt, Args)) of
        Bin ->
            {Key, Bin}
    catch
        _:_ ->
            Ref = make_ref(),
            lager:warning("Invalid domain error_code: ~p, ~p (~p)", [Fmt, Args, Ref]),
            {Key, to_bin(erlang:ref_to_list(Ref))}
    end;

error_code(_SrvId, Term) ->
    Ref = make_ref(),
    lager:warning("Invalid domain error_code: ~p (~p)", [Term, Ref]),
    {unknown_error, to_bin(erlang:ref_to_list(Ref))}.


%% @doc Add mandatory fields to syntax
-spec add_mandatory([atom()], module(), map()) ->
    map().

add_mandatory(Fields, Module, Base) ->
    Fields2 = [list_to_binary([to_bin(Module), $., to_bin(F)]) || F<- Fields],
    Mandatory1 = maps:get('__mandatory', Base, []),
    Mandatory2 = Fields2 ++ Mandatory1,
    Base#{'__mandatory' => Mandatory2}.


%% @private
to_bin(Term) -> nklib_util:to_binary(Term).