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

-export([error_code/2, add_mandatory/3]).
-export_type([error/0]).

-type error() ::
    atom() |
    {atom(), term()} |
    {atom(), list(), list()}.


%% ===================================================================
%% Public
%% ===================================================================

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