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

-export([is_path/1, get_parts/2, name/1]).
-export([get_service_domain/1]).
-export([error_code/2, add_mandatory/3]).
-export([api_common/4, api_create/3, api_delete/2, api_update/2]).
-export([search_api/2]).
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
-spec get_parts(nkdomain:type(), nkdomain:path()) ->
    {ok, Base::nkdomain:path(), Name::binary()} | {error, term()}.

get_parts(<<"domain">>, <<"/">>) ->
    {ok, <<"/">>, <<>>};

get_parts(Type, Path) ->
    case is_path(Path) of
        {true, Path2} ->
            case lists:reverse(binary:split(Path2, <<"/">>, [global])) of
                [<<>>|_] ->
                    {error, {invalid_object_path, Path2}};
                [ObjName|Parts] when Type==<<"domain">> ->
                    ObjName2 = name(ObjName),
                    case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                        <<>> ->
                            {ok, <<"/">>, ObjName2};
                        Base ->
                            {ok, Base, ObjName2}
                    end;
                [ObjName, Class|Parts] ->
                    ObjName2 = name(ObjName),
                    case <<Type/binary, "s">> of
                         Class ->
                            case nklib_util:bjoin(lists:reverse(Parts), <<"/">>) of
                                <<>> ->
                                    {ok, <<"/">>, <<Class/binary, $/, ObjName2/binary>>};
                                Base ->
                                    {ok, Base, <<Class/binary, $/, ObjName2/binary>>}
                            end;
                        _ ->
                            {error, {invalid_object_path, Path2}}
                    end;
                _ ->
                    {error, {invalid_object_path, Path2}}
            end;
        false ->
            {error, {invalid_object_path, to_bin(Path)}}
    end.


%% @private
name(Name) ->
    nklib_parse:normalize(Name, #{space=>$-, allowed=>[$-]}).


%% @doc
get_service_domain(Srv) ->
    case nkservice:get(Srv, nkdomain_data) of
        #{domain_obj_id:=Domain} -> Domain;
        _ -> undefined
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


%% @doc
api_common(Type, Cmd, Data, State) ->
    case Cmd of
        create ->
            api_create(Type, Data, State);
        delete ->
            api_delete(Data, State);
        update ->
            api_update(Data, State);
        _ ->
            {error, not_implemented, State}
    end.


%% @doc
api_create(Type, Data, #{srv_id:=SrvId}=State) ->
    case nkdomain_obj:create(SrvId, Data#{type=>Type}, #{}) of
        {ok, ObjId, _Pid} ->
            {ok, #{obj_id=>ObjId}, State};
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc
api_delete(Data, #{srv_id:=SrvId, user_id:=UserId}=State) ->
    Id = maps:get(id, Data, UserId),
    Reason = maps:get(reason, Data, api_delete),
    case nkdomain_obj:load(SrvId, Id) of
        {ok, _Type, ObjId, _Path, _Pid} ->
            case nkdomain_obj:delete(ObjId, Reason) of
                ok ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc
api_update(Data, #{srv_id:=SrvId, user_id:=UserId}=State) ->
    Id = maps:get(id, Data, UserId),
    case nkdomain_obj:load(SrvId, Id) of
        {ok, _Type, ObjId, _Path, _Pid} ->
            case nkdomain_obj:update(ObjId, Data) of
                ok ->
                    {ok, #{}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc
search_api({ok, Total, List}, State) ->
    Data = #{
        total => Total,
        data =>
            lists:map(
                fun({Type, ObjId, Path}) -> #{type=>Type, obj_id=>ObjId, path=>Path} end,
                List)
    },
    {ok, Data, State};

search_api({error, Error}, State) ->
    {error, Error, State}.



%% @private
to_bin(Term) -> nklib_util:to_binary(Term).

