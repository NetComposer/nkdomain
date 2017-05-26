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

-export([is_path/1, get_parts/2, class/1, name/1, update/4]).
-export([add_destroyed/3]).
-export([get_service_domain/1]).
-export([error_code/2]).
-export([timestamp/0]).
-export_type([error/0]).

-type error() ::
    atom() |
    {atom(), term()} |
    {atom(), list(), list()}.

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").



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
        _ ->
            false
%%        [Path1] ->
%%            case binary:split(Path1, <<".">>, [global]) of
%%                [_] ->
%%                    false;
%%                Path2 ->
%%                    Path3 = nklib_util:bjoin(lists:reverse(Path2), <<"/">>),
%%                    {true, <<"/", Path3/binary>>}
%%            end
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
                    {error, {invalid_object_path, Path2}};
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
                            {error, {invalid_object_path, Path2}}
                    end;
                _ ->
                    {error, {invalid_object_path, Path2}}
            end;
        false ->
            {error, {invalid_object_path, to_bin(Path)}}
    end;

get_parts(Type, Path) ->
    get_parts(to_bin(Type), Path).


%% @private
class(?DOMAIN_DOMAIN) ->
    <<>>;
class(Type) ->
    <<Type/binary, "s">>.


%% @private
name(Name) ->
    nklib_parse:normalize(Name, #{space=>$_, allowed=>[$-, $., $_]}).


%% @doc Finds the domain for a service
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


%% @doc
-spec update(nkservice:id(), nkdomain:type(), nkdomain:id(), nkdomain_obj:apply_fun()) ->
    {ok, term()} | {error, term()}.

update(Srv, Type, Id, Fun) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{type=Type, pid=Pid} ->
            case nkdomain_obj:sync_op(Pid, {apply, Fun}) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        #obj_id_ext{} ->
            {error, invalid_object};
        {error, Error} ->
            {error, Error}
    end.


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

