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

-export([is_path/1, get_parts/2, name/1, update/4]).
-export([add_destroyed/3]).
-export([get_service_domain/1]).
-export([error_code/2, add_mandatory/3]).
-export([api_cmd_common/4, api_cmd_get/3, api_cmd_create/3, api_cmd_delete/3, api_cmd_update/3]).
-export([api_search/2, api_getid/3, api_add_id/3]).
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


%% @doc Add mandatory fields to syntax
-spec add_mandatory([atom()], module(), map()) ->
    map().

add_mandatory(Fields, Module, Base) ->
    Fields2 = [list_to_binary([to_bin(Module), $., to_bin(F)]) || F<- Fields],
    Mandatory1 = maps:get('__mandatory', Base, []),
    Mandatory2 = Fields2 ++ Mandatory1,
    Base#{'__mandatory' => Mandatory2}.


%% @doc
-spec update(nkservice:id(), nkdomain:type(), nkdomain:id(), nkdomain_obj:apply_fun()) ->
    {ok, term()} | {error, term()}.

update(Srv, Type, Id, Fun) ->
    case nkdomain:load(Srv, Id, #{}) of
        {ok, Type, _ObjId, _Path, Pid} ->
            case nkdomain_obj:sync_op(Pid, {apply, Fun}) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {ok, _, _, _, _} ->
            {error, invalid_object};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
add_destroyed(SrvId, Reason, Obj) ->
    {Code, Txt} = nkapi_util:api_error(SrvId, Reason),
    Obj2 = maps:remove(active, Obj),
    ?ADD_TO_OBJ(
        #{
            destroyed_time => nklib_util:m_timestamp(),
            destroyed_code => Code,
            destroyed_reason => Txt
        }, Obj2).


%% @doc
api_cmd_common(Type, Cmd, Data, State) ->
    case Cmd of
        get ->
            api_cmd_get(Type, Data, State);
        create ->
            api_cmd_create(Type, Data, State);
        delete ->
            api_cmd_delete(Type, Data, State);
        update ->
            api_cmd_update(Type, Data, State);
        _ ->
            {error, not_implemented, State}
    end.


%% @doc
api_cmd_get(Type, Data, #{srv_id:=SrvId}=State) ->
    case api_getid(Type, Data, State) of
        {ok, Id} ->
            case nkdomain:load(SrvId, Id, #{}) of
                {ok, _Type, _ObjId, _Path, Pid} ->
                    case nkdomain_obj:get_session(Pid) of
                        {ok, #obj_session{obj=Obj}} ->
                            {ok, Obj, State};
                        {error, Error} ->
                            {error, Error, State}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @doc
api_cmd_create(Type, Data, #{srv_id:=SrvId}=State) ->
    case nkdomain:create(SrvId, Data#{type=>Type}, #{}) of
        {ok, _Type, ObjId, Path, _Pid} ->
            {ok, #{obj_id=>ObjId, path=>Path}, State};
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc
api_cmd_delete(Type, Data, #{srv_id:=SrvId}=State) ->
    case api_getid(Type, Data, State) of
        {ok, Id} ->
            Reason = maps:get(reason, Data, api_delete),
            case nkdomain:load(SrvId, Id) of
                {ok, _Type, ObjId, _Path, _Pid} ->
                    case nkdomain_obj:delete(ObjId, Reason) of
                        ok ->
                            {ok, #{}, State};
                        {error, Error} ->
                            {error, Error, State}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @doc
api_cmd_update(Type, Data, #{srv_id:=SrvId}=State) ->
    case api_getid(Type, Data, State) of
        {ok, Id} ->
            case nkdomain:load(SrvId, Id) of
                {ok, _Type, ObjId, _Path, _Pid} ->
                    case nkdomain_obj:update(ObjId, Data) of
                        ok ->
                            {ok, #{}, State};
                        {error, Error} ->
                            {error, Error, State}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end.


%% @doc
api_search({ok, Total, List}, State) ->
    Data = #{
        total => Total,
        data =>
            lists:map(
                fun({Type, ObjId, Path}) -> #{type=>Type, obj_id=>ObjId, path=>Path} end,
                List)
    },
    {ok, Data, State};

api_search({error, Error}, State) ->
    {error, Error, State}.


%% @doc
api_getid(_Type, #{id:=Id}, _State) ->
    {ok, Id};

api_getid(Type, _Data, #{nkdomain_obj_ids:=ObjIds}=State) ->
    case maps:find(Type, ObjIds) of
        {ok, Id} ->
            {ok, Id};
        error ->
            {error, missing_id, State}
    end;

api_getid(_Type, _Data, State) ->
    {error, missing_id, State}.


%% @doc Adds 'logged in' information to the state
api_add_id(Type, Id, State) ->
    ObjIds1 = maps:get(nkdomain_obj_ids, State, #{}),
    ObjIds2 = ObjIds1#{Type => Id},
    ?ADD_TO_API_SESSION(nkdomain_obj_ids, ObjIds2, State).




%% @private
to_bin(Term) -> nklib_util:to_binary(Term).

