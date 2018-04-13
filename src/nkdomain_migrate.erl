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
-module(nkdomain_migrate).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').


-export([import_7_to_8/1, print/1]).
-export([import_8_to_9/1]).
-export([move_users_from_to/2]).
-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN migrate: "++Txt, Args)).


%% ===================================================================
%% 8 to 9
%% ===================================================================


import_8_to_9(Path) ->
    Fun = fun(Obj) ->
        case Obj of
            #{<<"obj_id">>:=<<"root">>} ->
                continue;
            #{<<"obj_id">>:=<<"admin">>} ->
                continue;
            #{<<"path">>:=<<"/sipstorm_c4">>} ->
                {upgrade, Obj};
            #{<<"path">>:=<<"/sphera">>} ->
                {upgrade, Obj};
            _ ->
                Obj2 = maps:remove(<<"srv_id">>, Obj),
                {upgrade, Obj2}
        end
    end,
    nkdomain_store_es_search:import_objects(<<"nkobjects_v8">>, Path, Fun).





%% ===================================================================
%% 7 to 8
%% ===================================================================


%% @doc
import_7_to_8(Path) ->
    Fun = fun(Obj) ->
        case Obj of
            #{?DOMAIN_USER := User} ->
                User2 = import_7_to_8_user(User),
                {upgrade, Obj#{?DOMAIN_USER:=User2}};
            #{<<"conversation">> := #{<<"push_app_id">>:=AppId}=Conv} ->
                Conv2 = maps:remove(<<"push_app_id">>, Conv),
                {upgrade, Obj#{<<"conversation">>:=Conv2#{<<"push_srv_id">>=>AppId}}};
            _ ->
                {upgrade, Obj}
        end
    end,
    nkdomain_store_es_search:import_objects(<<"nkobjects_v7">>, Path, Fun).


%% @private
import_7_to_8_user(User) ->
    Push = lists:map(
        fun(P) ->
            case maps:take(<<"app_id">>, P) of
                {<<"sphera_collab">>, P2} ->
                    P2#{<<"srv_id">> => <<"sphera_telemed">>};
                {Key, P2} ->
                    P2#{<<"srv_id">> => Key};
                error ->
                    P
            end
        end,
        maps:get(<<"push">>, User, [])),
    Status = lists:map(
        fun(S) ->
            case maps:take(<<"app_id">>, S) of
                {<<"sphera_collab">>, S2} ->
                    S2#{<<"srv_id">> => <<"sphera_telemed">>};
                {Key, S2} ->
                    S2#{<<"srv_id">> => Key};
                error ->
                    S
            end
        end,
        maps:get(<<"status">>, User, [])),
    User#{<<"push">>=>Push, <<"status">>:=Status}.



%% ===================================================================
%% Move users from a domain to another domain
%% ===================================================================

%% @doc
move_users_from_to(FromDomain, ToDomain) ->
    Result = case nkdomain_db:load(FromDomain) of
        #obj_id_ext{obj_id=FId, path=FPath, type=?DOMAIN_DOMAIN} ->
            case nkdomain_db:load(ToDomain) of
                #obj_id_ext{obj_id=TId, path=TPath, type=?DOMAIN_DOMAIN} ->
                    case nkdomain:enable(FId, false) of
                        ok ->
                            %% Loaded both domains and disabled FromDomain
                            ?LLOG(notice, "Moving users from ~p (~p) to ~p (~p)", [FId, FPath, TId, TPath]),
                            %% Unload domain childs
                            nkdomain_domain:unload_childs(FId),
                            case wait_for_unload(FPath) of
                                ok ->
                                    {ok, {FId, FPath}, {TId, TPath}};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error};
                _ ->
                    {error, invalid_object_type}
                end;
        {error, Error} ->
            {error, Error};
        _ ->
            {error, invalid_object_type}
    end,
    case Result of
        {ok, {FromId, FromPath}, {ToId, ToPath}} ->
            Res = case nkdomain_store_es_util:get_opts() of
                {ok, ESOpts} ->
                    case nkdomain_db:iterate(core, {query_paths, FromId, #{type=>?DOMAIN_USER, deep=>true, sort=>path, get_deleted=>true}}, move_fun(ESOpts, ?DOMAIN_USER, FromId, FromPath, ToId, ToPath), 0) of
                        {ok, Count} ->
                            {ok, Count};
                        {error, Error2} ->
                            {error, Error2}
                    end;
                {error, Error2} ->
                    {error, Error2}
            end,
            nkdomain:enable(FromId, true),
            Res;
        {error, Error2} ->
            {error, Error2}
    end.


%% ===================================================================
%% Util
%% ===================================================================


%% @private
print(Obj) ->
    io:format("\n~s\n", [nklib_json:encode_pretty(Obj)]).

%% @private
move_fun(_, _, Id, _, Id, _) ->
    fun(_, Acc) ->
        {ok, Acc}
    end;

move_fun(_, _, _, Path, _, Path) ->
    fun(_, Acc) ->
        {ok, Acc}
    end;

move_fun(ESOpts, Type, FromId, _FromPath, ToId, ToPath) ->
    fun(#{<<"obj_id">>:=ObjId, <<"path">>:=Path, <<"type">>:=Type2}, Acc) ->
        Acc2 = case Type2 of
            Type ->
                case nkelastic:get(ObjId, ESOpts) of
                    {ok, #{<<"obj_id">>:=<<"admin">>}, _} ->
                        ?LLOG(warning, "Ignoring object ~s (~s)", [Path, ObjId]),
                        Acc;
                    {ok, #{<<"domain_id">>:=FromId, <<"parent_id">>:=FromId, <<"obj_name">>:=ObjName, <<"path">>:=OldPath}=Obj, _} ->
                        NewPath = case ToPath of
                            <<"/">> ->
                                <<$/, (nkdomain_util:class(Type))/binary, $/, ObjName/binary>>;
                            _ ->
                                <<ToPath/binary, $/, (nkdomain_util:class(Type))/binary, $/, ObjName/binary>>
                        end,
                        Obj2 = Obj#{
                            <<"domain_id">> => ToId,
                            <<"parent_id">> => ToId,
                            <<"path">> => NewPath
                        },
                        case nkdomain_db:find(NewPath) of 
                            #obj_id_ext{obj_id=_} ->
                                ?LLOG(error, "New path ~p already exists", [NewPath]),
                                Acc;
                            {error, _Error} ->
                                %% NewPath is not used
                                case nkelastic:put(ObjId, Obj2, ESOpts) of
                                    {ok, _} ->
                                        ?LLOG(info, "Moved object ~p from ~p to ~p", [ObjId, OldPath, NewPath]),
                                        Acc+1;
                                    {error, Error2} ->
                                        ?LLOG(error, "~p while saving modified object ~p", [Error2, ObjId]),
                                        Acc
                                end
                        end;
                    {ok, _Obj, _} ->
                        ?LLOG(warning, "Ignoring object ~s (~s)", [Path, ObjId]),
                        Acc;
                    {error, Error} ->
                        ?LLOG(error, "~p while getting object ~s (~s)", [Error, Path, ObjId]),
                        Acc
                end;
            _ ->
                Acc
        end,
        {ok, Acc2}
    end.

%% @private
wait_for_unload(DomainPath) ->
    wait_for_unload(DomainPath, 1, #{wait_time => 2000, max_tries => 5}).

wait_for_unload(_, Tries, #{max_tries := MaxTries}) when Tries > MaxTries ->
    ?LLOG(error, "Maximum number of tries reached", []),
    {error, object_has_childs};

wait_for_unload(DomainPath, Tries, #{wait_time := WaitTime}=Opts) ->
    LoadedObjs = nkdomain_obj:get_all(),
    case is_loaded(DomainPath, LoadedObjs) of
        true ->
            ?LLOG(notice, "Waiting for ~p msecs", [WaitTime]),
            timer:sleep(WaitTime),
            wait_for_unload(DomainPath, Tries+1, Opts);
        false ->
            ok
    end.

%% @private
is_loaded(DomainPath, LoadedObjs) ->
    is_loaded(DomainPath, size(DomainPath), LoadedObjs).

is_loaded(_, _, []) ->
    false;

is_loaded(DomainPath, DomainSize, [{_Type, _ObjId, Path, _Pid}|LoadedObjs]) ->
    case Path of
        <<DomainPath:DomainSize/binary, "/", _/binary>> ->
            ?LLOG(notice, "Found ~p", [Path]),
            true;
        _ ->
            is_loaded(DomainPath, DomainSize, LoadedObjs)
    end.
