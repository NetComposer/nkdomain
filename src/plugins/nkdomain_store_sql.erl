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

%% @doc Elasticsearch plugin
-module(nkdomain_store_sql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-compile(export_all).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store SQL "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================

connect() ->
    Opts = [
        {database, "system"},
        {host, "127.0.0.1"},
        {user, "root"},
        {port, 26257},
        {password, ""},
        %{connect_timeout, ConnectTimeout},
        {as_binary, true}
    ],
    {ok, P} = pgsql_proto:start(Opts),
    put(pgsql, P),
    P.


connect2() ->
    Opts = [
        {database, "carlosj"},
        {host, "127.0.0.1"},
        {user, "carlosj"},
        {port, 5432},
        {password, ""},
        %{connect_timeout, ConnectTimeout},
        {as_binary, true}
    ],
    {ok, P} = pgsql_proto:start(Opts),
    put(pgsql, P),
    P.


create_database() ->
    create_database("nkobjects").


create_base() ->
%%    --DROP DATABASE IF EXISTS nkobjects;
%%--CREATE DATABASE nkobjects;
%%--SET DATABASE TO nkobjects
%%--DROP TABLE IF EXISTS carlosj.object CASCADE;
%%--DROP TABLE IF EXISTS carlosj.aliases CASCADE;
    Sql = <<"


        CREATE TABLE object (
            obj_id STRING PRIMARY KEY NOT NULL,
            path STRING UNIQUE NOT NULL,
            parent_id STRING NOT NULL,
            subtype STRING,
            created_by STRING,
            created_time INT,
            updated_by STRING,
            updated_time INT,
            enabled BOOL DEFAULT TRUE,
            active BOOL DEFAULT TRUE,
            expires_time INT,
            destroyed_time INT,
            destroyed_code STRING,
            destroyed_reason STRING,
            name STRING,
            description STRING,
            icon_id STRING,
            INDEX (path),
            INDEX (created_time)
        );

        CREATE TABLE aliases (
            obj_id STRING PRIMARY KEY NOT NULL,
            referred_id STRING NOT NULL,
            INDEX (obj_id)
        );


    ">>,
    query(Sql).


insert_objs(Start, End) when Start < End ->
    Pos = esc(Start),
    Id = <<"TestId-", Pos/binary>>,
    Path = <<"TestPath-", Pos/binary>>,
    Parent = <<"TestParent-", Pos/binary>>,
    Alias = <<"TestAlias-", Pos/binary>>,
    Type = <<"TestType-", Pos/binary>>,
    insert_obj(Id, Path, Parent, Alias, Type),
    insert_objs(Start+1, End);

insert_objs(_Start, _End) ->
    ok.




insert_obj(Id, Path, Parent, Alias, Type) ->
    S = [<<"
            --BEGIN;

            INSERT INTO nkobjects.object (obj_id, path, parent_id, subtype) VALUES ">>, params([Id, Path, Parent, Type]), <<";

            INSERT INTO nkobjects.aliases (obj_id, referred_id) VALUES ">>, params([Alias, Id]), <<";

            --COMMIT;
        ">>],
    query(S).



f1(Id) ->
    S = [
        <<"SELECT obj_id, path FROM nkobjects.object WHERE object.obj_id=">>, esc(Id)
    ],
    query(S).


f2(Id) ->
    S = [
        <<"SET DATABASE TO nkobjects;\n">>,
        <<"SELECT obj_id, path FROM nkobjects.object WHERE obj_id=">>, esc(Id), <<" OR subtype=">>, esc(Id), <<";">>
    ],
    query(S).



f3(Id) ->
    S = [<<"
            SET DATABASE TO nkobjects;
            SELECT obj_id, path FROM object@primary, object@object_path_idx WHERE obj_id>">>, esc(Id), <<" ORDER BY obj_id DESC LIMIT 10 OFFSET 9000 ;">> %<<" OR path=">>, esc(Id)
    ],
    query(S).


f(Id) ->
    S = [<<"
            SET DATABASE TO nkobjects;
            SELECT object.obj_id, path, aliases.obj_id FROM nkobjects.object, nkobjects.aliases
           WHERE object.obj_id=">>, esc(Id), <<" OR path=">>, esc(Id), <<" OR aliases.obj_id=">>, esc(Id)
    ],
    query(S).

show() ->
    query(<<"SHOW INDEXES FROM nkobjects.object;">>).




esc(true) -> <<"TRUE">>;
esc(false) -> <<"FALSE">>;
esc(Term) when is_integer(Term) -> integer_to_binary(Term);
esc(Term) -> [$', to_bin(Term), $'].

list(List) -> nklib_util:bjoin([esc(T) || T <-List]).
params(List) -> [$(, list(List), $)].



create_database(Name) ->
    case query(["CREATE DATABASE IF NOT EXISTS ", Name]) of
        {ok, [<<"CREATE DATABASE">>]} ->
            ok;
        {error, Error} ->
            {error, Error};
        Other ->
            {error, Other}
    end.



create_colum(Data) ->
    Type = case maps:get(type, Data) of
        integer -> <<"INT">>;
        serial -> <<"SERIAL">>;
        decimal -> <<"DECIMAL">>;
        {decimal, P} -> <<"DECIMAL(", (to_bin(P))/binary, ")">>;
        {decimal, P, D} -> <<"DECIMAL(", (to_bin(P))/binary, ",", (to_bin(D))/binary, ")">>;
        float -> <<"FLOAT">>;
        boolean -> <<"BOOL">>;
        date -> <<"DATE">>;
        timestamp -> <<"TIMESTAMP">>;
        timestampz -> <<"TIMESTAMPZ">>;
        interval -> <<"INTERVAL">>;
        string -> <<"STRING">>;
        {string, S} -> <<"STRING(", (to_bin(S))/binary, ")">>;
        {collate, Lang} -> <<"STRING COLLATE ",(to_bin(Lang))/binary>>;
        bytes -> <<"BYTES">>
    end,
    Str = [
        Type,
        case maps:get(primary, Data, false) of
            true -> <<" PRIMARY KEY">>;
            false -> <<>>
        end,
        case maps:get(null, Data, true) of
            false -> <<" NOT NULL">>;
            true -> <<>>
        end,
        case maps:get(unique, Data, false) of
            true -> <<" UNIQUE">>;
            false -> <<>>
        end,
        case maps:get(default, Data, none) of
            none -> <<>>;
            Default -> to_bin(Default)
        end
    ],
    list_to_binary(Str).




query(Cmd) ->
    P = get(pgsql),
    Bin = iolist_to_binary(Cmd),
    Start = nklib_util:l_timestamp(),
    case pgsql:squery(P,  Bin, 5000) of
        {ok, List} ->
            Time = (nklib_util:l_timestamp() - Start) / 1000,
            io:format("Q (~p): ~s\n", [Time, Bin]),
            lists:map(
                fun
                    ({error, Error}) -> {error, nklib_util:get_value(message, Error, <<>>)};
                    (Res) -> {ok, Res}
                end,
                List);
        Other ->
            {error, Other}
    end.



%% @private
to_bin(K) when is_binary(K) -> K;
to_bin(K) -> nklib_util:to_binary(K).