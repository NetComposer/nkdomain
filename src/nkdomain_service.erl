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

%% @doc NkDomain service callback module
-module(nkdomain_service).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([syntax/0, config/2, init/1]).

-include("nkdomain.hrl").

%% ===================================================================
%% Public functions
%% ===================================================================


%% @private
syntax() ->
    BaseFile = nkfile_util:store_syntax(),
    #{
        listen_ip => host,
        listen_port => {integer, 1, 65535},
        listen_path => basepath,
        listen_secure => boolean,
        start_api_server => boolean,
        start_admin => boolean,
        start_rest => boolean,
        db_store => binary,
        user_password_pbkdf2_iters => {integer, 1, none},
        default_store_id => binary,
        db_clusters => {list, map},
        default_file_store => binary,
        file_stores => {list, BaseFile#{id=>binary}},
        default_mail_provider => binary,
        mail_providers => {list, mail_syntax()},
        '__defaults' => #{
            listen_ip => <<"127.0.0.1">>,
            listen_port => 9301,
            listen_path => <<"/">>,
            listen_secure => false,
            start_api_server => true,
            start_admin => true,
            start_rest => true
        },
        '__mandatory' => [db_store, default_file_store]
    }.


%% @private
config(DomCfg, Config) ->
    #{
        listen_ip := Host,
        listen_port := Port,
        listen_path := Path,
        listen_secure := Secure,
        start_api_server := ApiServer,
        start_admin := Admin,
        start_rest := Rest
    } = DomCfg,
    BinPort = nklib_util:to_binary(Port),
    Http = case Secure of true -> <<"https">>; false -> <<"http">> end,
    Ws = case Secure of true -> <<"wss">>; false -> <<"ws">> end,
    BaseHttp = <<Http/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    BaseWs = <<Ws/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    Config2 = case ApiServer of
        true ->
            Config#{api_server => <<BaseHttp/binary, "/_api, ", BaseWs/binary, "/_api/ws">>};
        false ->
            Config
    end,
    Config3 = case Admin of
        true ->
            Config2#{admin_url => <<BaseHttp/binary, "/_admin">>};
        false ->
            Config2
    end,
    Config4 = case Rest of
        true ->
            Config3#{rest_url => BaseHttp};
        false ->
            Config3
    end,
    DbStore = case Config4 of
        #{nkdomain_db_store:=DbStore0} ->
            DbStore0;
        _ ->
            error(missing_nkdomain_db_store)
    end,
    case parse_file_stores(DomCfg, Config4) of
        {ok, #{default_file_store:=FileStore}=Config5} ->
            case parse_mail_providers(DomCfg, Config5) of
                {ok, #{default_mail_provider:=MailProvider}=Config6} ->
                    Cache = #nkdomain_cache{
                        db_store = DbStore,
                        file_store = FileStore,
                        email_provider = MailProvider
                    },
                    {ok, Config6, Cache};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
init(#{id:=SrvId}=State) ->
    case SrvId:object_db_init(State) of
        {ok, State2} ->
            ok = load_file_stores(SrvId),
            ok = load_mail_providers(SrvId),
            case SrvId:object_db_start(SrvId) of
                ok ->
                    %% gen_server:cast(self(), nkdomain_load_domain),
                    {ok, State2};
                {error, Error} ->
                    {stop, Error}
            end;
        {error, Error} ->
            lager:error("NkDOMAIN: could not start db store: ~p", [Error]),
            {stop, Error}
    end.



%% ===================================================================
%% Internal - File Stores
%% ===================================================================

mail_syntax() ->
    #{
        id => binary,
        class => binary,
        from => binary,
        config => map,
        '__madatory' => [id, class, from, config]
    }.


%% @private
parse_file_stores(DomCfg, Config) ->
    Stores1 = maps:get(file_stores, DomCfg, []),
    case do_parse_file_stores(Stores1, #{}) of
        {ok, Stores2} ->
            #{default_file_store:=FileStoreId1} = DomCfg,
            FileStoreId2 = case maps:is_key(FileStoreId1, Stores2) of
                true ->
                    <<"/file.stores/", FileStoreId1/binary>>;
                false ->
                    FileStoreId1
            end,
            Config2 = Config#{
                default_file_store => FileStoreId2,
                nkdomain_file_stores => Stores2
            },
            {ok, Config2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_parse_file_stores([], Acc) ->
    {ok, Acc};

do_parse_file_stores([Data|Rest], Acc) ->
    Id = maps:get(id, Data, <<"main">>),
    case maps:is_key(Id, Acc) of
        false ->
            do_parse_file_stores(Rest, Acc#{Id=>maps:remove(id, Data)});
        true ->
            {error, {duplicated_id, Id}}
    end.


%% @private
load_file_stores(SrvId) ->
    case SrvId:config() of
        #{nkdomain_file_stores:=Stores} ->
            load_file_stores(SrvId, maps:to_list(Stores));
        _ ->
            ok
    end.


%% @private
load_file_stores(_SrvId, []) ->
    ok;
load_file_stores(SrvId, [{Id, Data}|Rest]) ->
    Obj = #{
        obj_name => Id,
        type => ?DOMAIN_FILE_STORE,
        domain_id => <<"root">>,
        created_by => <<"admin">>,
        ?DOMAIN_FILE_STORE => Data
    },
    case nkdomain_obj_make:create(SrvId, Obj) of
        {ok, #obj_id_ext{path=Path}, _} ->
            lager:notice("NkDOMAIN: created file.store ~s", [Path]);
        {error, object_already_exists} ->
            lager:notice("NkDOMAIN: file.store ~s NOT created (already exists)", [Id]);
        {error, Error} ->
            lager:error("NkDOMAIN: file.store ~s NOT created: ~p", [Id, Error]),
            error(object_creation)
    end,
    load_file_stores(SrvId, Rest).





%% ===================================================================
%% Internal - Mail Providers
%% ===================================================================


%% @private
parse_mail_providers(DomCfg, Config) ->
    Providers1 = maps:get(mail_providers, DomCfg, []),
    case do_parse_mail_providers(Providers1, #{}) of
        {ok, Providers2} ->
            MailProviderId1 = maps:get(default_mail_provider, DomCfg, <<>>),
            MailProviderId2 = case maps:is_key(MailProviderId1, Providers2) of
                true ->
                    <<"/mail.providers/", MailProviderId1/binary>>;
                false ->
                    MailProviderId1
            end,
            Config2 = Config#{
                default_mail_provider => MailProviderId2,
                nkdomain_mail_providers => Providers2
            },
            {ok, Config2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_parse_mail_providers([], Acc) ->
    {ok, Acc};

do_parse_mail_providers([Data|Rest], Acc) ->
    Id = maps:get(id, Data, <<"main">>),
    case maps:is_key(Id, Acc) of
        false ->
            do_parse_mail_providers(Rest, Acc#{Id=>maps:remove(id, Data)});
        true ->
            {error, {duplicated_id, Id}}
    end.


%% @private
load_mail_providers(SrvId) ->
    case SrvId:config() of
        #{nkdomain_mail_providers:=Providers} ->
            load_mail_providers(SrvId, maps:to_list(Providers));
        _ ->
            ok
    end.


%% @private
load_mail_providers(_SrvId, []) ->
    ok;
load_mail_providers(SrvId, [{Id, Data}|Rest]) ->
    Obj = #{
        obj_name => Id,
        type => ?DOMAIN_MAIL_PROVIDER,
        domain_id => <<"root">>,
        created_by => <<"admin">>,
        ?DOMAIN_MAIL_PROVIDER => Data
    },
    case nkdomain_obj_make:create(SrvId, Obj) of
        {ok, #obj_id_ext{path=Path}, _} ->
            lager:notice("NkDOMAIN: created mail.provider ~s", [Path]);
        {error, object_already_exists} ->
            lager:notice("NkDOMAIN: mail.provider ~s NOT created (already exists)", [Id]);
        {error, Error} ->
            lager:error("NkDOMAIN: mail.provider ~s NOT created: ~p", [Id, Error]),
            error(object_creation)
    end,
    load_mail_providers(SrvId, Rest).




