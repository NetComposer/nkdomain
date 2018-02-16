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

%% @doc File Store Object View

-module(nkdomain_file_store_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("Nkdomain File.Store " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    FileStore = maps:get(?DOMAIN_FILE_STORE, Obj, #{}),
    Class = nklib_util:to_binary(maps:get(class, FileStore, <<"filesystem">>)),
    VisibleBatch = case Class of
        <<"filesystem">> ->
            <<"filesystem">>;
        _ ->
            <<"aws">>
    end,
    Encryption = maps:get(encryption, FileStore, <<>>),
    Config = maps:get(config, FileStore, #{}),
    CfgPath = maps:get(path, Config, <<>>),
    CfgAwsId = maps:get(aws_id, Config, <<>>),
    CfgBucket = maps:get(bucket, Config, <<>>),
    CfgBucketAccess = maps:get(bucket_access, Config, <<"auto">>),
    CfgHost = maps:get(host, Config, <<>>),
    CfgPort = maps:get(port, Config, 443),
    CfgScheme = maps:get(scheme, Config, <<"https">>),
    FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_FILE_STORE, ObjId),
    Spec = #{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
        visible_batch => VisibleBatch,
        groups => [
            #{
                header => ?DOMAIN_FILE_STORE,
                values => [
                    case IsNew of
                        true ->
                            {ok, Domains} = nkdomain_admin_util:get_domains("/", Session),
                            Opts = [
                                #{id=>Id, value=>Value} ||
                                {Id, Value} <- [{<<"root">>, <<"/">>}|Domains]
                            ],
                            #{
                                id => <<"domain">>,
                                type => combo,
                                label => <<"Domain">>,
                                value => DomainId,
                                editable => true,
                                options => Opts
                            };
                        false ->
                            DomainPath = case nkdomain_db:find(DomainId) of
                                #obj_id_ext{path=DP} -> DP;
                                _ -> <<>>
                            end,
                            #{
                                id => <<"domain">>,
                                type => text,
                                label => <<"Domain">>,
                                value => DomainPath,
                                editable => false
                            }
                    end,
                    #{
                        id => <<"obj_name">>,
                        type => text,
                        label => <<"Object name">>,
                        value => ObjName,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"class">>,
                        type => combo,
                        label => <<"Class">>,
                        value => Class,
                        options => [#{
                            <<"id">> => <<"filesystem">>,
                            <<"value">> => <<"filesystem">>
                        }, #{
                            <<"id">> => <<"s3">>,
                            <<"value">> => <<"s3">>
                        }, #{
                            <<"id">> => <<"s3_mini">>,
                            <<"value">> => <<"s3_mini">>
                        }],
                        onChange => class_on_change(FormId),
                        required => true,
                        editable => true %IsNew
                    },
                    #{
                        id => <<"encryption">>,
                        type => combo,
                        label => <<"Encryption">>,
                        value => Encryption,
                        options => [<<>>, <<"aes_cfb128">>, <<"aes_cbc256">>],
                        editable => true
                    }
                ]
            },
            #{
                header => <<"LOCAL STORE">>,
                batch => <<"filesystem">>,
                values => [
                    #{
                        id => <<"path">>,
                        type => text,
                        label => <<"Path">>,
                        value => CfgPath,
                        required => true,
                        editable => true
                    }
                ]
            },
            #{
                header => <<"S3 COMPATIBLE STORE">>,
                batch => <<"aws">>,
                values => [
                    #{
                        id => <<"aws_id">>,
                        type => text,
                        label => <<"AWS ID">>,
                        value => CfgAwsId,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"aws_secret">>,
                        type => password,
                        label => <<"AWS secret">>,
                        value => <<>>,
                        required => IsNew,
                        editable => true
                    },
                    #{
                        id => <<"bucket">>,
                        type => text,
                        label => <<"Bucket">>,
                        value => CfgBucket,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"bucket_access">>,
                        type => combo,
                        label => <<"Bucket access">>,
                        value => CfgBucketAccess,
                        options => [
                            <<"auto">>,
                            <<"path">>,
                            <<"virtual_hosted">>
                        ],
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"host">>,
                        type => text,
                        label => <<"Host">>,
                        value => CfgHost,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"port">>,
                        type => number_edit,
                        label => <<"Port">>,
                        value => CfgPort,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"scheme">>,
                        type => combo,
                        label => <<"Scheme">>,
                        value => CfgScheme,
                        options => [<<"http">>, <<"https">>],
                        editable => true
                    }
                ]
            },
            nkadmin_webix_form:creation_fields(Obj, IsNew)
        ]
    },
    Data = #{
        id => FormId,
        class => webix_ui,
        value => nkadmin_webix_form:form(Spec, ?DOMAIN_FILE_STORE, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    #{
        <<"obj_name">> := ObjName,
        <<"class">> := Class,
        <<"encryption">> := Encryption
    } = Data,
    Update = case Encryption of
        <<>> ->
            maps:without([<<"encryption">>], Data);
        _ ->
            Data
    end,
    Data2 = case Data of
        #{<<"aws_secret">> := <<>>} when Class =/= <<"filesystem">> ->
            case nkdomain:get_obj(ObjId) of
                {ok, #{?DOMAIN_FILE_STORE := #{config := #{aws_secret := OldPassword}}}} ->
                    Data#{<<"aws_secret">> => OldPassword};
                {ok, _Obj} ->
                    ?LLOG(warning, "couldn't find aws_secret field in: ~p", [_Obj]),
                    Data;
                {error, _Error} ->
                    ?LLOG(warning, "couldn't load object old aws_secret because: ~p", [_Error]),
                    Data
            end;
        _ ->
            Data
    end,
    Config = case Class of
        <<"filesystem">> ->
            maps:with([<<"path">>], Data);
        <<"s3">> ->
            maps:with([<<"aws_id">>, <<"aws_secret">>, <<"bucket">>, <<"bucket_access">>, <<"host">>, <<"port">>, <<"scheme">>], Data2);
        <<"s3_mini">> ->
            maps:with([<<"aws_id">>, <<"aws_secret">>, <<"bucket">>, <<"bucket_access">>, <<"host">>, <<"port">>, <<"scheme">>], Data2)
    end,
    Update2 = Update#{
        <<"class">> => Class,
        <<"config">> => Config
    },
    case nkdomain:update(ObjId, #{?DOMAIN_FILE_STORE => Update2}) of
        {ok, _} ->
            case nkdomain:update_name(ObjId, ObjName) of
                {ok, _} ->
                    ?LLOG(notice, "file.store ~s updated", [ObjId]),
                    ok;
                {error, Error} ->
                    ?LLOG(notice, "could not update file.store ~s: ~p", [ObjId, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?LLOG(notice, "could not update file.store ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.


create(Data, _Session) ->
    %lager:error("NKLOG CREATE ~p", [Data]),
    #{
        <<"domain">> := DomainId,
        <<"obj_name">> := ObjName,
        <<"class">> := Class,
        <<"encryption">> := Encryption
    } = Data,
    Config = case Class of
        <<"filesystem">> ->
            maps:with([<<"path">>], Data);
        <<"s3">> ->
            maps:with([<<"aws_id">>, <<"aws_secret">>, <<"bucket">>, <<"bucket_access">>, <<"host">>, <<"port">>, <<"scheme">>], Data);
        <<"s3_mini">> ->
            maps:with([<<"aws_id">>, <<"aws_secret">>, <<"bucket">>, <<"bucket_access">>, <<"host">>, <<"port">>, <<"scheme">>], Data)
    end,
    FileStore = case Encryption of
        <<>> ->
            #{};
        _ ->
            #{encryption => Encryption}
    end,
    Create = #{
        type => ?DOMAIN_FILE_STORE,
        domain_id => DomainId,
        obj_name => ObjName,
        ?DOMAIN_FILE_STORE => FileStore#{
            class => Class,
            config => Config
        }
    },
    lager:error("NKLOG CREATE ~p", [Create]),
    case nkdomain_obj_make:create(Create) of
        {ok, #obj_id_ext{obj_id=ObjId}, []} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.



%% private
class_on_change(FormId) ->
    <<"function() {
        var value = this.getValue();
        if (value === 'filesystem') {
            $$('", FormId/binary, "').showBatch('filesystem');
        } else {
            $$('", FormId/binary, "').showBatch('aws');
        }
    }">>.