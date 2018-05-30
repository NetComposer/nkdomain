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

%% @doc Mail Provider Object View

-module(nkdomain_mail_provider_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("Nkdomain Mail Provider: " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    MailProvider = maps:get(?DOMAIN_MAIL_PROVIDER, Obj, #{}),
    Class = nklib_util:to_binary(maps:get(class, MailProvider, <<"smtp">>)),
    From = nklib_util:to_binary(maps:get(from, MailProvider, <<>>)),
    Config = maps:get(config, MailProvider, #{}),
    CfgRelay = maps:get(relay, Config, <<>>),
    CfgUsername = maps:get(username, Config, <<>>),
    CfgPassword = maps:get(password, Config, <<>>),
    CfgForceTLS = maps:get(force_tls, Config, true),
    FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_MAIL_PROVIDER, ObjId),
    Spec = #{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
        groups => [
            #{
                header => ?DOMAIN_MAIL_PROVIDER,
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
                            <<"id">> => <<"smtp">>,
                            <<"value">> => <<"smtp">>
                        }],
                        editable => true
                    }
                ]
            },
            #{
                header => <<"CONFIGURATION">>,
                values => [
                    #{
                        id => <<"relay">>,
                        type => text,
                        label => <<"Relay">>,
                        value => CfgRelay,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"username">>,
                        type => text,
                        label => <<"Username">>,
                        value => CfgUsername,
                        editable => true
                    },
                    #{
                        id => <<"password">>,
                        type => password,
                        label => <<"Password">>,
                        value => <<>>,
                        editable => true
                    },
                    #{
                        id => <<"from">>,
                        type => text,
                        label => <<"From">>,
                        value => From,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"force_tls">>,
                        type => checkbox,
                        label => <<"Force TLS">>,
                        value => CfgForceTLS,
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
        value => nkadmin_webix_form:form(Spec, ?DOMAIN_MAIL_PROVIDER, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    #{
        <<"obj_name">> := ObjName,
        <<"class">> := Class,
        <<"from">> := From,
        <<"relay">> := Relay,
        <<"username">> := Username,
        <<"password">> := Password,
        <<"force_tls">> := ForceTLS
    } = Data,
    Config = maps:with([<<"relay">>, <<"username">>, <<"password">>, <<"force_tls">>], Data),
    Update = #{
        <<"class">> => Class,
        <<"from">> => From,
        <<"config">> => Config
    },
    case nkdomain:update(ObjId, #{?DOMAIN_MAIL_PROVIDER => Update}) of
        {ok, _} ->
            case nkdomain:update_name(ObjId, ObjName) of
                {ok, _} ->
                    ?LLOG(notice, "~s updated", [ObjId]),
                    ok;
                {error, Error} ->
                    ?LLOG(notice, "could not update ~s: ~p", [ObjId, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?LLOG(notice, "could not update ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.


create(Data, _Session) ->
    %lager:notice("NKLOG CREATE ~p", [Data]),
    #{
        <<"domain">> := DomainId,
        <<"obj_name">> := ObjName,
        <<"class">> := Class,
        <<"from">> := From,
        <<"relay">> := Relay,
        <<"username">> := Username,
        <<"password">> := Password,
        <<"force_tls">> := ForceTLS
    } = Data,
    Config = maps:with([<<"relay">>, <<"username">>, <<"password">>, <<"force_tls">>], Data),
    MailProvider = case Relay of
        <<>> ->
            #{};
        _ ->
            #{config => Config}
    end,
    Create = #{
        type => ?DOMAIN_MAIL_PROVIDER,
        domain_id => DomainId,
        obj_name => ObjName,
        ?DOMAIN_MAIL_PROVIDER => MailProvider#{
            class => Class,
            from => From
        }
    },
    %lager:notice("NKLOG CREATE ~p", [Create]),
    case nkdomain_obj_make:create(Create) of
        {ok, #obj_id_ext{obj_id=ObjId}, []} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.