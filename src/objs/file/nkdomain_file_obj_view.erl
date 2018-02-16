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

%% @doc File Object

-module(nkdomain_file_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Admin File " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Name = maps:get(name, Obj, <<>>),
    Description = maps:get(description, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    SrvId = maps:get(srv_id, Obj, <<>>),
    File = maps:get(?DOMAIN_FILE, Obj, #{}),
    FileUrl = nkdomain_admin_util:get_file_url(ObjId, Session),
    %% class='photo'
    ContentType = maps:get(content_type, File, <<>>),
    Links = maps:get(links, File, []),
    LinkIds = [nkdomain_admin_util:obj_id_url(Id, Type)|| #{<<"id">> := Id, <<"type">> := Type} <- Links],
    LinksValue = nklib_util:bjoin(LinkIds, <<", ">>),
    Size = maps:get(size, File, 0),
    SizeBinary = nkdomain_admin_util:get_size_bin(Size),
    SizeValue = case Size of
        0 ->
            <<>>;
        _ ->
            <<(nklib_util:to_binary(Size))/binary, " bytes (", SizeBinary/binary, ")">>
    end,
    StoreId = maps:get(store_id, File, <<>>),
    StoreValue = nkdomain_admin_util:obj_id_url(StoreId),
    Base = case ContentType of
        <<"image/", _/binary>> ->
            IconImage = <<"<img style='width:100%; height:auto;' src='", FileUrl/binary, "'/>">>,
            #{with_image => IconImage};
        <<"video/", _/binary>> ->
            IconImage = <<"<video style='width:100%; height:auto;' ref='video-player' controls='true' controlslist='nodownload' src='", FileUrl/binary, "'/>">>,
            #{with_image => IconImage};
        <<"audio/", _/binary>> ->
            IconImage = <<"<audio style='width:100%; height:auto;' ref='audio-player' controls='true' controlslist='nodownload' src='", FileUrl/binary, "'/>">>,
            #{with_image => IconImage};
        _ ->
            case IsNew of
                true -> 
                    #{};
                false ->
                    #{with_image => <<"<a href=\"", FileUrl/binary, "\">Content type not supported</a>">>}
            end
    end,
    FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_FILE, ObjId),
    Spec = Base#{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
        groups => [
            #{
                header => <<"FILE">>,
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
                        id => <<"object_name">>,
                        type => text,
                        label => <<"Object name">>,
                        value => ObjName,
                        required => false,
                        editable => true
                    },
                    #{
                        id => <<"name">>,
                        type => text,
                        label => <<"Name">>,
                        value => Name,
                        editable => true
                    },
                    #{
                        id => <<"description">>,
                        type => text,
                        label => <<"Description">>,
                        value => Description,
                        editable => true
                    },
                    #{
                        id => <<"srv_id">>,
                        type => text,
                        label => <<"Service ID">>,
                        value => SrvId,
                        hidden => IsNew,
                        editable => false
                    }
                ]
            },
            #{
                header => <<"CONFIG">>,
                values => [
                    #{
                        id => <<"store_id">>,
                        type => html,
                        label => <<"Store">>,
                        value => StoreValue,
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"new_store_id">>,
                        type => combo,
                        label => <<"Store">>,
                        value => <<>>,
                        options => nkdomain_admin_util:get_agg_name(<<"obj_id">>, ?DOMAIN_FILE_STORE, <<"/">>, Session),
                        required => true,
                        hidden => not IsNew,
                        editable => true
                    },
                    #{
                        id => <<"content_type">>,
                        type => text,
                        label => <<"Content type">>,
                        value => ContentType,
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"size">>,
                        type => text,
                        label => <<"Size">>,
                        value => SizeValue,
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"links">>,
                        type => html,
                        label => <<"Links">>,
                        value => LinksValue,
                        hidden => LinksValue =:= <<>>,
                        editable => false
                    }
                ]
            },
            nkadmin_webix_form:creation_fields(Obj, IsNew)
        ]
    },
    Data = #{
        id => FormId,
        class => webix_ui,
        value => nkadmin_webix_form:form(Spec, ?DOMAIN_FILE, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    lager:error("NKLOG UPDATE ~p ~p", [ObjId, Data]),
    #{
        <<"object_name">> := ObjName,
        <<"name">> := Name,
        <<"description">> := Description
    } = Data,
    case nkdomain:get_obj(ObjId) of
        {ok, #{?DOMAIN_FILE := _FileData}=OrigData} ->
            Update = OrigData#{
                name => Name,
                description => Description
            },
            case nkdomain:update(ObjId, Update) of
                {ok, _} ->
                    case nkdomain:update_name(ObjId, ObjName) of
                        {ok, _} ->
                            ?LLOG(notice, "user ~s updated", [ObjId]),
                            ok;
                        {error, Error} ->
                            ?LLOG(notice, "could not update user ~s: ~p", [ObjId, Error]),
                            {error, Error}
                    end;
                {error, Error} ->
                    ?LLOG(notice, "could not update user ~s: ~p", [ObjId, Error]),
                    {error, Error}
            end;
        {ok, _Obj} ->
            ?LLOG(warning, "couldn't find file data in: ~p", [_Obj]),
            {error, object_not_found};
        {error, _Error} ->
            ?LLOG(warning, "couldn't load object because: ~p", [_Error]),
            {error, _Error}
    end.


create(Data, _Session) ->
    lager:error("NKLOG CREATE ~p", [Data]),
    #{
        <<"domain">> := DomainId,
        <<"username">> := UserName,
        <<"name">> := Name,
        <<"surname">> := SurName,
        <<"email">> := Email,
        <<"password">> := Pass,
        <<"address">> := Address,
        <<"phone">> := Phone
    } = Data,
    Create = #{
        type => ?DOMAIN_FILE,
        domain_id => DomainId,
        obj_name => UserName,
        ?DOMAIN_FILE => #{
            name => Name,
            surname => SurName,
            email => Email,
            address_t => Address,
            phone_t => Phone,
            password => Pass
        }
    },
    case nkdomain_obj_make:create(Create) of
        {ok, #obj_id_ext{obj_id=ObjId}, []} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.