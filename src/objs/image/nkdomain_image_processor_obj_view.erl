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

%% @doc Image Processor Object View

-module(nkdomain_image_processor_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkimage/include/nkimage.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("Nkdomain Image.Processor " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    CreatedBy = maps:get(created_by, Obj, <<>>),
    CreatedTime = maps:get(created_time, Obj, 0),
    UpdatedBy = maps:get(updated_by, Obj, <<>>),
    UpdatedTime = maps:get(updated_time, Obj, 0),
    Enabled = maps:get(enabled, Obj, true),
    ImageProcessor = maps:get(?IMAGE_PROCESSOR, Obj, #{}),
    Class = nklib_util:to_binary(maps:get(class, ImageProcessor, <<"pillow">>)),
    VisibleBatch = Class,
    Encryption = maps:get(encryption, ImageProcessor, <<>>),
    Config = maps:get(config, ImageProcessor, #{}),
    CfgPath = maps:get(path, Config, <<>>),
    CfgHost = maps:get(host, Config, <<>>),
    CfgPort = maps:get(port, Config, 443),
    CfgScheme = maps:get(scheme, Config, <<"https">>),
    FormId = nkdomain_admin_util:make_obj_view_id(?IMAGE_PROCESSOR, ObjId),
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
                header => ?IMAGE_PROCESSOR,
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
                            <<"id">> => <<"pillow">>,
                            <<"value">> => <<"pillow">>
                        }],
                        onChange => class_on_change(FormId),
                        required => true,
                        editable => true %IsNew
                    }
                ]
            },
            #{
                header => <<"CONFIGURATION">>,
                %batch => <<"pillow">>,
                values => [
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
                    },
                    #{
                        id => <<"path">>,
                        type => text,
                        label => <<"Path">>,
                        value => CfgPath,
                        required => false,
                        editable => true
                    }
                ]
            },
            #{
                header => <<"OTHER">>,
                values => [
                    #{
                        id => <<"created_by">>,
                        type => html,
                        label => <<"Created by">>,
                        value => nkdomain_admin_util:obj_id_url(CreatedBy),
                        editable => false
                    },
                    #{
                        id => <<"created_time">>,
                        type => date,
                        label => <<"Created time">>,
                        value => CreatedTime,
                        editable => false
                    },
                    #{
                        id => <<"updated_by">>,
                        type => html,
                        label => <<"Updated by">>,
                        value => nkdomain_admin_util:obj_id_url(UpdatedBy),
                        editable => false
                    },
                    #{
                        id => <<"updated_time">>,
                        type => date,
                        label => <<"Updated time">>,
                        value => UpdatedTime,
                        editable => false
                    }
                ]
            }
        ]
    },
    Data = #{
        id => FormId,
        class => webix_ui,
        value => nkadmin_webix_form:form(Spec, ?IMAGE_PROCESSOR, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    #{
        <<"obj_name">> := ObjName,
        <<"class">> := Class
    } = Data,
    Config = maps:with([<<"host">>, <<"port">>, <<"scheme">>, <<"path">>], Data),
    Update = #{
        <<"class">> => Class,
        <<"config">> => Config
    },
    case nkdomain:update(ObjId, #{?IMAGE_PROCESSOR => Update}) of
        {ok, _} ->
            case nkdomain:update_name(ObjId, ObjName) of
                {ok, _} ->
                    ?LLOG(notice, "image.processor ~s updated", [ObjId]),
                    ok;
                {error, Error} ->
                    ?LLOG(notice, "could not update image.processor ~s: ~p", [ObjId, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?LLOG(notice, "could not update image.processor ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.


create(Data, _Session) ->
    %lager:error("NKLOG CREATE ~p", [Data]),
    #{
        <<"domain">> := DomainId,
        <<"obj_name">> := ObjName,
        <<"class">> := Class
    } = Data,
    Config = maps:with([<<"host">>, <<"port">>, <<"scheme">>, <<"path">>], Data),
    Create = #{
        type => ?IMAGE_PROCESSOR,
        domain_id => DomainId,
        obj_name => ObjName,
        ?IMAGE_PROCESSOR => #{
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



%% @private
class_on_change(FormId) ->
    <<"function() {
        var value = this.getValue();
        $$('", FormId/binary, "').showBatch(value);
    }">>.