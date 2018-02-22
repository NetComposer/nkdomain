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

%% @doc Image Job Object View

-module(nkdomain_image_job_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkimage/include/nkimage.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("Nkdomain Image.Job " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    ImageJob = maps:get(?IMAGE_JOB, Obj, #{}),
%    VisibleBatch = <<>>,
    CallbackUrl = maps:get(callback_url, ImageJob, <<>>),
    Format = maps:get(format, ImageJob, <<>>),
    Height = maps:get(height, ImageJob, <<>>),
    Width = maps:get(width, ImageJob, <<>>),
    Input = maps:get(input, ImageJob, <<>>),
    Output = maps:get(output, ImageJob, <<>>),
    ProcessorId = maps:get(processor_id, ImageJob, <<>>),
    Progress = maps:get(progress, ImageJob, <<>>),
    Status = maps:get(status, ImageJob, <<"not_started">>),
    FormId = nkdomain_admin_util:make_obj_view_id(?IMAGE_JOB, ObjId),
    Spec = #{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
%        visible_batch => VisibleBatch,
        groups => [
            #{
                header => ?IMAGE_JOB,
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
                        required => false,
                        editable => true
                    }
                ]
            },
            #{
                header => <<"CONFIGURATION">>,
                %batch => <<>>,
                values => [
                    #{
                        id => <<"processor_id">>,
                        type => combo,
                        label => <<"Processor">>,
                        value => ProcessorId,
                        required => true,
                        editable => IsNew,
                        options => nkdomain_admin_util:get_agg_name(<<"obj_id">>, ?IMAGE_PROCESSOR, <<"/">>, Session)
                    },
                    #{
                        id => <<"status">>,
                        type => text,
                        label => <<"Status">>,
                        value => Status,
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"progress">>,
                        type => text,
                        label => <<"Progress">>,
                        value => Progress,
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"format">>,
                        type => suggest,
                        label => <<"Format">>,
                        value => Format,
                        options => format_suggestions(),
                        required => true,
                        editable => IsNew
                    },
                    #{
                        id => <<"height">>,
                        type => number_edit,
                        label => <<"Height">>,
                        value => Height,
                        required => true,
                        editable => IsNew
                    },
                    #{
                        id => <<"width">>,
                        type => number_edit,
                        label => <<"Width">>,
                        value => Width,
                        required => true,
                        editable => IsNew
                    },
                    #{
                        id => <<"new_input">>,
                        type => combo,
                        label => <<"Input">>,
                        value => Input,
                        suggest_type => ?DOMAIN_FILE,
                        suggest_field => <<"name">>,
                        suggest_template => <<"#name# (#file_type#, #file_size#)">>,
                        required => true,
                        hidden => (not IsNew),
                        editable => IsNew
                    },
                    #{
                        id => <<"input">>,
                        type => html,
                        label => <<"Input">>,
                        value => nkdomain_admin_util:obj_id_url(Input),
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"output">>,
                        type => html,
                        label => <<"Output">>,
                        value => nkdomain_admin_util:obj_id_url(Output, Output),
                        hidden => IsNew or (Output =:= <<>>),
                        editable => false
                    },
                    #{
                        id => <<"callback_url">>,
                        type => text,
                        label => <<"Callback URL">>,
                        value => CallbackUrl,
                        required => false,
                        editable => IsNew
                    }
                ]
            },
            nkadmin_webix_form:creation_fields(Obj, IsNew)
        ]
    },
    Data = #{
        id => FormId,
        class => webix_ui,
        value => nkadmin_webix_form:form(Spec, ?IMAGE_JOB, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    #{
        <<"obj_name">> := ObjName
    } = Data,
    case nkdomain:update_name(ObjId, ObjName) of
        {ok, _} ->
            ?LLOG(notice, "image.job ~s updated", [ObjId]),
            ok;
        {error, Error} ->
            ?LLOG(notice, "could not update image.job ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.


create(Data, _Session) ->
    %lager:error("NKLOG CREATE ~p", [Data]),
    #{
        <<"domain">> := DomainId,
        <<"obj_name">> := ObjName,
        <<"new_input">> := NewInput
    } = Data,
    ImageJob = maps:with([<<"processor_id">>, <<"format">>, <<"height">>, <<"width">>, <<"callback_url">>], Data),
    Create = #{
        type => ?IMAGE_JOB,
        domain_id => DomainId,
        obj_name => ObjName,
        ?IMAGE_JOB => ImageJob#{
            <<"input">> => NewInput
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
%class_on_change(FormId) ->
%    <<"function() {
%        var value = this.getValue();
%        $$('", FormId/binary, "').showBatch(value);
%    }">>.


%% @private
format_suggestions() ->
    [<<"image/gif">>, <<"image/jpeg">>, <<"image/png">>, <<"image/pdf">>, <<"image/tiff">>].