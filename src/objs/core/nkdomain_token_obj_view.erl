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

-module(nkdomain_token_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("Nkdomain Token " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    Subtype = maps:get(subtype, Obj, <<>>),
    Token = maps:get(?DOMAIN_TOKEN, Obj, #{}),
%    VisibleBatch = <<>>,
    TokenVsn = maps:get(vsn, Token, <<>>),
    TokenData = maps:get(data, Token, #{}),
    FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_TOKEN, ObjId),
    Spec = #{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save, disabled => IsNew}
        ],
%        visible_batch => VisibleBatch,
        groups => [
            #{
                header => ?DOMAIN_TOKEN,
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
                    },
                    #{
                        id => <<"subtype">>,
                        type => text,
                        label => <<"Type">>,
                        value => Subtype,
                        required => true,
                        editable => false
                    }
                ]
            },
            #{
                header => <<"CONFIGURATION">>,
                %batch => <<>>,
                values => [
                    #{
                        id => <<"vsn">>,
                        type => text,
                        label => <<"Version">>,
                        value => TokenVsn,
                        required => true,
                        editable => false
                    },
                    #{
                        id => <<"data">>,
                        type => textarea,
                        label => <<"Data">>,
                        value => nklib_json:encode_pretty(TokenData),
                        height => 300,
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
        value => nkadmin_webix_form:form(Spec, ?DOMAIN_TOKEN, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    #{
        <<"obj_name">> := ObjName
    } = Data,
    case nkdomain:update_name(ObjId, ObjName) of
        {ok, _} ->
            ?LLOG(notice, "token ~s updated", [ObjId]),
            ok;
        {error, Error} ->
            ?LLOG(notice, "could not update token ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.


create(_Data, _Session) ->
    %lager:notice("NKLOG CREATE ~p", [Data]),
    {error, not_implemented}.

%    #{
%        <<"domain">> := DomainId,
%        <<"obj_name">> := ObjName,
%        <<"new_input">> := NewInput
%    } = Data,
%    Token = maps:with([<<"processor_id">>, <<"format">>, <<"height">>, <<"width">>, <<"callback_url">>], Data),
%    Create = #{
%        type => ?DOMAIN_TOKEN,
%        domain_id => DomainId,
%        obj_name => ObjName,
%        ?DOMAIN_TOKEN => Token#{
%            <<"input">> => NewInput
%        }
%    },
%    lager:notice("NKLOG CREATE ~p", [Create]),
%    case nkdomain_obj_make:create(Create) of
%        {ok, #obj_id_ext{obj_id=ObjId}, []} ->
%            {ok, ObjId};
%        {error, Error} ->
%            {error, Error}
%    end.



%% @private
%class_on_change(FormId) ->
%    <<"function() {
%        var value = this.getValue();
%        $$('", FormId/binary, "').showBatch(value);
%    }">>.

