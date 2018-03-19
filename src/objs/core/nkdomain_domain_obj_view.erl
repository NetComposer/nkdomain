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

%% @doc Domain Object

-module(nkdomain_domain_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

-define(CHAT_CONVERSATION, <<"conversation">>).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Admin Domain " ++ Txt, Args)).




%% @private
view(Obj, IsNew, #admin_session{user_id=UserId, domain_id=DefDomain, srv_id=SrvId}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, DefDomain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Name = maps:get(name, Obj, <<>>),
    Description = maps:get(description, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    Tags = maps:get(tags, Obj, []),
    IconId = maps:get(icon_id, Obj, <<>>),
    IconUrl = case IconId of
        <<>> ->
            <<"img/question_mark.png">>;
        IconId ->
            nkdomain_admin_util:get_file_url(IconId, Session)
    end,
    IconImage = <<"<img class='photo' style='padding: 0px 10% 0 10%; width:80%; height:auto;' src='", IconUrl/binary, "'/>">>,
    ClientId = nkdomain_admin_util:get_client_id(Session),
    Domain = maps:get(?DOMAIN_DOMAIN, Obj, #{}),
    Configs = maps:get(configs, Domain, #{}),
    DefChanMap = maps:get(<<"default_channels_", ClientId/binary>>, Configs, #{<<"list">> => []}),
    DefaultChannels = case DefChanMap of
        #{<<"list">> := L} ->
            L;
        #{list := L} ->
            L
    end,
    DefaultChannelsBin = lists:join(<<",">>, DefaultChannels),
    DefaultChannelsOpts = get_convs_opts(DefaultChannels),
    FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_DOMAIN, ObjId),
    Base = case IsNew of
        true ->
            #{};
        false ->
            #{with_image => IconImage, with_css => <<"photo">>, with_file_types => ["png", "jpg", "jpeg"]}
    end,
    Spec = Base#{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
        groups => [
            #{
                header => ?DOMAIN_DOMAIN,
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
                        required => IsNew,
                        editable => IsNew
                    },
                    #{
                        id => <<"name">>,
                        type => text,
                        label => <<"Name">>,
                        value => Name,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"description">>,
                        type => text,
                        label => <<"Description">>,
                        value => Description,
                        editable => true
                    }
                ]
            },
            #{
                header => <<"CONFIGURATION">>,
                hidden => IsNew,
                values => [
                    #{
                        id => <<"default_channels">>,
                        type => multicombo,
                        label => <<"Default channels">>,
                        value => DefaultChannelsBin,
                        suggest_type => ?CHAT_CONVERSATION,
                        suggest_field => <<"name">>,
                        suggest_filters => #{
                            conversation_type => <<"channel">>
                        },
                        suggest_template => <<"#name#">>,
                        options => DefaultChannelsOpts,
                        hidden => IsNew,
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
        value => nkadmin_webix_form:form(Spec, ?DOMAIN_DOMAIN, Session)
    },
    {ok, Data, Session}.





update(ObjId, Data, #admin_session{user_id=UserId}=Session) ->
    ?LLOG(info, "NKLOG UPDATE ~p ~p", [ObjId, Data]),
    Base = maps:with([<<"name">>, <<"description">>, <<"icon_id">>], Data),
    DefaultChannels = maps:get(<<"default_channels">>, Data, []),
    DefaultChannelsList = case binary:split(DefaultChannels, <<",">>, [global]) of
        [<<>>] ->
            [];
        Other ->
            Other
    end,
    DefChanMap = #{<<"list">> => DefaultChannelsList},
    ClientId = nkdomain_admin_util:get_client_id(Session),
    case nkdomain:update(ObjId, Base) of
        {ok, _} ->
            ?LLOG(notice, "domain ~s updated", [ObjId]),
            nkdomain_domain:set_config(ObjId, <<"default_channels_", ClientId/binary>>, DefChanMap);
        {error, Error} ->
            ?LLOG(notice, "could not update domain ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.


create(Data, _Session) ->
    ?LLOG(info, "NKLOG CREATE ~p", [Data]),
    #{
        <<"domain">> := DomainId,
        <<"obj_name">> := ObjName,
        <<"name">> := Name,
        <<"description">> := Description
    } = Data,
    DomainCreate = #{
        type => ?DOMAIN_DOMAIN,
        domain_id => DomainId,
        obj_name => ObjName,
        name => Name,
        description => Description,
        ?DOMAIN_DOMAIN => #{}
    },
    case nkdomain_obj_make:create(DomainCreate) of
        {ok, #obj_id_ext{obj_id=ObjId}, [<<"domain.config">>]} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.


get_convs_opts(Ids) ->
    get_convs_opts(Ids, []).

%% @private
get_convs_opts([], Acc) ->
    lists:reverse(Acc);

get_convs_opts([Id|Ids], Acc) ->
    case nkdomain:get_name(Id) of
        {ok, #{obj_id:=ObjId, name:=Name}} ->
            get_convs_opts(Ids, [#{id => ObjId, name => Name} | Acc]);
        {error, _Error} ->
            ?LLOG(warning, "Unknown ID found: ~p", [Id]),
            get_convs_opts(Ids, [#{id => Id, name => Id} | Acc])
    end.