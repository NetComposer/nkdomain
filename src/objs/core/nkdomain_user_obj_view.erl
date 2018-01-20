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

%% @doc User Object

-module(nkdomain_user_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/2, save/3]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAN Admin User " ++ Txt, Args)).

-define(CHAT_MESSAGE, <<"message">>).


%% @doc
view(#obj_id_ext{pid=Pid}, Session) ->
    case nkdomain:get_obj(Pid) of
        {ok, Obj} ->
            #{
                obj_id := ObjId,
                domain_id := DomainId,
                type := Type,
                obj_name := ObjName,
                created_by := CreatedBy,
                created_time := CreatedTime,
                updated_by := UpdatedBy,
                updated_time := UpdatedTime,
                ?DOMAIN_USER := User
            } = Obj,
            IconUrl = case maps:get(icon_id, Obj, <<>>) of
                <<>> ->
                    <<"img/avatar.png">>;
                IconId ->
                    nkdomain_admin_util:get_file_url(IconId, Session)
            end,
            IconImage = <<"<img class='photo' style='width:150px; height:150px;' src='", IconUrl/binary, "'/>">>,
            FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_USER, ObjId),
            IsNew = true,
            Enabled = maps:get(enabled, Obj, true),
            Spec = #{
                form_id => FormId,
                with_image => IconImage,
                buttons => [
                    #{type => case Enabled of true -> disable; _ -> enable end},
                    #{type => delete},
                    #{type => save}
                ],
                groups => [
                    #{
                        header => <<"CREDENTIALS">>,
                        values => [
                            case IsNew of
                                true ->
                                    {ok, Domains} = nkdomain_admin_util:get_domains("/"),
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
                                id => <<"username">>,
                                type => text,
                                label => <<"Username">>,
                                value => ObjName,
                                editable => true
                            },
                            #{
                                id => <<"email">>,
                                type => text,
                                label => <<"Email">>,
                                value => maps:get(email, User, <<>>),
                                editable => true
                            },
                            #{
                                id => <<"password">>,
                                type => password,
                                label => <<"Password">>,
                                editable => true
                            }
                        ]
                    },
                    #{
                        header => <<"PERSONAL DATA">>,
                        values => [
                            #{
                                id => <<"name">>,
                                type => text,
                                label => <<"First name">>,
                                value => maps:get(name, User, <<>>),
                                editable => true
                            },
                            #{
                                id => <<"surname">>,
                                type => text,
                                label => <<"Last name">>,
                                value => maps:get(surname, User, <<>>),
                                editable => true
                            },
                            #{
                                id => <<"phone">>,
                                type => text,
                                label => <<"Phone">>,
                                value => maps:get(phone_t, User, <<>>),
                                editable => true
                            },
                            #{
                                id => <<"address">>,
                                type => text,
                                label => <<"Address">>,
                                value => maps:get(address_t, User, <<>>),
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
                value => nkadmin_webix_form:form(Spec, Type, Session)
            },
            {Data, Session};
        {error, Error} ->
            {error, Error}
    end.




%%
%%%% @doc
%%subview(?CHAT_MESSAGE, ObjId, Updates, Session) ->
%%    Base = nkadmin_util:make_id([?ID_ADMIN_DETAIL_OBJ_SUBVIEW, ?DOMAIN_USER, ObjId, ?CHAT_MESSAGE]),
%%    BaseId = <<Base/binary, "__table">>,
%%    Opts = #{
%%        table_id => BaseId,
%%        subdomains_id => nkdomain_admin_util:make_type_view_subfilter_id(?CHAT_MESSAGE),
%%        deleted_id => nkdomain_admin_util:make_type_view_showdeleted_id(?CHAT_MESSAGE),
%%        header => <<"MESSAGES">>
%%    },
%%    {Table, _Session2} = nkchat_message_obj_type_view:subview(Opts, <<>>, Session),
%%    Update = #{
%%        id => <<Base/binary, "__table_body">>,
%%        class => webix_ui,
%%        value => Table
%%    },
%%    {ok, [Update|Updates], Session}.



save(ObjId, Data, _Session) ->
    #{
        <<"username">> := UserName,
        <<"name">> := Name,
        <<"surname">> := SurName,
        <<"email">> := Email,
        <<"password">> := Pass,
        <<"address_t">> := Address,
        <<"phone_t">> := Phone
    } = Data,
    UserUpdate1 = #{
        name => Name,
        surname => SurName,
        email => Email,
        address_t => Address,
        phone_t => Phone
    },
    UserUpdate2 = case Pass of
        <<>> ->
            UserUpdate1;
        _ ->
            UserUpdate1#{password=>Pass}
    end,
    case nkdomain:update(ObjId, #{?DOMAIN_USER => UserUpdate2}) of
        {ok, _} ->
            case nkdomain:update_name(ObjId, UserName) of
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
    end.



%%subtables(FormId, #{obj_id:=ObjId}) ->
%%    #{
%%        view => <<"tabview">>,
%%        id => <<FormId/binary, "__tabview">>,
%%        gravity => 1.0,
%%        cells => [
%%            #{
%%                id => <<"user_sessions">>,
%%                header => <<"SESSIONS">>,
%%                template => <<"WIP SESSIONS...">>
%%            },
%%            #{
%%                id => <<"user_conversations">>,
%%                header => <<"CONVERSATIONS">>,
%%                template => <<"WIP CONVERSATIONS...">>
%%            },
%%            #{
%%                header => <<"MESSAGES">>,
%%                body => #{
%%                    id => get_subtable_id(ObjId, ?CHAT_MESSAGE),
%%                    rows => [
%%                        #{
%%                            id => get_subtable_id(ObjId, <<?CHAT_MESSAGE/binary, "__table_body">>),
%%                            template => <<>>
%%                        }
%%                    ]
%%                }
%%            },
%%            #{
%%                id => <<"user_roles">>,
%%                header => <<"ROLES">>,
%%                template => <<"WIP ROLES...">>
%%            },
%%            #{
%%                id => <<"user_files">>,
%%                header => <<"FILES">>,
%%                template => <<"WIP FILES...">>
%%            },
%%            #{
%%                id => <<"user_mails">>,
%%                header => <<"MAILS">>,
%%                template => <<"WIP MAILS...">>
%%            }
%%        ],
%%        tabbar => #{
%%            on => #{
%%                onChange => #{
%%                    nkParseFunction => <<"
%%                            function(newTab, oldTab) {
%%                                console.log('Selected tab: ', newTab, oldTab);
%%                                var oldTable = $$(oldTab + '__table');
%%                                if (oldTable && oldTable.hasOwnProperty('nkClearInterval')) {
%%                                    oldTable.nkClearInterval();
%%                                }
%%                                ncClient.sendMessageAsync('objects/admin.session/element_action', {
%%                                    element_id: newTab,
%%                                    action: 'selected'
%%                                }).then(function(response) {
%%                                    console.log('Tab selected OK: ', response);
%%                                    if (response.data && response.data.elements) {
%%                                        // Update view
%%                                        updateView(response.data.elements);
%%                                    }
%%                                }).catch(function(response) {
%%                                    console.log('Error at tabbar onChange: ', response);
%%                                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
%%                                });
%%                            }
%%                        ">>
%%                }
%%            }
%%        }
%%    }.
%%
%%
%%
%%get_subtable_id(ObjId, Key) ->
%%    nkadmin_util:make_id([?ID_ADMIN_DETAIL_OBJ_SUBVIEW, ?DOMAIN_USER, ObjId, Key]).
%%

%%get_subtable_id(FormId, Key) ->
%%    nkadmin_util:make_id([FormId, Key]).
