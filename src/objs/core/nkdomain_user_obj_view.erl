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

-export([view/2, save/3, subview/4]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAN Admin User " ++ Txt, Args)).

-define(CHAT_MESSAGE, <<"message">>).


%% @doc
view(#obj_id_ext{obj_id=ObjId, pid=Pid}, Session) ->
    FormId = nkdomain_admin_util:make_obj_view_id(?DOMAIN_USER, ObjId),
    case nkdomain:get_obj(Pid) of
        {ok, Obj} ->
            Data = #{
                id => FormId,
                class => webix_ui,
                value => get_form(FormId, Obj, Session)
            },
            {Data, Session};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
subview(?CHAT_MESSAGE, ObjId, Updates, Session) ->
    Base = nkadmin_util:make_id([?ADMIN_DETAIL_OBJ_SUBVIEW, ?DOMAIN_USER, ObjId, ?CHAT_MESSAGE]),
    Opts = #{table_id => <<Base/binary, "__table">>, header => <<"MESSAGES">>},
    {Table, _Session2} = nkchat_message_obj_type_view:subview(Opts, <<>>, Session),
    Update = #{
        id => <<Base/binary, "__table_body">>,
        class => webix_ui,
        value => Table
    },
    {ok, [Update|Updates], Session}.



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


get_form(FormId, Obj, Session) ->
    #{
        id => <<"body">>,
        view => <<"accordion">>,
        type => <<"clean">>,
        height => <<"100%">>,
        margin => 0,
        borderless => false,
        multi => <<"mixed">>,
        rows => [
            buttons(FormId, Obj),
            #{
                header => <<"USER DETAILS">>,
                body => form(FormId, Obj, Session)                
            },
            #{
                view => <<"resizer">>
            },
            #{
                header => <<"SUBTABLES">>,
                body => subtables(FormId, Obj)
            },
            #{
                gravity => 0.0
            }
        ]
    }.


buttons(FormId, Obj) ->
    Enabled = nklib_util:to_binary(maps:get(enabled, Obj, true)),
    #{
        id => <<"user_buttons">>,
        type => <<"space">>,
        view => <<"layout">>,
        cols => [
            #{
                id => <<"user_disable_button">>,
                view => <<"button">>,
                label => <<"Disable">>,
                type => <<"iconButton">>,
                icon => <<"ban">>,
                css => <<"webix_img_btn__centered">>,
                align => <<"center">>,
                hidden => (Enabled == <<"false">>),
                click => #{
                    nkParseFunction => <<"
                            function() {
                                ncClient.sendMessageAsync(\"objects/admin.session/element_action\", {
                                    element_id: \"", FormId/binary, "\",
                                    action: \"disable\"
                                }).then(function(response) {
                                    console.log('Disable button clicked OK: ', response);
                                    if (response.data && response.data.elements) {
                                        // Update view
                                        updateView(response.data.elements);
                                    }
                                }).catch(function(response) {
                                    console.log('Error at disable button clicked: ', response);
                                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
                                });
                            }
                        ">>
                }
            },
            #{
                id => <<"user_enable_button">>,
                view => <<"button">>,
                label => <<"Enable">>,
                type => <<"iconButton">>,
                icon => <<"circle-thin">>,
                css => <<"webix_img_btn__centered">>,
                align => <<"center">>,
                hidden => (Enabled == <<"true">>),
                click => #{
                    nkParseFunction => <<"
                            function() {
                                ncClient.sendMessageAsync(\"objects/admin.session/element_action\", {
                                    element_id: \"", FormId/binary, "\",
                                    action: \"enable\"
                                }).then(function(response) {
                                    console.log('Enable button clicked OK: ', response);
                                    if (response.data && response.data.elements) {
                                        // Update view
                                        updateView(response.data.elements);
                                    }
                                }).catch(function(response) {
                                    console.log('Error at enable button clicked: ', response);
                                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
                                });
                            }
                        ">>
                }
            },
            #{
                id => <<"user_delete_button">>,
                view => <<"button">>,
                label => <<"Delete">>,
                type => <<"iconButton">>,
                icon => <<"trash">>,
                css => <<"webix_img_btn__centered">>,
                align => <<"center">>,
                hidden => false,
                click => #{
                    nkParseFunction => <<"
                            function() {
                                ncClient.sendMessageAsync(\"objects/admin.session/element_action\", {
                                    element_id: \"", FormId/binary, "\",
                                    action: \"delete\"
                                }).then(function(response) {
                                    console.log('Delete button clicked OK: ', response);
                                    if (response.data && response.data.elements) {
                                        // Update view
                                        updateView(response.data.elements);
                                    }
                                }).catch(function(response) {
                                    console.log('Error at delete button clicked: ', response);
                                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
                                });
                            }
                        ">>
                }
            },
            #{
                id => <<"user_save_button">>,
                view => <<"button">>,
                label => <<"Save changes">>,
                type => <<"iconButton">>,
                icon => <<"floppy-o">>,
                css => <<"webix_img_btn__centered">>,
                align => <<"center">>,
                disabled => false,
                hidden => false,
                click => #{
                    nkParseFunction => <<"
                            function() {
                                var domain = $$(\"form_domain\").getValue();
                                var username = $$(\"form_username\").getValue();
                                var email = $$(\"form_email\").getValue();
                                var password = $$(\"form_password\").getValue();
                                var name = $$(\"form_name\").getValue();
                                var surname = $$(\"form_surname\").getValue();
                                var phone_t = $$(\"form_phone_t\").getValue();
                                var address_t = $$(\"form_address_t\").getValue();
                                ncClient.sendMessageAsync(\"objects/admin.session/element_action\", {
                                    element_id: \"", FormId/binary, "\",
                                    action: \"save\",
                                    value: {
                                        domain: domain,
                                        username: username,
                                        email: email,
                                        password: password,
                                        name: name,
                                        surname: surname,
                                        phone_t: phone_t,
                                        address_t: address_t
                                    }
                                }).then(function(response) {
                                    console.log('Save button clicked OK: ', response);
                                    if (response.data && response.data.elements) {
                                        // Update view
                                        updateView(response.data.elements);
                                    }
                                }).catch(function(response) {
                                    console.log('Error at save button clicked: ', response);
                                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
                                });
                            }
                        ">>
                }
            }
        ]
    }.


form(_FormId, Obj, Session) ->
    #{
        domain_id := DomainId,
        obj_name := ObjName,
        created_by := CreatedBy,
        created_time := CreatedTime,
        updated_by := UpdatedBy,
        updated_time := UpdatedTime,
        ?DOMAIN_USER := UserObj
    } = Obj,
    lager:error("NKLOG UP ~p", [UpdatedBy]),
    IconId = maps:get(icon_id, Obj, <<>>),
    IconUrl = case IconId of
        <<>> ->
            <<"img/avatar.png">>;
        _ ->
            nkdomain_admin_util:get_file_url(IconId, Session)
    end,
    IconImage = <<"<img class='photo' style='width:150px; height:150px;' src='", IconUrl/binary, "'/>">>,
    UserEmail = maps:get(email, UserObj, <<>>),
    UserName = maps:get(name, UserObj, <<>>),
    UserSurname = maps:get(surname, UserObj, <<>>),
    UserPhoneT = maps:get(phone_t, UserObj, <<>>),
    UserAddressT = maps:get(address_t, UserObj, <<>>),
    #{
        id => <<"user_form">>,
        header => <<"USER DETAILS">>,
        type => <<"clean">>,
        gravity => 2.0,
        height => <<"100%">>,
        margin => 0,
        borderless => false,
        cols => [
            #{
                view => <<"layout">>,
                height => <<"100%">>,
                rows => [
                    #{
                        height => 30
                    },
                    #{
                        view => <<"label">>,
                        label => IconImage,
                        width => 200,
                        height => 150,
                        align => <<"center">>
                    },
                    #{
                        % SPACER
                    }
                ]
            },
            #{
                view => <<"form">>,
                height => <<"100%">>,
                scroll => <<"y">>,
                margin => 0,
                elements => [
                    #{
                        minWidth => 300,
                        rows => [
                            #{
                                template => <<"Credentials">>,
                                type => <<"section">>
                            },
                            #{
                                view => <<"combo">>,
                                id => <<"form_domain">>,
                                label => <<"Domain">>,
                                placeholder => <<"Domain">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => DomainId,
                                disabled => false, % true/false
                                hidden => false, % true/false
                                options => [% TODO: Set the correct values
                                            #{id => <<"root">>, value => <<"/">>},
                                            #{id => <<"domain-XoAnWfVVVu8B0msXgmDxHXqTMbt">>, value => <<"/SIPSTORM/C4">>}
                                ]
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_username">>,
                                label => <<"Username">>,
                                placeholder => <<"username...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => ObjName
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_email">>,
                                label => <<"E-mail">>,
                                placeholder => <<"e-mail address...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => UserEmail
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_password">>,
                                label => <<"Password">>,
                                placeholder => <<"new password...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => <<"">>,
                                type => <<"password">>
                            }
                        ]
                    },
                    #{
                        rows => [
                            #{
                                template => <<"Personal data">>,
                                type => <<"section">>,
                                hidden => false
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_name">>,
                                label => <<"First name">>,
                                placeholder => <<"first name...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => UserName
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_surname">>,
                                label => <<"Last name">>,
                                placeholder => <<"last name...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => UserSurname
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_phone_t">>,
                                label => <<"Phone">>,
                                placeholder => <<"phone...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => UserPhoneT
                            },
                            #{
                                view => <<"text">>,
                                id => <<"form_address_t">>,
                                label => <<"Address">>,
                                placeholder => <<"address...">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => UserAddressT
                            }
                        ]
                    },
                    #{
                        rows => [
                            #{
                                template => <<"Other">>,
                                type => <<"section">>
                            },
                            #{
                                % Can't show HTML code inside a text "value"
                                %                        view => <<"text">>,
                                %                        label => <<"Created by">>,
                                %                        labelWidth => 150,
                                %                        labelAlign => <<"left">>,
                                %                        value => <<"<a href=\"#/", DomainUsers/binary, "/", CreatedBy/binary, "\">", CreatedBy/binary, "</a>">>,
                                %                        disabled => true
                                cols => [
                                    #{
                                        view => <<"label">>,
                                        label => <<"Created by">>,
                                        width => 150,
                                        disabled => true
                                    },
                                    #{
                                        view => <<"template">>,
                                        template => nkdomain_admin_util:obj_id_url(CreatedBy),
                                        borderless => true,
                                        autoheight => true
                                    }
                                ]
                            },
                            #{
                                view => <<"text">>,
                                label => <<"Created time">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => CreatedTime,
                                on => #{
                                    onBeforeRender => #{
                                        nkParseFunction => <<"
                                    function(data) {   // 'en-US', 'es-ES', etc.
                                        if (data && data.value && typeof data.value === 'number') {
                                            data.value = (new Date(data.value)).toLocaleString();
                                        }
                                    }
                                    ">>
                                    }
                                },
                                disabled => true
                            },
                            #{
                                cols => [
                                    #{
                                        view => <<"label">>,
                                        label => <<"Updated by">>,
                                        width => 150,
                                        disabled => true
                                    },
                                    #{
                                        view => <<"template">>,
                                        template => nkdomain_admin_util:obj_id_url(UpdatedBy),
                                        borderless => true,
                                        autoheight => true
                                    }
                                ]
                            },
                            #{
                                view => <<"text">>,
                                label => <<"Updated time">>,
                                labelWidth => 150,
                                labelAlign => <<"left">>,
                                value => UpdatedTime,
                                on => #{
                                    onBeforeRender => #{
                                        nkParseFunction => <<"
                                    function(data) {   // 'en-US', 'es-ES', etc.
                                        if (data && data.value && typeof data.value === 'number') {
                                            data.value = (new Date(data.value)).toLocaleString();
                                        }
                                    }
                                    ">>
                                    }
                                },
                                disabled => true
                            }
                        ]
                    }
                ],
                elementsConfig => #{
                    labelAlign => <<"right">>
                }
            }
        ]
    }.



subtables(FormId, #{obj_id:=ObjId}) ->
    #{
        view => <<"tabview">>,
        id => <<FormId/binary, "__tabview">>,
        gravity => 1.0,
        cells => [
            #{
                id => <<"user_sessions">>,
                header => <<"SESSIONS">>,
                template => <<"WIP SESSIONS...">>
            },
            #{
                id => <<"user_conversations">>,
                header => <<"CONVERSATIONS">>,
                template => <<"WIP CONVERSATIONS...">>
            },
            #{
                header => <<"MESSAGES">>,
                body => #{
                    id => get_subtable_id(ObjId, ?CHAT_MESSAGE),
                    rows => [
                        #{
                            id => get_subtable_id(ObjId, <<?CHAT_MESSAGE/binary, "__table_body">>),
                            template => <<>>
                        }
                    ]
                }
            },
            #{
                id => <<"user_roles">>,
                header => <<"ROLES">>,
                template => <<"WIP ROLES...">>
            },
            #{
                id => <<"user_files">>,
                header => <<"FILES">>,
                template => <<"WIP FILES...">>
            },
            #{
                id => <<"user_mails">>,
                header => <<"MAILS">>,
                template => <<"WIP MAILS...">>
            }
        ],
        tabbar => #{
            on => #{
                onChange => #{
                    nkParseFunction => <<"
                            function(newTab, oldTab) {
                                console.log('Selected tab: ', newTab, oldTab);
                                var oldTable = $$(oldTab + '__table');
                                if (oldTable && oldTable.hasOwnProperty('nkClearInterval')) {
                                    oldTable.nkClearInterval();
                                }
                                ncClient.sendMessageAsync('objects/admin.session/element_action', {
                                    element_id: newTab,
                                    action: 'selected'
                                }).then(function(response) {
                                    console.log('Tab selected OK: ', response);
                                    if (response.data && response.data.elements) {
                                        // Update view
                                        updateView(response.data.elements);
                                    }
                                }).catch(function(response) {
                                    console.log('Error at tabbar onChange: ', response);
                                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
                                });
                            }
                        ">>
                }
            }
        }
    }.



get_subtable_id(ObjId, Key) ->
    nkadmin_util:make_id([?ADMIN_DETAIL_OBJ_SUBVIEW, ?DOMAIN_USER, ObjId, Key]).


%%get_subtable_id(FormId, Key) ->
%%    nkadmin_util:make_id([FormId, Key]).
