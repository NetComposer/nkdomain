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

%-define(ID, <<"domain_detail_form__user">>).
-define(ID_MESSAGES, <<"domain_detail_form__user__messages">>).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAN Admin User " ++ Txt, Args)).



%% @doc
view(#obj_id_ext{obj_id=ObjId, pid=Pid}, Session) ->
    Id = nkdomain_admin_util:make_view(?DOMAIN_USER, ObjId),
    case nkdomain:get_obj(Pid) of
        {ok, Obj} ->
            Data = #{
                id => Id,
                class => webix_ui,
                value => get_form(Id, Obj, Session)
            },
            {Data, Session};
        {error, Error} ->
            {error, Error}
    end.


save(ObjId, Data, _Session) ->
    #{
        <<"username">> := _UserName,
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
            ?LLOG(notice, "user ~s updated", [ObjId]),
            ok;
        {error, Error} ->
            ?LLOG(notice, "could not update user ~s: ~p", [ObjId, Error]),
            {error, Error}
    end.








get_form(FormId, Obj, Session) ->
    % TODO: Binaries VS Atoms
    CreatedBy = maps:get(created_by, Obj, <<>>),
    CreatedTime = maps:get(created_time, Obj, <<>>),
    _Description = maps:get(description, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, <<>>),
    _Name = maps:get(name, Obj, <<>>),
    _ObjId = maps:get(obj_id, Obj, <<>>),
    IconId = maps:get(icon_id, Obj, <<>>),
    ObjName = maps:get(obj_name, Obj, <<>>),
    _ParentId = maps:get(parent_id, Obj, <<>>),
    _Path = maps:get(path, Obj, <<>>),
    _Type = maps:get(type, Obj, <<>>),
    UpdatedBy = maps:get(updated_by, Obj, <<>>),
    UpdatedTime = maps:get(updated_time, Obj, <<>>),
    UserObj = maps:get(<<"user">>, Obj, #{}),
    UserEmail = maps:get(email, UserObj, <<>>),
    UserName = maps:get(name, UserObj, <<>>),
    UserSurname = maps:get(surname, UserObj, <<>>),
    UserPhoneT = maps:get(phone_t, UserObj, <<>>),
    UserAddressT = maps:get(address_t, UserObj, <<>>),
    Enabled = maps:get(enabled, Obj, <<"true">>),
    DomainUsers = nkdomain_util:class(?DOMAIN_USER),
    IconUrl = case IconId of
        <<>> ->
            <<"img/avatar.png">>;
        _ ->
            nkdomain_admin_util:get_file_url(IconId, Session)
    end,
    case Enabled == <<"true">> orelse Enabled == true of
        true ->
            DisableBtnHidden = false,
            EnableBtnHidden = true;
        false ->
            DisableBtnHidden = true,
            EnableBtnHidden = false
    end,
    IconImage = <<"<img class='photo' style='width:150px; height:150px;' src='", IconUrl/binary,"'/>">>,
    #{
        id => <<"body">>,
        type => <<"clean">>,
        height => <<"100%">>,
        margin => 0,
        borderless => false,
        rows => [#{
%           ACTION BUTTONS (DISABLE, ENABLE, DELETE, SAVE, ETC)
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
                    hidden => DisableBtnHidden,
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
                }, #{
                    id => <<"user_enable_button">>,
                    view => <<"button">>,
                    label => <<"Enable">>,
                    type => <<"iconButton">>,
                    icon => <<"circle-thin">>,
                    css => <<"webix_img_btn__centered">>,
                    align => <<"center">>,
                    hidden => EnableBtnHidden,
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
                }, #{
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
                }, #{
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
%       CURRENT OBJECT FORM
        }, #{
            id => <<"user_form">>,
            header => <<"USER DETAILS">>,
            type => <<"clean">>,
            gravity => 2.0,
            height => <<"100%">>,
            margin => 0,
            borderless => false,
            cols => [#{
                view => <<"layout">>,
                height => <<"100%">>,
                rows => [#{
                    height => 30
                }, #{
                    view => <<"label">>,
                    label => IconImage,
                    width => 200,
                    height => 150,
                    align => <<"center">>
                }, #{
                    % SPACER
                }]
            }, #{
                view => <<"form">>,
                height => <<"100%">>,
                scroll => <<"y">>,
                margin => 0,
                elements => [#{
                    minWidth => 300,
                    rows => [#{
                        template => <<"Credentials">>,
                        type => <<"section">>
                    }, #{
                        view => <<"combo">>,
                        id => <<"form_domain">>,
                        label => <<"Domain">>,
                        placeholder => <<"Domain">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => DomainId,
                        disabled => false, % true/false
                        hidden => false, % true/false
                        options => [ % TODO: Set the correct values
                            #{ id => <<"root">>, value => <<"/">> },
                            #{ id => <<"domain-XoAnWfVVVu8B0msXgmDxHXqTMbt">>, value => <<"/SIPSTORM/C4">> }
                        ]
                    }, #{
                        view => <<"text">>,
                        id => <<"form_username">>,
                        label => <<"Username">>,
                        placeholder => <<"username...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => ObjName
                    }, #{
                        view => <<"text">>,
                        id => <<"form_email">>,
                        label => <<"E-mail">>,
                        placeholder => <<"e-mail address...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserEmail
                    }, #{
                        view => <<"text">>,
                        id => <<"form_password">>,
                        label => <<"Password">>,
                        placeholder => <<"new password...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => <<"">>,
                        type => <<"password">>
                    }]
                }, #{
                    rows => [#{
                        template => <<"Personal data">>,
                        type => <<"section">>,
                        hidden => false
                    }, #{
                        view => <<"text">>,
                        id => <<"form_name">>,
                        label => <<"First name">>,
                        placeholder => <<"first name...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserName
                    }, #{
                        view => <<"text">>,
                        id => <<"form_surname">>,
                        label => <<"Last name">>,
                        placeholder => <<"last name...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserSurname
                    }, #{
                        view => <<"text">>,
                        id => <<"form_phone_t">>,
                        label => <<"Phone">>,
                        placeholder => <<"phone...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserPhoneT
                    }, #{
                        view => <<"text">>,
                        id => <<"form_address_t">>,
                        label => <<"Address">>,
                        placeholder => <<"address...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserAddressT
                    }]
                }, #{
                    rows => [#{
                        template => <<"Other">>,
                        type => <<"section">>
                    }, #{
% Can't show HTML code inside a text "value"
%                        view => <<"text">>,
%                        label => <<"Created by">>,
%                        labelWidth => 150,
%                        labelAlign => <<"left">>,
%                        value => <<"<a href=\"#/", DomainUsers/binary, "/", CreatedBy/binary, "\">", CreatedBy/binary, "</a>">>,
%                        disabled => true
                        cols => [#{
                            view => <<"label">>,
                            label => <<"Created by">>,
                            width => 150,
                            disabled => true
                        }, #{
                            view => <<"template">>,
                            template => <<"<a href=\"#/", DomainUsers/binary, "/", CreatedBy/binary, "\">", CreatedBy/binary, "</a>">>,
                            borderless => true,
                            autoheight => true
                        }]
                    }, #{
                        view => <<"text">>,
                        label => <<"Created time">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => CreatedTime,
                        on => #{
                            onBeforeRender => #{
                                nkParseFunction => <<"
                                    function(data) {   // 'en-US', 'es-ES', etc.
                                        if (data && data.value) {
                                            data.value = (new Date(data.value)).toLocaleString();
                                        }
                                    }
                                ">>
                            }
                        },
                        disabled => true
                    }, #{
                        cols => [#{
                            view => <<"label">>,
                            label => <<"Updated by">>,
                            width => 150,
                            disabled => true
                        }, #{
                            view => <<"template">>,
                            template => <<"<a href=\"#/", DomainUsers/binary, "/", UpdatedBy/binary, "\">", UpdatedBy/binary, "</a>">>,
                            borderless => true,
                            autoheight => true
                        }]
                    }, #{
                        view => <<"text">>,
                        label => <<"Updated time">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UpdatedTime,
                        on => #{
                            onBeforeRender => #{
                                nkParseFunction => <<"
                                    function(data) {   // 'en-US', 'es-ES', etc.
                                        if (data && data.value) {
                                            data.value = (new Date(data.value)).toLocaleString();
                                        }
                                    }
                                ">>
                            }
                        },
                        disabled => true
                    }]
            	}],
                elementsConfig => #{
                    labelAlign => <<"right">>
                }
            }]
%       TABVIEW WITH RELATED DATATABLES (DELETE IN CASE THERE ISN'T ANY)
        }, #{
            view => <<"tabview">>,
            id => <<FormId/binary, "__tabview">>,
          	gravity => 1.0,
            cells => [#{
                id => <<"user_sessions">>,
              	header => <<"SESSIONS">>,
                template => <<"WIP SESSIONS...">>
            }, #{
                id => <<"user_conversations">>,
              	header => <<"CONVERSATIONS">>,
                template => <<"WIP CONVERSATIONS...">>
        	}, #{
                header => <<"MESSAGES">>,
                body => #{
                    id => ?ID_MESSAGES,
                    rows => [#{
                        id => <<?ID_MESSAGES/binary, "__table_body">>,
                        template => <<>>
                    }]
                }
            }, #{
                id => <<"user_roles">>,
              	header => <<"ROLES">>,
                template => <<"WIP ROLES...">>
            }, #{
                id => <<"user_files">>,
              	header => <<"FILES">>,
                template => <<"WIP FILES...">>
            }, #{
                id => <<"user_mails">>,
              	header => <<"MAILS">>,
                template => <<"WIP MAILS...">>
            }],
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
        }]
    }.