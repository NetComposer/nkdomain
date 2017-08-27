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

-export([view/2]).

-include("nkdomain.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

-define(ID, <<"domain_detail_form__user">>).
-define(ID_MESSAGES, <<"domain_detail_form__user__messages">>).



%% @doc
view(#obj_id_ext{obj_id=ObjId, pid=Pid}, Session) ->
    case nkdomain:get_obj(Pid) of
        {ok, Obj} ->
            Data = #{
                id => <<?ID/binary, "__", ObjId/binary>>,
                class => webix_ui,
                value => get_form(Obj, Session)
            },
            {Data, Session};
        {error, Error} ->
            {error, Error}
    end.

get_form(Obj, Session) ->
    % TODO: Binaries VS Atoms
    CreatedBy = maps:get(created_by, Obj, <<>>),
    CreatedTime = maps:get(created_time, Obj, <<>>),
    _Description = maps:get(description, Obj, <<>>),
    _DomainId = maps:get(domain_id, Obj, <<>>),
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
    DomainUsers = nkdomain_util:class(?DOMAIN_USER),
    IconUrl = case IconId of
        <<>> ->
            <<"img/avatar.png">>;
        _ ->
            nkdomain_admin_util:get_file_url(IconId, Session)
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
                    hidden => false,
                    click => #{
                        nkParseFunction => <<"
                            function() {
                                alert('Disable object');
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
                    hidden => true,
                    click => #{
                        nkParseFunction => <<"
                            function() {
                                alert('Enable object');
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
                                alert('Delete object');
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
                    disabled => true,
                    hidden => false,
                    click => #{
                        nkParseFunction => <<"
                            function() {
                                alert('Save object');
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
            gravity => 1.0,
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
                        view => <<"text">>,
                        label => <<"Username">>,
                        placeholder => <<"username...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => ObjName,
                        disabled => true
                    }, #{
                        view => <<"text">>,
                        label => <<"E-mail">>,
                        placeholder => <<"e-mail address...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserEmail
                    }, #{
                        view => <<"text">>,
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
                        label => <<"First name">>,
                        placeholder => <<"first name...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserName
                    }, #{
                        view => <<"text">>,
                        label => <<"Last name">>,
                        placeholder => <<"last name...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserSurname
                    }, #{
                        view => <<"text">>,
                        label => <<"Phone">>,
                        placeholder => <<"phone...">>,
                        labelWidth => 150,
                        labelAlign => <<"left">>,
                        value => UserPhoneT
                    }, #{
                        view => <<"text">>,
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
                        value => CreatedTime, %% TODO: Format to client date string
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
                        value => UpdatedTime, %% TODO: Format to client date string
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
            id => <<?ID/binary, "__tabview">>,
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