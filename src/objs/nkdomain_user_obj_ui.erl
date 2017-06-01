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

-module(nkdomain_user_obj_ui).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([table/0]).


table() ->
    #{
        view => <<"scrollview">>,
        id => <<"body">>,
        borderless => true,
        type => <<"space">>,
        css => <<"flex-tmp">>,
        scroll => <<"xy">>,
        body => #{
            rows => [objects_table()]
        }
    }.


objects_table() ->
    #{
        id => <<"objectsTable">>,
        type => <<"space">>,
        minHeight => 300,
        minWidth => 400,
        rows => [
            #{
                height => 40,
                cols => [
                    #{
                        view => <<"button">>,
                        type => <<"iconButton">>,
                        icon => <<"refresh">>,
                        autowidth => true,
                        label => <<"Refresh">>,
                        click => <<"function () {
                                    var grid = $$(\"objectsData\");
                                    grid.showProgress();
                                    webix.delay(function () {
                                        grid.hideProgress();
                                    }, null, null, 300);
                                    }">>
                    }
                ]
            },
            #{
                rows => [
                    % create default objects table data,
                    create_default_objects_table_data(),
                    #{
                        view => <<"toolbar">>,
                        css => <<"highlighted_header header6">>,
                        paddingX => 5,
                        paddingY => 5,
                        height => 40,
                        cols => [
                            #{
                                view => <<"pager">>,
                                id => <<"pagerA">>,
                                template => <<"{common.first()}{common.prev()}&nbsp; {common.pages()}&nbsp; {common.next()}{common.last()}">>,
                                autosize => true,
                                height => 35,
                                group => 5
                            }
                        ]
                    }
                ]
            }
        ]
    }.



create_default_objects_table_data() ->
    #{
        id => <<"objectsData">>,
        view => <<"datatable">>,
        select => true,
        editable => false,
        columns => [
            #{
                id => <<"id">>, header => [#{ text => <<"#">>, height => 20 }, #{ content => <<"customFilter">> } ], sort => <<"int">>, width => 70
            },
            #{
                id => <<"uuid">>, header => [<<"UUID">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 80, fillspace => 1
            },
            #{
                id => <<"parentUuid">>, header => [<<"Parent UUID">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 80, fillspace => 1
            },
            #{
                id => <<"typeName">>, header => [<<"Type">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 120, fillspace => 2, editor => <<"select">>, template => <<"<div class='type#type#'>#typeName#</div>">>
            },
            #{
                id => <<"shortName">>, header => [<<"Name">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 120, fillspace => 2, editor => <<"text">>
            },
            #{
                id => <<"enabled">>, header => [<<"Enabled?">>, <<"">>], sort => <<"string">>, minWidth => 50, fillspace => 1, template => <<"<span  style='cursor:pointer;' class='webix_icon #enabledIcon#'></span>">>
            },
            #{
                id => <<"view">>, header => <<"&nbsp;">>, width => 35, template => <<"<span style='cursor:pointer;' class='webix_icon fa-eye'></span>">>
            }
        ],
        pager => <<"pagerA">>,
        export => true,
        data => create_objects_array(150, []),
        url => <<"wsProxy->">>,
        save => <<"wsProxy->">>,
        onClick => #{
            <<"fa-eye">> => <<"function (e, id, node) {
                console.log('Redirect user to the object selected: ' + id);
            }">>,
            <<"fa-check">> => <<"function (e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be disabled. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function (res) {
                        if (res) {
                            var item = webix.$$(\"objectsData\").getItem(id);
                            item.enabled = false;
                            item.enabledIcon = \"fa-times\";
                            webix.$$(\"objectsData\").refresh(id);
                        }
                    }
                });
            }">>,
            <<"fa-times">> => <<"function (e, id, node) {
                webix.confirm({
                    text: \"This object will be enabled. <br/> Are you sure?\",
                    ok: \"Yes\",
                    cancel: \"Cancel\",
                    callback: function (res) {
                        if (res) {
                            var item = webix.$$(\"objectsData\").getItem(id);
                            item.enabled = true;
                            item.enabledIcon = \"fa-check\";
                            webix.$$(\"objectsData\").refresh(id);
                        }
                    }
                });
            }">>
        },
        ready => <<"function () {
            webix.extend(this, webix.ProgressBar);
        }">>,
        on => #{
            <<"onBeforeLoad">> => <<"function() {
                webix.ui.datafilter.customFilter = {
                    refresh: function(master, node, column) {
                        node.onchange = function() {};
                        node.onclick = function(e) {
                            // Prevent the column from changing the order when clicking the filter
                            e.stopPropagation();
                        };
                    },
                    render: function(a, b) {
                        return  \"<select style='width:100%; height:25px; font-family:Verdana'; id=\"+b.columnId+\">\" +
                                \"<option>Old</option>\" +
                                \"<option>New</option>\" +
                                \"</select>\";
                    }
                };
                webix.ui.datafilter.extendedFilter = webix.extend({
                    refresh:function(master, node, column){
                        //event handlers
                        node.onclick = function(e) {
                            // Prevent the column from changing the order when clicking the filter
                            e.stopPropagation();
                        };
                        node.onkeyup = function(){
                            let input = this.children[0].children[0];
                            if (input.prevValue !== input.value) {
                                console.log('Filter ' + column.columnId + ' changed: ' + input.value);
                                master.clearAll();
                                let newObj =
                                {
                                    id: 1,
                                    uuid: 123456789,
                                    parentUuid: 987654321,
                                    type: 0,
                                    typeName: \"User\",
                                    shortName: \"user\",
                                    enabled: true,
                                    enabledIcon: \"fa-check\"
                                };
                                if (column.columnId === 'id') {
                                    newObj.id = input.value;
                                } else if (column.columnId === 'uuid') {
                                    newObj.uuid = input.value;
                                } else if (column.columnId === 'parentUuid') {
                                    newObj.parentUuid = input.value;
                                } else if (column.columnId === 'typeName') {
                                    newObj.typeName = input.value;
                                } else if (column.columnId === 'shortName') {
                                    newObj.shortName = input.value;
                                }
                                master.add(newObj, 0);
                            };
                            input.prevValue = input.value;
                        }
                    }
                }, webix.ui.datafilter.textFilter);
            }">>
        }
    }.

create_objects_array(N, T) when (N =< 0) ->
    T;
create_objects_array(N, T) ->
    UUID = 999999999+N,
    ParentUUID = UUID - 1,
    Type = UUID rem 5,
    Enabled = ((UUID rem 5) rem 2) =:= 0,
    if
        Enabled ->
            Icon = <<"fa-check">>;
        true ->
            Icon = <<"fa-times">>
    end,
    case Type of
        0 -> TypeName = <<"User">>,
            ShortName = list_to_binary([<<"user">>,list_to_binary(integer_to_list(UUID))]);
        1 -> TypeName = <<"File">>,
            ShortName = list_to_binary([<<"file">>,list_to_binary(integer_to_list(UUID))]);
        2 -> TypeName = <<"Node">>,
            ShortName = list_to_binary([<<"node">>,list_to_binary(integer_to_list(UUID))]);
        3 -> TypeName = <<"User session">>,
            ShortName = list_to_binary([<<"user session">>,list_to_binary(integer_to_list(UUID))]);
        _ -> TypeName = <<"Service">>,
            ShortName = list_to_binary([<<"service">>,list_to_binary(integer_to_list(UUID))])
    end,
    %% io:format(ShortName),
    create_objects_array (N-1, [
        #{
            id => N,
            uuid => UUID,
            parentUuid => ParentUUID,
            type => Type,
            typeName => TypeName,
            shortName => ShortName,
            enabled => Enabled,
            enabledIcon => Icon
        }
        | T]).