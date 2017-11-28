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

%% @doc NkDomain service callback module
-module(nkdomain_admin_detail).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([element_action/5]).
-export([selected_obj/3, selected_type/4]).


-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args, Session),
    lager:Type("NkDOMAN Admin (~s) " ++ Txt, [Session#admin_session.session_id|Args])).


%% ===================================================================
%% Callbacks
%% ===================================================================

%% @doc
element_action([?ADMIN_OBJ_TYPE, Path, Type], selected, _Value, Updates, Session) ->
    selected_type(Type, Path, Updates, Session);

element_action([?ADMIN_OBJ_ID, _SrvId, ObjId, Type, Path], selected, Value, Updates, Session) ->
    case selected_obj(ObjId, Type, Path, Updates, Session) of
        {ok, Updates2, Session2} ->
            case Value of
                #{update_url:=true} ->
                    {Updates3, Session3} = nkadmin_util:update_url(Updates2, Session2),
                    {ok, Updates3, Session3};
                _ ->
                    {ok, Updates2, Session2}
            end;
        Other ->
            Other
    end;

element_action([?ADMIN_TREE_SERVICES, Service], selected, _Value, Updates, Session) ->
    lager:info("NKLOG Admin Service ~p selected", [Service]),
    Detail = get_dash_detail_test(),
    Item = #{
        class => detail,
        id => detail,
        value => Detail
    },
    {ok, [Item|Updates], Session};

element_action([?ADMIN_DETAIL_TYPE_VIEW, Type], updated, Value, Updates, Session) ->
    #{
        <<"obj_id">> := ObjId,
        <<"value">> := ObjValue
    } = Value,
    {ok, Mod} = nkdomain_admin_util:get_type_view_mod(Type, Session),
    case Mod:element_updated(ObjId, ObjValue, Session) of
        {ok, Update} ->
            case nkdomain:update(ObjId, Update) of
                {ok, _} ->
                    {ok, Updates, Session};
                {error, Error} ->
                    ?LLOG(warning, "Object update error: ~p", [Error], Session),
                    {ok, Updates, Session}
            end
    end;

element_action([?ADMIN_DETAIL_TYPE_VIEW, _Type], enable, #{<<"ids">>:=Ids}, Updates, Session) ->
    {Updates2, Session2} = type_view_enable(true, Ids, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_TYPE_VIEW, _Type], disable, #{<<"ids">>:=Ids}, Updates, Session) ->
    {Updates2, Session2} = type_view_enable(false, Ids, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_TYPE_VIEW, _Type], delete, #{<<"ids">>:=Ids}, Updates, Session) ->
    {Updates2, Session2} = type_view_delete(Ids, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_TYPE_VIEW, Type], new, _Value, Updates, Session) ->
    {Updates2, Session2} = type_view_new(Type, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_OBJ_VIEW, _Type, ObjId], enable, _Value, Updates, Session) ->
    _ = type_view_enable(true, [ObjId], Updates, Session),
    {ok, Updates2, Session2} = selected_obj(ObjId, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_OBJ_VIEW, _Type, ObjId], disable, _Value, Updates, Session) ->
    _ = type_view_enable(false, [ObjId], Updates, Session),
    {ok, Updates2, Session2} = selected_obj(ObjId, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_OBJ_VIEW, Type, ObjId], delete, _Value, Updates, #admin_session{base_path=Path} = Session) ->
    _ = type_view_delete([ObjId], Updates, Session),
    {ok, Updates2, Session2} = selected_type(Type, Path, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_OBJ_VIEW, Type, ObjId], save, Value, Updates, #admin_session{base_path=Path} = Session) ->
    obj_view_save(Type, ObjId, Value, Session),
    {ok, Updates2, Session2} = selected_type(Type, Path, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ADMIN_DETAIL_OBJ_SUBVIEW, Type, ObjId, SubType], selected, _Value, Updates, Session) ->
    obj_view_subview(Type, ObjId, SubType, Updates, Session);


%%element_action([?ADMIN_DETAIL_OBJ_VIEW, <<"user">>, _Id, <<"messages">>]=M, selected, _Value, Updates, Session) ->
%%    lager:error("NKLOG SELECTEC"),
%%    M1 = nkadmin_util:make_id(M),
%%    Opts = #{table_id => <<M1/binary, "__table">>, header => <<"MESSAGES">>},
%%    {Table, _Session2} = nkchat_message_obj_type_view:subview(Opts, Session),
%%    Update = #{
%%        id => <<M1/binary, "__table_body">>,
%%        class => webix_ui,
%%        value => Table
%%    },
%%    {ok, [Update|Updates], Session};

element_action(_Elements, _Action, _Value, Updates, Session) ->
    lager:error("NKLOG Admin Unhandled Element Action ~p", [{_Elements, _Action, _Value}]),
    {ok, Updates, Session}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
selected_type(Type, Path, Updates, Session) ->
    Class = nkdomain_util:class(Type),
    DetailPath = case Type of
        ?DOMAIN_DOMAIN -> Path;
        _ -> nkdomain_util:append(Path, Class)
    end,
    case nkdomain_admin_util:get_type_info(Type, Session) of
        {true, #{type_view_mod:=Mod}} ->
            {Detail, Session2} = Mod:view(Path, Session),
            {Updates3, Session3} = nkadmin_util:update_detail(DetailPath, Detail, Updates, Session2),
            {Updates4, Session4} = nkadmin_util:update_url(Updates3, Session3),
            {ok, Updates4, Session4};
        _ ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {Updates2, Session2} = nkadmin_util:update_detail(DetailPath, #{}, Updates, Session),
            {ok, Updates2, Session2}
    end.


%% @doc
selected_obj(ObjId, Updates, Session) ->
    case nkdomain_db:find(ObjId) of
        #obj_id_ext{type=Type, path=Path} ->
            selected_obj(ObjId, Type, Path, Updates, Session);
        {error, Error} ->
            ?LLOG(notice, "error reading object ~s (~p)", [ObjId, Error], Session),
            {ok, Updates, Session}
    end.


%% @doc
selected_obj(_ObjId, ?DOMAIN_DOMAIN, ObjPath, Updates, Session) ->
    selected_type(?DOMAIN_DOMAIN, ObjPath, Updates, Session);

selected_obj(ObjId, Type, ObjPath, Updates, Session) ->
    case nkdomain_admin_util:get_obj_view_mod(Type, Session) of
        {ok, Mod} ->
            case nkdomain_db:load(ObjId) of
                #obj_id_ext{}=ObjIdExt ->
                    {Detail, Session2} = Mod:view(ObjIdExt, Session),
                    {Updates3, Session3} = nkadmin_util:update_detail(ObjPath, Detail, Updates, Session2),
                    {ok, Updates3, Session3};
                {error, Error} ->
                    ?LLOG(notice, "error loading object ~s: ~p", [ObjPath, Error], Session),
                    {error, object_load_error, Session}
            end;
        not_found ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {ok, Updates, Session}
    end.


%% ===================================================================
%% Util
%% ===================================================================




%% ===================================================================
%% Type View
%% ===================================================================

%% @private
type_view_enable(_Enable, [], Updates, Session) ->
    {Updates, Session};

type_view_enable(Enable, [Id|Rest], Updates, Session) ->
    case nkdomain:enable(Id, Enable) of
        ok ->
            ?LLOG(info, "object enabled (~p): ~s", [Enable, Id], Session),
            ok;
        {error, Error} ->
            ?LLOG(warning, "could not enable ~p: ~p", [Id, Error], Session)
    end,
    type_view_enable(Enable, Rest, Updates, Session).


%% @private
type_view_delete([], Updates, Session) ->
    {Updates, Session};

type_view_delete([Id|Rest], Updates, Session) ->
    case nkdomain:delete(Id) of
        ok ->
            ok;
        {error, Error} ->
            ?LLOG(warning, "could not delete ~s: ~p", [Id, Error], Session)
    end,
    type_view_delete( Rest, Updates, Session).


%% @private
obj_view_save(Type, ObjId, Data, Session) ->
    {ok, Mod} = nkdomain_admin_util:get_obj_view_mod(Type, Session),
    Mod:save(ObjId, Data, Session).


%% @private
type_view_new(Type, Updates, Session) ->
    case nkdomain_admin_util:get_obj_view_mod(Type, Session) of
        {ok, Mod} ->
            {Path, Detail, Session2} = Mod:new(Session),
            {Updates3, Session3} = nkadmin_util:update_detail(Path, Detail, Updates, Session2),
            {ok, Updates3, Session3};
        not_found ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {ok, Updates, Session}
    end.


obj_view_subview(Type, ObjId, SubType, Updates, Session) ->
    case nkdomain_admin_util:get_obj_view_mod(Type, Session) of
        {ok, Mod} ->
            {ok, Updates2, Session2} = Mod:subview(SubType, ObjId, Updates, Session),
            {ok, Updates2, Session2};
        not_found ->
            ?LLOG(notice, "type ~s with no supported subview: ~s", [Type, SubType], Session),
            {ok, Updates, Session}
    end.




get_dash_detail_test() ->
    #{
        class => <<"webix_ui">>,
        id => <<"detail_test">>,
        value => #{
            view => <<"scrollview">>,
            type => <<"clean">>,
            scroll => <<"xy">>,
            id => <<"body">>,
            css => <<"flex-tmp">>,
            borderless => true,
            body => #{
                type => <<"clean">>,
                rows => [
                    get_dashline(),
                    #{
                        type => <<"clean">>,
                        rows => [#{
                            height => 300,
                            type => <<"clean">>,
                            cols => [
                                get_series_chart_json(#{
                                    id => <<"activity_stats_line_chart">>,
                                    header => #{
                                        text => <<"Activity Stats">>,
                                        css => <<"chart_header">>
                                    },
                                    type => <<"line">>,
                                    x => #{
                                        value => <<"month">>,
                                        formatter => nkadmin_webix_chart:parse_date_template(<<"month">>, <<"{month: 'short'}">>)
                                    },
                                    y => #{
                                        values => [#{
                                            value => <<"messages">>,
                                            value_hashed => <<"#messages#">>,
                                            text => <<"Messages">>,
                                            color => <<"#73cfd8">>
                                        },
                                        #{
                                            value => <<"audio">>,
                                            value_hashed => <<"#audio#">>,
                                            text => <<"Calls">>,
                                            color => <<"#98d774">>
                                        },
                                        #{
                                            value => <<"video">>,
                                            value_hashed => <<"#video#">>,
                                            text => <<"Video Calls">>,
                                            color => <<"#ff665f">>
                                        }]
                                    },
                                    dynamic => false
                                })
                            ]
                        }, #{
                            height => 300,
                            type => <<"clean">>,
                            cols => [
                                get_chart_json(#{
                                    id => <<"video_calls_barh_chart">>,
                                    header => #{
                                        text => <<"Video Calls & Calls">>,
                                        css => <<"chart_header">>
                                    },
                                    type => <<"barH">>,
                                    units => <<"(mins)">>,
                                    x => #{
                                        value => <<"minutes">>
                                    },
                                    y => #{
                                        value => <<"type">>
                                    },
                                    dynamic => false
                                }),
                                get_chart_json(#{
                                    id => <<"sent_files_barh_chart">>,
                                    header => #{
                                        text => <<"Sent Files">>,
                                        css => <<"chart_header">>
                                    },
                                    type => <<"barH">>,
                                    units => <<"(Q)">>,
                                    x => #{
                                        value => <<"number">>
                                    },
                                    y => #{
                                        value => <<"type">>
                                    },
                                    dynamic => false
                                }),
                                get_chart_json(#{
                                    id => <<"conversations_barh_chart">>,
                                    header => #{
                                        text => <<"Conversations">>,
                                        css => <<"chart_header">>
                                    },
                                    type => <<"barH">>,
                                    units => <<"(Q)">>,
                                    x => #{
                                        value => <<"number">>
                                    },
                                    y => #{
                                        value => <<"type">>
                                    },
                                    dynamic => false
                                })
                            ]
                        }, #{
                            height => 300,
                            type => <<"clean">>,
                            cols => [
                                get_list_chart_json(#{
                                    id => <<"top_users_list_chart">>,
                                    %template => <<"#id#. #fullname# (#username#) #messages#">>,
                                    template => <<"<span class='chart_list_rank'>#id#. </span><span class='chart_list_element'>#fullname# (#username#)</><span class='chart_list_number'>#messages#</span>">>,
                                    header => #{
                                        text => <<"Top 5 Users (messages)">>,
                                        css => <<"chart_header">>
                                    },
                                    dynamic => true
                                }),
                                get_list_chart_json(#{
                                    id => <<"top_channels_list_chart">>,
                                    %template => <<"#id#. ##name# #messages#">>,
                                    template => <<"<span class='chart_list_rank'>#id#. </span><span class='chart_list_element'>##name#</><span class='chart_list_number'>#messages#</span>">>,
                                    header => #{
                                        text => <<"Top 5 Channels (messages)">>,
                                        css => <<"chart_header">>
                                    },
                                    dynamic => true
                                })
                            ]
                        }]
                    }
                ]
            }
        }
    }.

get_dash_detail_test2() ->
    #{
        class => <<"webix_ui">>,
        id => <<"detail_test">>,
        value => #{
            view => <<"scrollview">>,
            type => <<"space">>,
            scroll => <<"xy">>,
            id => <<"body">>,
            css => <<"flex-tmp">>,
            borderless => true,
            body => #{
                type => <<"clean">>,
                rows => [
                    #{
                        type => <<"space">>,
                        rows => [#{
                            height => 220,
                            type => <<"wide">>,
                            cols => [
                                get_chart_json(<<"chart_id14">>, <<"scatter">>, <<"Scatter 2">>, true)
                            ]
                        }]
                    }
                ]
            }
        }
    }.

get_dashline() ->
    #{
        id => <<"dashline">>,
        height => 125,
        css => <<"dash-item">>,
        type => <<"clean">>,
        template => #{
            nkParseFunction => <<"
                function(data) {
                    var t = null;
                    var value = null;
                    var delta = null;
                    var deltaClass = '';
                    var items = data.items;
                    var html = \"<div class='flex-tmp'>\";
                    for (var i = 0; i < items.length; i++) {
                        t = items[i];
                        if (!t.numberFormat) {
                            t.numberFormat = {
                                groupDelimiter:',',
                                groupSize:3,
                                decimalDelimiter:'.',
                                decimalSize:0
                            };
                        }
                        value = webix.Number.format(t.value,t.numberFormat);
                        delta = t.delta? webix.Number.format(t.delta,t.numberFormat) : 0;
                        html += \"<div class='item \"+t.css+\"'>\";
                        html += \"<div class='item_header'>\";
                        html += \"<div class='webix_icon icon \"+ t.icon+\"'></div>\";
                        html += \"<div class='text'>\"+t.text+\"</div>\";
                        if (delta > 0) {
                            deltaClass = 'delta_pos';
                        } else if (delta < 0) {
                            deltaClass = 'delta_neg';
                        } else {
                            deltaClass = 'delta_zero';
                        }
                        html += \"<div class='delta \"+deltaClass+\"'>\"+delta+\"%</div>\";
                        html += \"</div>\";
                        html += \"<div class='item_body'>\";
                        html += \"<div class='value'>\"+value+\"</div>\";
                        html += \"</div>\";
                        html += \"</div>\";
                    }
                    html += '</div>';
                    return html;
                }
            ">>
        },
        data => #{
            items => [#{
                id => <<"total_users">>,
                text => <<"Total Users">>,
                value => 0,
                delta => 0,
                icon => <<"fa-user-o">>,
                css => <<"dash-item">>
            }, #{
                id => <<"users_online">>,
                text => <<"Users Online">>,
                value => 0,
                delta => 0,
                icon => <<"fa-user">>,
                css => <<"dash-item">>
            }, #{
                id => <<"total_messages">>,
                text => <<"Total Messages">>,
                value => 0,
                delta => 0,
                icon => <<"fa-commenting">>,
                css => <<"dash-item">>
            }, #{
                id => <<"total_files">>,
                text => <<"Total Sent Files">>,
                value => 0,
                delta => 0,
                icon => <<"fa-paperclip">>,
                css => <<"dash-item">>
            }]
        },
        nkIntervalTime => 5000,
        nkDynamic => false,
        nkIntervalFunction => #{
            nkParseFunction => <<"
                function(id) {
                    var line = $$(id);
                    if (line) {
                        if (line.nkTimer) {
                            // Clear this inteval
                            //console.log('nkIntervalFunction clearInterval', line);
                            clearInterval(line.nkTimer);
                            line.nkTimer = null;
                        }
                        var data = line.data.items;
                        var newData = [];
                        var newValue = 0;
                        var newDelta = 0;
                        var inc = 0;
                        var promises = [];
                        for (var i = 0; i < data.length; i++) {
                            promises.push(ncClient.sendMessageAsync('objects/admin.session/get_chart_data', {
                                element_id: data[i].id
                            }));
                        }
                        Promise.all(promises).then(function(responses) {
                            console.log('get_chart_data: got', responses);
                            for (var i = 0; i < responses.length; i++) {
                                newValue = responses[i].data[data[i].id].value;
                                newDelta = responses[i].data[data[i].id].delta;
                                newData.push({
                                    id: data[i].id,
                                    text: data[i].text,
                                    value: newValue,
                                    delta: newDelta,
                                    icon: data[i].icon,
                                    css: data[i].css
                                })
                            }
                            line.parse({items: newData});
                        }).catch(function(error) {
                            console.log('ERROR: at get_chart_data', error);
                        }).then(function() {
                            if (line.config.nkDynamic) {
                                // Set next interval
                                //console.log('nkIntervalFunction setNextInterval');
                                line.nkTimer = setInterval(function() { line.config.nkIntervalFunction(id) }, line.config.nkIntervalTime);
                            }
                        });
                    }
                }
            ">>
        },
        on => #{
            <<"onAfterRender">> => #{
                nkParseFunction => <<"
                    webix.once(function() {
                        console.log('Dashboard line initial setup', this);
                        var line = $$('dashline');
                        if (line) {
                            line.config.nkIntervalFunction(line.config.id);
                            line.attachEvent('onDestruct', function() {
                                console.log('Dashboard line: Interval cleared');
                                if (this.nkTimer) {
                                    clearInterval(this.nkTimer);
                                }
                            })
                        }
                    })
                ">>
            }
        }
    }.

get_dashline_old() ->
    #{
        id => <<"dashline">>,
        height => 136,
        css => <<"dash-item">>,
        type => <<"space">>,
        template => #{
            nkParseFunction => <<"
                function(data) {
                    var t = null;
                    var items = data.items;
                    var html = \"<div class='flex-tmp'>\";
                    for (var i = 0; i < items.length; i++) {
                        t = items[i];
                        html += \"<div class='item \"+t.css+\"'>\";
                        html += \"<div class='webix_icon icon \"+ t.icon+\"'></div>\";
                        html += \"<div class='details'><div class='value'>\"+t.value+\"</div><div class='text'>\"+t.text+\"</div></div>\";
                        html += \"<div class='footer'>View more <span class='webix_icon fa-angle-double-right'></span></div>\";
                        html += \"</div>\";
                    }
                    html += '</div>';
                    return html;
                }
            ">>
        },
        data => #{
            items => [#{
                id => 1,
                text => <<"Nodes">>,
                value => 250,
                icon => <<"fa-cubes">>,
                css => <<"dash-item nodes">>
            }, #{
                id => 2,
                text => <<"Users">>,
                value => 250,
                icon => <<"fa-users">>,
                css => <<"dash-item users">>
            }, #{
                id => 3,
                text => <<"Domains">>,
                value => 250,
                icon => <<"fa-comment">>,
                css => <<"dash-item domains">>
            }, #{
                id => 4,
                text => <<"Sessions">>,
                value => 250,
                icon => <<"fa-comments">>,
                css => <<"dash-item sessions">>
            }]
        },
        onAfterRender => #{
            nkParseFunction => <<"
                function() {
                    // Set interval
                    var timer;
                    timer = setInterval(function() {
                        var line = $$('dashline');
                        console.log('LINE: ', line);
                        if (!line) {
                            var count = 0;
                            var start = 0;
                            var now = true;
                            //line.loadNext(count, start, null, 'wsChartProxy', now);
                            //line.load('wsChartProxy');
                        }
                    }, 5000);
                    var line = $$('dashline');
                    if (line) {
                        line.nkTimer = timer;
                        line.attachEvent('onDestruct', function() {
                            console.log('Interval cleared');
                            clearInterval(this.nkTimer);
                        })
                    }
                }
            ">>
        }
    }.

get_series_chart_json(#{id := ChartId, x := X, y := Y}=Opts) ->
    Type = maps:get(type, Opts, <<"line">>),
    Header = maps:get(header, Opts, #{}),
    HeaderValue = maps:get(text, Header, <<>>),
    HeaderCss = maps:get(css, Header, <<>>),
    XValue = maps:get(value, X),
    XFormatter = case maps:is_key(formatter, X) of
        true ->
            maps:get(formatter, X);
        false ->
            #{
                nkParseFunction => <<"
                    function(obj) {
                        if (obj && obj.", XValue/binary, ") {
                            return obj.", XValue/binary, ";
                        }
                        return "";
                    }
                ">>
            }
    end,
    YValues = maps:get(values, Y),
    NKCharts1 = [maps:get(value, YValue) || YValue <- YValues],
    NKCharts2 = [XValue|NKCharts1],
    IsDynamic = maps:get(dynamic, Opts, false),
    Spec = case HeaderValue of
        <<>> ->
            #{};
        _ ->
            #{
                header => #{
                    text => HeaderValue,
                    css => HeaderCss
                }
            }
    end,
    Spec2 = Spec#{
        chart_id => ChartId,
        chart_type => Type,
        is_subchart => true,
        nk_charts => NKCharts2,
        x_axis => #{
            template => XFormatter
        },
        y_axis => #{
            start => 0,
            template => nkadmin_webix_chart:parse_number_template()
        },
        legend => #{
            values => [
                #{
                    text => maps:get(text, YValue, <<>>),
                    color => maps:get(color, YValue, <<>>),
                    markerType => maps:get(markerType, YValue, round)
                }
            || YValue <- YValues],
            align => <<"right">>,
            valign => <<"middle">>,
            layout => <<"y">>,
            width => 100,
            margin => 8
        },
        offset => 0,
        origin => 0,
        charts => [
            #{
%                value => YItemValue, %YItemValueHash,
%                item => #{
%                    borderColor => YItemColor,
%                    color => YItemColor
%                },
%                tooltip => #{
%                    template => YItemValue %YItemValueHash
%                }
                value => maps:get(value_hashed, YItem),
                item => #{
                    borderColor => maps:get(color, YItem),
                    color => maps:get(color, YItem)
                },
                line => #{
                    color => maps:get(color, YItem),
                    width => 3
                },
                tooltip => #{
                    template => maps:get(value_hashed, YItem)
                }
            }
            ||
            YItem <- YValues
%            YItemValue <- maps:get(value, YItem),
%            YItemValueHash <- YItemValue, %<<"#", YItemValue/binary, "#">>,
%            YItemColor <- maps:get(color, YItem)
        ],
        dynamic => IsDynamic
    },
    nkadmin_webix_chart:chart(Spec2, #{}).

get_list_chart_json(#{id := ChartId}=Opts) ->
    Autoheight = maps:get(autoheight, Opts, true),
    Data = maps:get(data, Opts, <<>>),
    Header = maps:get(header, Opts, #{}),
    HeaderValue = maps:get(text, Header, <<>>),
    HeaderCss = maps:get(css, Header, <<>>),
    IsDynamic = maps:get(dynamic, Opts, false),
    Select = maps:get(select, Opts, false),
    Template = maps:get(template, Opts, <<"template">>),
    Spec = case HeaderValue of
        <<>> ->
            #{};
        _ ->
            #{
                header => #{
                    text => HeaderValue,
                    css => HeaderCss
                }
            }
    end,
    Spec2 = Spec#{
        chart_id => ChartId,
        chart_type => <<"list">>,
        is_subchart => true,
        autoheight => Autoheight,
        select => Select,
        template => Template,
        data => Data,
        dynamic => IsDynamic
    },
    nkadmin_webix_chart:chart(Spec2,#{}).

get_chart_json(#{id := ChartId, x := X, y := Y}=Opts) ->
    Type = maps:get(type, Opts, <<"line">>),
    Header = maps:get(header, Opts, #{}),
    HeaderValue = maps:get(text, Header, <<>>),
    HeaderCss = maps:get(css, Header, <<>>),
    XValue = maps:get(value, X),
    YValue = maps:get(value, Y),
    NKCharts = [XValue|YValue],
    IsDynamic = maps:get(dynamic, Opts, false),
    Color = maps:get(color, Opts, <<>>),
    Units = maps:get(units, Opts, <<>>),
    Spec = case HeaderValue of
        <<>> ->
            #{};
        _ ->
            #{
                header => #{
                    text => HeaderValue,
                    css => HeaderCss
                }
            }
    end,
    Spec1 = case Color of
        <<>> ->
            Spec;
        _ ->
            Spec#{
                color => nkadmin_webix_chart:parse_color_template(Color)
            }
    end,
    {XAxis, YAxis, Value, Label} = case nkadmin_webix_chart:is_horizontal(Type) of
        true ->
            {
                #{
                    start => 0,
                    template => nkadmin_webix_chart:parse_number_template(Units)
                },
                #{
                    template => #{
                        nkParseFunction => <<"
                            function(obj) {
                                if (obj && obj.", YValue/binary, ") {
                                    return obj.", YValue/binary, ";
                                }
                                return "";
                            }
                        ">>
                    }
                },
                <<"#", XValue/binary, "#">>,
                %<<"#", XValue/binary, "#">>
                nkadmin_webix_chart:parse_number_template(XValue, <<>>)
            };
        false ->
            {
                #{
                    template => #{
                        nkParseFunction => <<"
                            function(obj) {
                                if (obj && obj.", XValue/binary, ") {
                                    return obj.", XValue/binary, ";
                                }
                                return "";
                            }
                        ">>
                    }
                },
                #{
                    start => 0,
                    template => nkadmin_webix_chart:parse_number_template(Units)
                },
                <<"#", YValue/binary, "#">>,
                %<<"#", YValue/binary, "#">>
                nkadmin_webix_chart:parse_number_template(YValue, <<>>)
            }
    end,
    Spec2 = Spec1#{
        chart_id => ChartId,
        chart_type => Type,
        is_subchart => true,
        nk_charts => NKCharts,
        value => Value,
        label => Label,
        x_axis => XAxis,
        y_axis => YAxis,
        offset => 0,
        origin => 0,
        charts => [],
        dynamic => IsDynamic,
        padding => #{
            left => 80
        }
    },
    nkadmin_webix_chart:chart(Spec2, #{}).


get_chart_json(ChartId, <<"scatter">>, Title, Dynamic) ->
    Spec = #{
        chart_id => ChartId,
        chart_type => <<"scatter">>,
        is_subchart => true,
        header => #{
            text => Title,
            css => <<"chart_header">>
        },
        nk_charts => [<<"sales">>, <<"sales2">>, <<"sales3">>],
        x_axis => #{
            title => <<"Value A">>
        },
        x_value => <<"#year#">>,
        y_axis => #{
            title => <<"Value B">>
        },
        item => #{
            radius => 3,
            type => <<"s">>,
            borderWidth => 2
        },
        legend => #{
            layout => <<"y">>,
            width => 75,
            margin => 5,
            marker => #{
                type => <<"item">>
            },
            align => <<"right">>,
            valign => <<"middle">>,
            values => [#{
                text => <<"Type A">>
            }, #{
                text => <<"Type B">>
            }, #{
                text => <<"Type C">>                
            }]
        },
        charts => [#{
            value => <<"#sales#">>,
            item => #{
                radius => 3,
                type => <<"s">>,
                borderColor => <<"#447900">>,
                borderWidth => 2,
                color => <<"#69ba00">>
            }
        }, #{
            value => <<"#sales2#">>,
            item => #{
                radius => 3,
                type => <<"s">>,
                borderColor => <<"#0a796a">>,
                borderWidth => 2,
                color => <<"#4aa397">>
            }
        }, #{
            value => <<"#sales3#">>,
            item => #{
                radius => 3,
                type => <<"s">>,
                borderColor => <<"#b7286c">>,
                borderWidth => 2,
                color => <<"#de619c">>
            }
        }],        
        dynamic => Dynamic
    },
    nkadmin_webix_chart:chart(Spec, #{});

get_chart_json(ChartId, ChartType, Title, Dynamic) ->
    Spec = #{
        chart_id => ChartId,
        chart_type => ChartType,
        is_subchart => true,
        header => #{
            text => Title,
            css => <<"chart_header">>
        },
        nk_charts => [<<"sales">>, <<"sales2">>, <<"sales3">>, <<"year">>],
        value => <<"#sales#">>,
%        item => #{
%            borderColor => <<"#1293f8">>,
%            color => <<"#ffffff">>
%        },
%        line => #{
%            color => <<"#1293f8">>,
%            width => 3
%        },
        x_axis => #{
            start => 0,
%            title => Title,
            template => <<>>
        },
        removed_missed => true,
        offset => 0,
        y_axis => #{
            start => 0,
%            'end' => 100,
            step => 10,
            template => #{
                nkParseFunction => <<"
                    function(obj){
                        return (obj%20?'':obj)
                    }
                ">>
            }
        },
        alpha => 0.7,
        fix_overflow => false,
        shadow => false,
        charts => [
            #{
                value => <<"#sales#">>,
			    color => <<"#58dccd">>,
			    tooltip => #{
                    template => <<"#sales#">>
                }
			},
            #{
                value => <<"#sales2#">>,
                color => <<"#a7ee70">>,
                tooltip => #{
                    template => <<"#sales2#">>
                }
            },
            #{
                value => <<"#sales3#">>,
                color => <<"#36abee">>,
                tooltip => #{
                    template => <<"#sales3#">>
                }
            }
        ],
        dynamic => Dynamic,
        animate_duration => 400,
        nk_interval_time => 5000
    },
    nkadmin_webix_chart:chart(Spec, #{}).
