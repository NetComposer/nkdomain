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

%% @doc State Object
-module(nkadmin_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/3]).
-export([switch_domain/3, element_action/4, get_data/3, get_chart_data/3]).
-export([find_all/0]).
-export([object_info/0, object_parse/2, object_es_mapping/0,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([object_admin_info/0, object_schema_types/0]).
-export([find_url_class/1]).
-export_type([session/0]).

-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include("nkdomain_debug.hrl").


%% Period to find for inactive conversations
-define(CHECK_TIME, 5*60*1000).


%% ===================================================================
%% Types
%% ===================================================================


-type start_opts() :: #{
    language => binary(),
    session_link => {module(), pid()},
    session_events => [binary()],
    domain_id => binary(),
    http_auth_id => binary(),
    url => binary()
}.

-type session() :: #admin_session{}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Creates a new session
-spec start(nkdomain:id(), nkdomain:id(), start_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(DomainId, UserId, Opts) ->
    Obj = #{
        type => ?DOMAIN_ADMIN_SESSION,
        domain_id => DomainId,
        parent_id => UserId,
        created_by => UserId,
        active => true,
        ?DOMAIN_SESSION => #{}
    },
    Opts2 = maps:with([session_link, session_events], Opts),
    Opts3 = Opts2#{meta => maps:with([languaje, http_auth_id], Opts)},
    case nkdomain_obj_make:create(Obj, Opts3) of
        {ok, #obj_id_ext{obj_id=SessId, pid=Pid}, _} ->
            AdminDomainId = maps:get(domain_id, Opts, DomainId),
            Url = maps:get(url, Opts, <<>>),
            case switch_domain(Pid, AdminDomainId, Url) of
                {ok, Updates} ->
                    {ok, SessId, Pid, Updates};
                {error, Error} ->
                    nkdomain:unload(Pid, start_error),
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
switch_domain(Id, DomainId, Url) ->
    nkdomain_obj:sync_op(Id, {?MODULE, switch_domain, DomainId, Url}).


%% @doc
element_action(Id, ElementId, Action, Value) ->
    nkdomain_obj:sync_op(Id, {?MODULE, element_action, ElementId, Action, Value}).


%% @doc
get_data(Id, ElementId, Data) ->
    nkdomain_obj:sync_op(Id, {?MODULE, get_data, ElementId, Data}).

%% @doc
get_chart_data(Id, ElementId, Data) ->
        nkdomain_obj:sync_op(Id, {?MODULE, get_chart_data, ElementId, Data}).    

%% @private
find_all() ->
    nkdomain_domain_obj:search_all(root, #{filters=>#{type=>?DOMAIN_ADMIN_SESSION}}).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?DOMAIN_ADMIN_SESSION,
        schema_type => 'AdminSession',
        stop_after_disabled => true,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 4000,
        type_view_mod => nkadmin_session_obj_type_view
%        tree_id => <<"domain_tree_sessions_admin.sessions">>
    }.


%% @doc
object_schema_types() ->
    #{
        'AdminSession' => #{
            fields => #{
            },
            is_object => true,
            comment => "An Admin Session"
        }
    }.


%% @private
object_es_mapping() ->
    #{
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
    }.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkadmin_session_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkadmin_session_obj_cmd:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkadmin_session_obj_events:event(Event, State).


%% @private
object_init(#obj_state{effective_srv_id=SrvId, domain_id=DomainId, id=Id, obj=Obj, meta=Meta}=State) ->
    #obj_id_ext{obj_id=SessId} = Id,
    #{created_by:=UserId} = Obj,
    Session = #admin_session{
        srv_id = SrvId,
        session_id = SessId,
        http_auth_id = maps:get(http_auth_id, Meta, <<>>),
        domain_id = DomainId,
        user_id = UserId,
        language = maps:get(language, Meta, <<"en">>)
    },
    ok = nkdomain_user:register_session(UserId, DomainId, ?DOMAIN_ADMIN_SESSION, SessId, #{}),
    State2 = nkdomain_obj_util:link_to_session_server(?MODULE, State),
    State3 = State2#obj_state{meta=#{}, session=Session},
    {ok, State3}.


%% @private
%% Version that does not kill the websocket
%% Client must read 'unloaded' events
%%object_stop(_Reason, State) ->
%%    {ok, nkdomain_obj_util:unlink_from_session_server(?MODULE, State)}.

%% @private
object_stop(_Reason, #obj_state{session_link={Mod, Pid}}=State) ->
    % When the session stops, we stop the WS
    Mod:stop_session(Pid, nkdomain_session_stop),
    {ok, State}.


%% @private
object_sync_op({?MODULE, switch_domain, Domain, Url}, _From, State) ->
    case nkdomain_lib:load(Domain) of
        #obj_id_ext{obj_id=DomainId, path=Path, type= ?DOMAIN_DOMAIN} ->
            case do_switch_domain(DomainId, Path, Url, State) of
                {ok, Reply, State2} ->
                    {reply, {ok, Reply}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        not_found ->
            {error, domain_unknown, State}
    end;

object_sync_op({?MODULE, element_action, <<"url">>, updated, Url}, _From, State) ->
    #obj_state{session=Session} = State,
    case find_url(Url, Session) of
        {ok, Parts} ->
            case do_element_action(Parts, selected, #{update_url=>false}, State) of
                {ok, Reply, State2} ->
                    {reply, {ok, Reply}, State2};
                {error, Error, State2} ->
                    {reply, {error, Error}, State2}
            end;
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, element_action, <<"breadcrumbs">>, selected, Url}, _From, State) ->
    #obj_state{session=Session} = State,
    case find_url(Url, Session) of
        {ok, Parts} ->
            case do_element_action(Parts, selected, #{update_url=>true}, State) of
                {ok, Reply, State2} ->
                    {reply, {ok, Reply}, State2};
                {error, Error, State2} ->
                    {reply, {error, Error}, State2}
            end;
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
    % object_sync_op({?MODULE, element_action, <<"url">>, updated, Val}, _From, State);

object_sync_op({?MODULE, element_action, ElementId, Action, Value}, _From, State) ->
    case do_element_action(get_id_parts(ElementId), Action, Value, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error, State2} ->
            {reply, {error, Error}, State2}
    end;

object_sync_op({?MODULE, get_data, ElementId, Data}, _From, State) ->
    case do_get_data(get_id_parts(ElementId), Data, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error, State2} ->
            {reply, {error, Error}, State2}
    end;

object_sync_op({?MODULE, get_chart_data, ElementId, Data}, _From, State) ->
    case do_get_chart_data(ElementId, Data, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error, State2} ->
            {reply, {error, Error}, State2}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op(_Op, _State) ->
    continue.


%% @private We received an event from a subscribed object
object_handle_info({nkevent, #nkevent{type=Type}=Event}, State) ->
    case lists:member(Type, [<<"created">>, <<"updated">>, <<"deleted">>, <<"type_counter">>]) of
        true ->
            {noreply, do_event(Event, State)};
        false ->
            {noreply, State}
    end;

object_handle_info({'DOWN', _Ref, process, Pid, _Reason}, #obj_state{session=Session}=State) ->
    #admin_session{reg_pids=RegPids} = Session,
    case maps:find(Pid, RegPids) of
        {ok, [domain]} ->
            RegPids2 = maps:remove(Pid, RegPids),
            Session2 = Session#admin_session{reg_pids=RegPids2},
            Session3 = subscribe_domain(<<>>, Session2),
            {noreply, State#obj_state{session=Session3}};
        error ->
            continue
    end;

object_handle_info(_Info, _State) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_switch_domain(DomainId, Path, Url, #obj_state{session=Session}=State) ->
    case nkdomain_domain_obj:search_all_types(DomainId, #{}) of
        {ok, _, TypeList, _Meta} ->
            Url2 = case Url of
                <<"#", U/binary>> -> U;
                _ -> Url
            end,
            Types = [Type || {Type, _Counter} <- TypeList],
            Session2 = Session#admin_session{
                domain_id   = DomainId,
                domain_path = Path,
                url         = case Url2 of <<>> -> Path; _ -> Url2 end,
                detail      = #{},
                db_types    = Types,
                resources   = [],
                sessions    = #{},
                object_tags = #{},
                key_data    = #{},
                special_urls= #{}
            },
            case do_get_domain(Session2, State) of
                {ok, Updates, #admin_session{}=Session3} ->
                    #admin_session{domain_path=OldPath} = Session,
                    Session4 = subscribe_domain(OldPath, Session3),
                    State4 = State#obj_state{session=Session4},
                    % io:format("UPDATES:\n\n~p\n", [Updates]),
                    {ok, #{elements=>lists:reverse(Updates)}, State4};
                {error, Error, #admin_session{}=Session3} ->
                    State3 = State#obj_state{session=Session3},
                    {error, Error, State3}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @private
do_get_domain(Session, State) ->
    do_get_domain_frame([], Session, State).


%% @private
do_get_domain_frame(Updates, Session, State) ->
    case handle(admin_get_frame, [], Session) of
        {ok, Frame, Session2} ->
            do_get_domain_tree([Frame|Updates], Session2, State);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% @private
do_get_domain_tree(Updates, Session, State) ->
    case handle(admin_get_tree, [], Session) of
        {ok, Tree, Session2} ->
            do_get_domain_detail([Tree|Updates], Session2, State);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% @private
do_get_domain_detail(Updates, Session, State) ->
    #admin_session{url=Url} = Session,
    case find_url(Url, Session) of
        {ok, Parts} ->
            case handle(admin_element_action, [Parts, selected, <<>>, Updates], Session) of
                {ok, Updates2, Session2} ->
                    {ok, Updates2, Session2};
                {error, Error, Session2} ->
                    ?LLOG(notice, "error calling element_action for ~p: ~p", [Parts, Error], State),
                    {ok, Updates, Session2}
            end;
        {error, url_unknown} ->
            % TODO set a default detail page
            ?LLOG(notice, "detail url ~s not found", [Url], State),
            {Updates2, Session2} = nkadmin_util:update_detail(<<"/">>, #{}, Updates, Session),
            {ok, Updates2, Session2}
    end.


%% @private Event from subscribed object
do_event(Event, #obj_state{session=Session}=State) ->
    {ok, UpdList, Session2} = handle(admin_event, [Event, []], Session),
    State2 = State#obj_state{session=Session2},
    case UpdList of
        [] ->
            State2;
        _ ->
            send_event({update_elements, UpdList}, State2)
    end.


%% @private
send_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_element_action(Parts, Action, Value, State) ->
    case do_element_action_updates(Parts, Action, Value, State) of
        {ok, UpdList, Session2} ->
            State2 = State#obj_state{session=Session2},
            case UpdList of
                [] ->
                    {ok, #{}, State2};
                _ ->
                    {ok, #{elements=>UpdList}, State2}
            end;
        {error, Error, Session2} ->
            {error, Error, State#obj_state{session=Session2}}
    end.


%% @private
do_element_action_updates(Parts, Action, Value, State) ->
    #obj_state{session=Session} = State,
    handle(admin_element_action, [Parts, Action, Value, []], Session).



%% @private
do_get_data(Parts, Spec, State) ->
    #obj_state{session=Session} = State,
    case handle(admin_get_data, [Parts, Spec], Session) of
        {ok, Reply, Session2} ->
            State2 = State#obj_state{session=Session2},
            {ok, Reply, State2};
        {error, Error, Session2} ->
            State2 = State#obj_state{session=Session2},
            {error, Error, State2}
    end.


%% @private
do_get_chart_data(<<"total_users">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"term">> => #{
                        <<"type">> => <<"user">>
                    }
                }]
            }
        }
        %"aggs" => #{
        %}
    },
    case nkelastic:search(Query, Opts) of
        {ok, Hits, _, _, _} ->
            %lager:info("RESPONSE: ~p~n", [Hits]),
            {ok, #{<<"total_users">> => #{value => Hits, delta => 10}}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(<<"total_messages">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"term">> => #{
                        <<"type">> => <<"message">>
                    }
                }]
            }
        }
        %"aggs" => #{
        %}
    },
    case nkelastic:search(Query, Opts) of
        {ok, Hits, _, _, _} ->
            %lager:info("RESPONSE: ~p~n", [Hits]),
            {ok, #{<<"total_messages">> => #{value => Hits, delta => 20}}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(<<"total_files">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"term">> => #{
                        <<"type">> => <<"file">>
                    }
                }]
            }
        }
        %"aggs" => #{
        %}
    },
    case nkelastic:search(Query, Opts) of
        {ok, Hits, _, _, _} ->
            %lager:info("RESPONSE: ~p~n", [Hits]),
            {ok, #{<<"total_files">> => #{value => Hits, delta => 15}}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(<<"activity_stats_line_chart">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"terms">> => #{
                        <<"message.type">> => [<<"text">>, <<"media.call">>]
                    }
                }, #{
                    <<"term">> => #{
                        <<"type">> => <<"message">>
                    }
                }]
            }
        },
        <<"aggs">> => #{
            <<"messages">> => #{
                <<"date_histogram">> => #{
                    <<"field">> => <<"created_time">>,
                    <<"interval">> => <<"month">>,
                    <<"min_doc_count">> => <<"0">>,
                    <<"format">> => <<"yyyy-MM-dd">>,
                    <<"extended_bounds">> => #{
                        <<"min">> => <<"2017-01-01">>,
                        <<"max">> => <<"2017-12-31">>
                    }
                }
            }
        }
    },
    case nkelastic:search(Query, Opts) of
        {ok, _Total, _Hits, Aggs, _Meta} ->
            %lager:error("ES RESPONSE: ~n~p~n~p~n~p~n~p~n", [_Total, _Hits, Aggs, _Meta]),
            Aggs2 = maps:get(<<"messages">>, Aggs),
            Buckets = maps:get(<<"buckets">>, Aggs2),
%            Data = [
%                #{ messages => 17500, audio =>  2500, video =>  7500, month => <<"jan.">> },
%                #{ messages => 18000, audio =>  7500, video => 12500, month => <<"feb.">> },
%                #{ messages => 21500, audio =>  5000, video => 15000, month => <<"mar.">> },
%                #{ messages => 20000, audio => 12500, video =>  7500, month => <<"apr.">> },
%                #{ messages => 18250, audio => 15000, video =>  7500, month => <<"may.">> },
%                #{ messages => 16000, audio => 10000, video => 12000, month => <<"jun.">> },
%                #{ messages => 18000, audio => 12500, video =>  7500, month => <<"jul.">> },
%                #{ messages => 18000, audio => 15000, video =>  7500, month => <<"aug.">> },
%                #{ messages => 22500, audio => 20000, video => 11500, month => <<"sep.">> },
%                #{ messages => 23000, audio => 17500, video => 11000, month => <<"oct.">> },
%                #{ messages => 23000, audio => 15000, video => 11000, month => <<"nov.">> },
%                #{ messages => 25000, audio => 20000, video => 12000, month => <<"dec.">> }
%            ],
            Data = [
                #{
                    messages => maps:get(<<"doc_count">>, Bucket),
                    audio => 0,
                    video => 0,
                    month => maps:get(<<"key_as_string">>, Bucket)
                }
                || Bucket <- Buckets
            ],
            {ok, #{data => Data}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(<<"video_calls_barh_chart">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"term">> => #{
                        <<"type">> => <<"file">>
                    }
                }]
            }
        }
        %"aggs" => #{
        %}
    },
    case nkelastic:search(Query, Opts) of
        {ok, _Hits, _, _, _} ->
            %lager:info("RESPONSE: ~p~n", [Hits]),
            %{ok, #{<<"total_files">> => #{value => Hits, delta => 15}}, State};
            Data = [
                #{ minutes => 2050, type => <<"Total">> },
                #{ minutes => 700, type => <<"VideoC.">> },
                #{ minutes => 1400, type => <<"Calls">> }
            ],
            {ok, #{data => Data}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(<<"sent_files_barh_chart">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"term">> => #{
                        <<"type">> => <<"file">>
                    }
                }]
            }
        },
        <<"aggs">> => #{
            <<"file_types">> => #{
                <<"terms">> => #{
                    <<"field">> => <<"file.content_type">>
                }
            }
        }
    },
    case nkelastic:search(Query, Opts) of
        {ok, _Total, _Hits, Aggs, _Meta} ->
            Aggs2 = maps:get(<<"file_types">>, Aggs),
            Buckets = maps:get(<<"buckets">>, Aggs2),
%            Data = [
%                #{ number => 140000, type => <<"Images">> },
%                #{ number => 40000, type => <<"Videos">> },
%                #{ number => 25000, type => <<"Audios">> },
%                #{ number => 30000, type => <<"Text">> },
%                #{ number => 35000, type => <<"PDF">> },
%                #{ number => 5000, type => <<"Other">> }
%            ],
            Data = [
                #{ number => maps:get(<<"doc_count">>, Bucket), type => maps:get(<<"key">>, Bucket) }
                || Bucket <- Buckets
            ],
            {ok, #{data => Data}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(<<"conversations_barh_chart">>, _Spec, State) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => 0,
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{ 
                    <<"terms">> => #{
                        <<"conversation.type">> => [<<"one2one">>, <<"channel">>, <<"private">>]
                    }
                }, #{
                    <<"term">> => #{
                        <<"type">> => <<"conversation">>
                    }
                }]
            }
        },
        <<"aggs">> => #{
            <<"conversation_types">> => #{
                <<"terms">> => #{
                    <<"field">> => <<"conversation.type">>
                }
            }
        }
    },
    case nkelastic:search(Query, Opts) of
        {ok, _Total, _Hits, Aggs, _Meta} ->
            Aggs2 = maps:get(<<"conversation_types">>, Aggs),
            Buckets = maps:get(<<"buckets">>, Aggs2),
%            Data = [
%                #{ number => 73000, type => <<"P2P">> },
%                #{ number => 45000, type => <<"Groups">> },
%                #{ number => 9000, type => <<"Channels">> }
%            ],
            Data = [
                #{ number => maps:get(<<"doc_count">>, Bucket), type => maps:get(<<"key">>, Bucket) }
                || Bucket <- Buckets
            ],
            {ok, #{data => Data}, State};
        _ ->
            {error, error, State}
    end;

do_get_chart_data(ElementId, _Spec, State) ->
    lager:info("Unknown element id: ~p~n", [ElementId]),
    {ok, #{ElementId => #{value => 0, delta => 0}}, State}.


%Query #{
%    '_source' => [<<"path">>,<<"obj_name">>,<<"srv_id">>,<<"created_time">>,<<"created_by">>,<<"enabled">>,<<"user.name">>,<<"user.surname">>,<<"user.email">>],
%    from => 0,
%    query => #{
%        bool => #{
%            filter => [#{
%                term => #{
%                    type => <<"user">>
%                }
%            }, #{
%                wildcard => #{
%                    path => <<"/?*">>
%                }
%            }]
%        }
%    },
%    size => 50,
%    sort => [#{
%        <<"path">> => #{
%            order => desc
%        }
%    }]
%}

%    #obj_state{session=Session} = State,
%    case handle(admin_get_data, [Parts, Spec], Session) of
%        {ok, Reply, Session2} ->
%            State2 = State#obj_state{session=Session2},
%            {ok, Reply, State2};
%        {error, Error, Session2} ->
%            State2 = State#obj_state{session=Session2},
%            {error, Error, State2}
%    end.


%% @private
find_url(<<"_id/", ObjId/binary>>, _Session) ->
    case nkdomain_lib:find(ObjId) of
        #obj_id_ext{type=Type, path=Path} ->
            {ok, [?ADMIN_OBJ_ID, ObjId, Type, Path]};
        {error, _Error} ->
            {error, url_unknown}
    end;

find_url(Url, Session) ->
    case nkadmin_util:get_special_url(Url, Session) of
        undefined ->
            #admin_session{domain_path=Base} = Session,
            Url2 = nkdomain_util:append(Base, Url),
            case nkdomain_lib:find(Url2) of
                #obj_id_ext{obj_id=ObjId, type=Type, path=Path} ->
                    {ok, [?ADMIN_OBJ_ID, ObjId, Type, Path]};
                {error, _} ->
                    find_url_class(Url2)
            end;
        Key ->
            {ok, get_id_parts(Key)}
    end.


%% @private
find_url_class(Url) ->
    case lists:reverse(binary:split(Url, <<"/">>, [global])) of
        [Class|Rest] ->
            case nkdomain_util:type(Class) of
                {ok, Type} ->
                    Path = case nklib_util:bjoin(lists:reverse(Rest), <<"/">>) of
                        <<>> -> <<"/">>;
                        Path0 -> Path0
                    end,
                    {ok, [?ADMIN_OBJ_TYPE, Path, Type]};
                error ->
                    {error, url_unknown}
            end;
        _ ->
            {error, url_unknown}
    end.


%% @private
%% TODO we should subscribe to type_counter only on my base (top) domain, and subscribe to any other object one-by-one
subscribe_domain(OldPath, #admin_session{srv_id=SrvId, domain_path=NewPath, reg_pids=RegPids}=Session) ->
    Types = [<<"created">>, <<"updated">>, <<"deleted">>, <<"type_counter">>],
    case OldPath of
        <<>> ->
            ok;
        _ ->
            Reg = #{
                srv_id => SrvId,
                class => ?DOMAIN_EVENT_CLASS,
                type => Types,
                domain => OldPath
            },
            nkevent:unreg(Reg)
    end,
    Reg2 = #{
        srv_id => SrvId,
        class => ?DOMAIN_EVENT_CLASS,
        type => Types,
        domain => NewPath
    },
    {ok, Pids} = nkevent:reg(Reg2),
    Reg2N = #{
        srv_id => sipstorm_c4,
        class => ?DOMAIN_EVENT_CLASS,
        type => Types,
        domain => NewPath
    },
    {ok, _} = nkevent:reg(Reg2N),
    RegPids2 = lists:foldl(
        fun(Pid, Acc) ->
            Objs = case maps:find(Pid, Acc) of
                {ok, Keys} ->
                    nklib_util:store_value(domain, Keys);
                error ->
                    erlang:monitor(process, Pid),
                    [domain]
            end,
            Acc#{Pid => Objs}
        end,
        RegPids,
        Pids),
    Session#admin_session{reg_pids=RegPids2}.


%% @private
get_id_parts(ElementId) ->
    binary:split(ElementId, <<"__">>, [global]).


%% @private
handle(Fun, Args, #admin_session{srv_id=SrvId}=Session) ->
    apply(SrvId, Fun, Args++[Session]).
