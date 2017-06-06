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
-module(nkdomain_admin_tree).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([categories/2, get_category/2, event/3, element_action/5]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).

-define(OVERVIEW,       <<"domain_tree_overview">>).
-define(DASHBOARD,      <<"domain_tree_overview_dashboard">>).
-define(DOMAINS,        <<"domain_tree_overview_domains">>).
-define(DOMAINS_ID,     <<"domain_tree_overview_domains_id">>).
-define(DOMAINS_ID2,      "domain_tree_overview_domains_id").
-define(DOMAINS_ALL,    <<"domain_tree_overview_domains_all">>).
-define(ALERTS,         <<"domain_tree_overview_alerts">>).
-define(RESOURCES,      <<"domain_tree_resources">>).
-define(SESSIONS,       <<"domain_tree_sessions">>).
-define(NETWORKS,       <<"domain_tree_networks">>).
-define(SERVICES,       <<"domain_tree_services">>).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc 
categories(Data, State) ->
    Admin = #{
        overview => 1000,
        resources => 1100,
        sessions => 1200,
        networks => 1300,
        services => 1400
    },
    {ok, maps:merge(Admin, Data), State}.


%% @doc
get_category(overview, State) ->
    case find_domains(State) of
        {ok, DomainList} ->
            {ok, Domains, State2} = get_domains(DomainList, State),
            {ok, Dashboards, State3} = get_dashboards(State2),
            {ok, Alerts, State4} = get_alerts(State3),
            Items = [Dashboards, Domains, Alerts],
            Category = nkadmin_util:menu_item(?OVERVIEW, menuCategory, #{items=>Items}, State4),
            {ok, Category, State4};
        {error, Error} ->
            {error, Error, State}
    end;

get_category(resources, #{types:=Types}=State) ->
    get_resources_category(Types, State);

get_category(sessions, #{types:=Types}=State) ->
    get_sessions_category(Types, State);

get_category(_Category, _State) ->
    continue.


%% @doc
event(#nkevent{type = <<"counter_updated">>, obj_id=ObjId}=Event, Updates, #{domain_path:=ObjId}=State) ->
    #nkevent{subclass=ObjType, body=#{counter:=Counter}}=Event,
    case update_session(ObjType, Counter, State) of
        {true, Item, State2} ->
            {continue, [Event, [Item|Updates], State2]};
        false ->
            continue
    end;

event(#nkevent{type = <<"created">>, subclass=ObjType, obj_id=ObjId, body=Body}=Event, Updates, State) ->
    {Updates2, State2} = update_resource(ObjType, Updates, State),
    {Updates3, State3} = case ObjType of
        ?DOMAIN_DOMAIN ->
            #{parent_id:=ParentId} = Body,
            created_domain(ObjId, ParentId, Updates2, State2);
        _ ->
            {Updates2, State2}
    end,
    {continue, [Event, Updates3, State3]};

event(#nkevent{type = <<"updated">>, subclass=ObjType, obj_id=ObjId}=Event, Updates, State) ->
    {Updates2, State2} = case ObjType of
        ?DOMAIN_DOMAIN ->
            updated_domain(ObjId, Updates, State);
        _ ->
            {Updates, State}
    end,
    {continue, [Event, Updates2, State2]};

event(#nkevent{type = <<"deleted">>, subclass=ObjType, obj_id=ObjId}=Event, Updates, State) ->
    {Updates2, State2} = case ObjType of
        ?DOMAIN_DOMAIN ->
            deleted_domain(ObjId, Updates, State);
        _ ->
            {Updates, State}
    end,
    {continue, [Event, Updates2, State2]};

event(Event, Updates, State) ->
    {continue, [Event, Updates, State]}.



%% @doc
element_action(?DOMAINS_ALL, selected, Value, Updates, State) ->
    {Updates2, State2} = selected_all_domains(Updates, State),
    {continue, [?DOMAINS_ALL, selected, Value, Updates2, State2]};

element_action(<<?DOMAINS_ID2, $_, ObjId/binary>>, selected, Value, Updates, State) ->
    {Updates2, State2} = selected_domain(ObjId, Updates, State),
    {continue, [?DOMAINS_ALL, selected, Value, Updates2, State2]};

%%element_action(<<?DOMAINS_ID2, $_, ObjId/binary>>, selected, Value, Updates, State) ->
%%    {Updates2, State2} = selected_domain(ObjId, Updates, State),
%%    {continue, [?DOMAINS_ALL, selected, Value, Updates2, State2]};

element_action(<<"domain_tree_resources_users">>, selected, Value, Updates, State) ->
    #{domain_id:=DomainId} = State,
    Table = nkdomain_user_obj_ui:table(root, DomainId),
    Updates2 = nkadmin_util:append_path(<<"users">>, Updates, State),
    Item = #{
        class => detail,
        id => detail,
        value => #{
            id => <<"domain_detail_user_table">>,
            webix_ui => Table
        }
    },
    {continue, [<<"domain_tree_resources_users">>, selected, Value, [Item|Updates2], State]};

element_action(<<"domain_tree_resources_", Type/binary>>=Key, selected, Value, Updates, State) ->
    Updates2 = nkadmin_util:append_path(Type, Updates, State),
    Item = #{
        class => detail,
        id => detail,
        value => #{}
    },
    {continue, [Key, selected, Value, [Item|Updates2], State]};

element_action(<<"domain_tree_sessions_", Type/binary>>=Key, selected, Value, Updates, State) ->
    Updates2 = nkadmin_util:append_path(Type, Updates, State),
    Item = #{
        class => detail,
        id => detail,
        value => #{}
    },
    {continue, [Key, selected, Value, [Item|Updates2], State]};

element_action(_Id, _Action, _Value, _Updates, _State) ->
    continue.



%% ===================================================================
%% Dashboards
%% ===================================================================

%% @private
get_dashboards(State) ->
    {ok, nkadmin_util:menu_item(?DASHBOARD, menuEntry, #{}, State), State}.



%% ===================================================================
%% Domains
%% ===================================================================


%% @private
find_domains(#{srv_id:=SrvId, domain_id:=DomainId}) ->
    Spec = #{
        filters => #{type => ?DOMAIN_DOMAIN}
    },
    case nkdomain_domain_obj:find_childs(SrvId, DomainId, Spec) of
        {ok, _N, List} ->
            {ok, [ObjId || {?DOMAIN_DOMAIN, ObjId, _Path} <- List]};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_domains(DomainList, State) ->
    {ok, Items, State2} = get_domain_items(DomainList, [], State),
    Items2 = Items ++ [nkadmin_util:menu_item(?DOMAINS_ALL, menuEntry, #{}, State)],
    Element = nkadmin_util:menu_item(?DOMAINS, menuGroup, #{items=>Items2}, State2),
    State3 = nkadmin_util:add_key(?DOMAINS_ALL, #{domain_ids=>DomainList}, State2),
    {ok, Element, State3}.


%% @private
get_domain_items([], [{_ObjName, Item}], State) ->
    {ok, [Item], State};

get_domain_items([], Acc, State) ->
    {ok, [Item || {_ObjName, Item} <- lists:sort(Acc)], State};

get_domain_items([ObjId|Rest], Acc, State) ->
    case nkdomain_obj:get_name(ObjId) of
        {ok, #{
            name := Name,
            obj_name := ObjName,
            description := Desc,
            path := Path
        }} ->
            Id = get_domain_id(ObjId),
            Value = #{label=>Name, tooltip=>Desc, path=>Path},
            Item = nkadmin_util:menu_item(Id, menuEntry, Value, State),
            get_domain_items(Rest, [{ObjName, Item}|Acc], State);
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [ObjId, Error]),
            get_domain_items(Rest, Acc, State)
    end.


%% @private
get_domain_id(ObjId) ->
    << ?DOMAINS_ID/binary, $_, ObjId/binary>>.


%% @private
created_domain(ObjId, ParentId, Updates, #{domain_id:=ParentId}=State) ->
    #{domain_ids:=DomList1} = nkadmin_util:get_key(?DOMAINS_ALL, State),
    DomList2 = nklib_util:store_value(ObjId, DomList1),
    State2 = nkadmin_util:add_key(?DOMAINS_ALL, #{domain_ids=>DomList2}, State),
    {ok, Item, State3} = get_domains(DomList2, State2),
    {[Item|Updates], State3};

created_domain(_ObjId, _ParentId, Updates, State) ->
    {Updates, State}.


%% @private
updated_domain(ObjId, Updates, State) ->
    #{domain_ids:=DomList} = nkadmin_util:get_key(?DOMAINS_ALL, State),
    case lists:member(ObjId, DomList) of
        true ->
            {ok, [Item], State2} = get_domain_items([ObjId], [], State),
            {[Item|Updates], State2};
        false ->
            {Updates, State}
    end.


%% @private
deleted_domain(ObjId, Updates, State) ->
    #{domain_ids:=DomList} = nkadmin_util:get_key(?DOMAINS_ALL, State),
    case lists:member(ObjId, DomList) of
        true ->
            DomList2 = DomList -- [ObjId],
            State2 = nkadmin_util:add_key(?DOMAINS_ALL, #{domain_ids=>DomList2}, State),
            {ok, Item, State3} = get_domains(DomList2, State2),
            {[Item|Updates], State3};
        false ->
            {Updates, State}
    end.


%% @private
selected_all_domains(Updates, State) ->
    Updates2 = nkadmin_util:append_path(<<"domains">>, Updates, State),
    {Updates2, State}.


%% @private
selected_domain(ObjId, Updates, #{srv_id:=SrvId}=State) ->
    case nkdomain_obj_lib:load(SrvId, ObjId, #{}) of
        #obj_id_ext{path=Path} ->
            Updates2 = nkadmin_util:update_path(Path, Updates),
            State2 = State#{detail_path=>Path},
            {Updates2, State2};
        {error, Error} ->
            lager:error("NKLOG SHOW VIEW ERROR: ~p", [Error]),
            {Updates, State}
    end.


%% ===================================================================
%% Alerts
%% ===================================================================

%% @private
get_alerts(State) ->
    Item = nkadmin_util:menu_item(?ALERTS, menuEntry, #{badge=>3}, State),
    {ok, Item, State}.


%% ===================================================================
%% Resources
%% ===================================================================

%% @private
get_resources_category(Types, State) ->
    {ok, Items, State2} = get_resource_items(Types, [], State),
    Category = nkadmin_util:menu_item(?RESOURCES, menuCategory, #{items=>Items}, State2),
    {ok, Category, State2}.


%% @private
is_resource(Type, #{srv_id:=SrvId}) ->
    case SrvId:object_admin_info(Type) of
        #{class:=resource} = Info ->
            {true, Info};
        _ ->
            false
    end.


%% @private
get_resource_items([], [{_Weight, Item}], State) ->
    {ok, [Item], State};

get_resource_items([], Acc, State) ->
    {ok, [Item || {_Weigth, Item} <- lists:keysort(1, Acc)], State};

get_resource_items([Type|Rest], Acc, State) ->
    case is_resource(Type, State) of
        {true, #{tree_id:=Key}=Info} ->
            Weight = maps:get(weight, Info, 9000),
            Item = nkadmin_util:menu_item(Key, menuEntry, #{}, State),
            #{resources:=Resources} = State,
            State2 = case lists:member(Type, Resources) of
                true -> State;
                false -> State#{resources:=[Type|Resources]}
            end,
            get_resource_items(Rest, [{Weight, Item}|Acc], State2);
        _ ->
            get_resource_items(Rest, Acc, State)
    end.


%% @private
update_resource(Type, Updates, #{resources:=Resources}=State) ->
    case lists:member(Type, Resources) of
        true ->
            {Updates, State};
        false ->
            case is_resource(Type, State) of
                {true, _} ->
                    Resources2 = [Type|Resources],
                    {ok, Category, State2} = get_resources_category(Resources2, State),
                    {[Category|Updates], State2};
                false ->
                    {Updates, State}
            end
    end.




%% ===================================================================
%% Sessions
%% ===================================================================

%% @private
get_sessions_category(Types, State) ->
    {ok, Items, State2} = get_session_items(Types, [], State),
    Category = nkadmin_util:menu_item(?SESSIONS, menuCategory, #{items=>Items}, State2),
    {ok, Category, State2}.


%% @private
is_session(Type, #{srv_id:=SrvId}) ->
    case SrvId:object_admin_info(Type) of
        #{class:=session} = Info ->
            {true, Info};
        _ ->
            false
    end.


%% @private
get_session_items([], [{_Weight, Item}], State) ->
    {ok, [Item], State};

get_session_items([], Acc, State) ->
    {ok, [Item || {_Weigth, Item} <- lists:keysort(1, Acc)], State};

get_session_items([Type|Rest], Acc, State) ->
    case is_session(Type, State) of
        {true, #{tree_id:=Key}=Info} ->
            Weight = maps:get(weight, Info, 9000),
            #{sessions:=Sessions} = State,
            case maps:find(Type, Sessions) of
                {ok, 0} ->
                    get_session_items(Rest, Acc, State);
                {ok, Counter} ->
                    Item = nkadmin_util:menu_item(Key, menuEntry, #{counter=>Counter}, State),
                    get_session_items(Rest, [{Weight, Item}|Acc], State);
                error ->
                    #{srv_id:=SrvId, domain_path:=DomainPath} = State,
                    case SrvId:object_get_counter(Type, DomainPath) of
                        {ok, 0} ->
                            get_session_items(Rest, Acc, State);
                        {ok, Counter} ->
                            Item = nkadmin_util:menu_item(Key, menuEntry, #{counter=>Counter}, State),
                            State2 = State#{sessions:=Sessions#{Type=>Counter}},
                            get_session_items(Rest, [{Weight, Item}|Acc], State2)
                    end
            end;
        _ ->
            get_session_items(Rest, Acc, State)
    end.


%% @private
update_session(Type, Counter, #{sessions:=Sessions}=State) ->
    case maps:is_key(Type, Sessions) of
        true when Counter > 0 ->
            State2 = State#{sessions:=Sessions#{Type=>Counter}},
            {ok, [Item], State3} = get_session_items([Type], [], State2),
            {true, Item, State3};
        true ->
            Sessions2 = maps:remove(Type, Sessions),
            State2 = State#{sessions:=Sessions2},
            {ok, Category, State3} = get_sessions_category(maps:keys(Sessions2), State2),
            {true, Category, State3};
        false when Counter > 0 ->
            case is_session(Type, State) of
                {true, _} ->
                    Sessions2 = Sessions#{Type=>Counter},
                    State2 = State#{sessions:=Sessions2},
                    {ok, Category, State3} = get_sessions_category(maps:keys(Sessions2), State2),
                    {true, Category, State3};
                false ->
                    false
            end;
        false ->
            false
    end.



%% ===================================================================
%% Util
%% ===================================================================
