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
-include_lib("nkadmin/include/nkadmin.hrl").

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
categories(Data, Session) ->
    Admin = #{
        overview => 1000,
        resources => 1100,
        sessions => 1200,
        networks => 1300,
        services => 1400
    },
    {ok, maps:merge(Admin, Data), Session}.


%% @doc
get_category(overview, Session) ->
    case find_domains(Session) of
        {ok, DomainList} ->
            {ok, Domains, Session2} = get_domains(DomainList, Session),
            {ok, Dashboards, Session3} = get_dashboards(Session2),
            {ok, Alerts, Session4} = get_alerts(Session3),
            Items = [Dashboards, Domains, Alerts],
            Category = nkadmin_util:menu_item(?OVERVIEW, menuCategory, #{items=>Items}, Session4),
            {ok, Category, Session4};
        {error, Error} ->
            {error, Error, Session}
    end;

get_category(resources, #{types:=Types}=Session) ->
    get_resources_category(Types, Session);

get_category(sessions, #{types:=Types}=Session) ->
    get_sessions_category(Types, Session);

get_category(_Category, _Session) ->
    continue.


%% @doc
event(#nkevent{type = <<"counter_updated">>, obj_id=ObjId}=Event, Updates, #{domain_path:=ObjId}=Session) ->
    #nkevent{subclass=ObjType, body=#{counter:=Counter}}=Event,
    case update_session(ObjType, Counter, Session) of
        {true, Item, Session2} ->
            {continue, [Event, [Item|Updates], Session2]};
        false ->
            continue
    end;

event(#nkevent{type = <<"created">>, subclass=ObjType, obj_id=ObjId, body=Body}=Event, Updates, Session) ->
    {Updates2, Session2} = update_resource(ObjType, Updates, Session),
    {Updates3, Session3} = case ObjType of
        ?DOMAIN_DOMAIN ->
            #{parent_id:=ParentId} = Body,
            created_domain(ObjId, ParentId, Updates2, Session2);
        _ ->
            {Updates2, Session2}
    end,
    {continue, [Event, Updates3, Session3]};

event(#nkevent{type = <<"updated">>, subclass=ObjType, obj_id=ObjId}=Event, Updates, Session) ->
    {Updates2, Session2} = case ObjType of
        ?DOMAIN_DOMAIN ->
            updated_domain(ObjId, Updates, Session);
        _ ->
            {Updates, Session}
    end,
    {continue, [Event, Updates2, Session2]};

event(#nkevent{type = <<"deleted">>, subclass=ObjType, obj_id=ObjId}=Event, Updates, Session) ->
    {Updates2, Session2} = case ObjType of
        ?DOMAIN_DOMAIN ->
            deleted_domain(ObjId, Updates, Session);
        _ ->
            {Updates, Session}
    end,
    {continue, [Event, Updates2, Session2]};

event(Event, Updates, Session) ->
    {continue, [Event, Updates, Session]}.



%% @doc
element_action(?DASHBOARD, selected, Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"dashboard">>, #{}, Updates, Session),
    {continue, [?DASHBOARD, selected, Value, Updates2, Session2]};

element_action(<<?DOMAINS_ID2, $_, ObjId/binary>>=Key, selected, Value, Updates, Session) ->
    {Updates2, Session2} = selected_domain(ObjId, Updates, Session),
    {continue, [Key, selected, Value, Updates2, Session2]};

element_action(?DOMAINS_ALL, selected, Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"domains">>, #{}, Updates, Session),
    {continue, [?DOMAINS_ALL, selected, Value, Updates2, Session2]};

element_action(?ALERTS, selected, Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"alerts">>, #{}, Updates, Session),
    {continue, [?ALERTS, selected, Value, Updates2, Session2]};

element_action(<<"domain_tree_resources_users">>=Key, selected, Value, Updates, Session) ->
    #{domain_id:=_DomainId} = Session,
    Table = nkdomain_user_obj_ui:table2(Session), %root, DomainId),
    Detail = #{
        id => <<"domain_detail_user_table">>,
        class => webix_ui,
        value => Table
    },
    {Updates2, Session2} = nkadmin_util:update_detail(<<"users">>, Detail, Updates, Session),
    {continue, [Key, selected, Value, Updates2, Session2]};

element_action(<<"domain_tree_resources_", Type/binary>>=Key, selected, Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(Type, #{}, Updates, Session),
    {continue, [Key, selected, Value, Updates2, Session2]};

element_action(<<"domain_tree_sessions_", Type/binary>>=Key, selected, Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(Type, #{}, Updates, Session),
    {continue, [Key, selected, Value, Updates2, Session2]};

element_action(_Id, _Action, _Value, _Updates, _Session) ->
    continue.



%% ===================================================================
%% Dashboards
%% ===================================================================

%% @private
get_dashboards(Session) ->
    Session2 = nkadmin_util:set_url_key(<<"/dashboard">>, ?DASHBOARD, Session),
    {ok, nkadmin_util:menu_item(?DASHBOARD, menuEntry, #{}, Session), Session2}.



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
get_domains(DomainList, Session) ->
    {ok, Items, Session2} = get_domain_items(DomainList, [], Session),
    Items2 = Items ++ [nkadmin_util:menu_item(?DOMAINS_ALL, menuEntry, #{}, Session2)],
    Element = nkadmin_util:menu_item(?DOMAINS, menuGroup, #{items=>Items2}, Session2),
    Session3 = nkadmin_util:set_key_data(?DOMAINS_ALL, #{domain_ids=>DomainList}, Session2),
    Session4 = nkadmin_util:set_url_key(<<"/domains">>, ?DOMAINS_ALL, Session3),
    {ok, Element, Session4}.


%% @private
get_domain_items([], [{_ObjName, Item}], Session) ->
    {ok, [Item], Session};

get_domain_items([], Acc, Session) ->
    {ok, [Item || {_ObjName, Item} <- lists:sort(Acc)], Session};

get_domain_items([ObjId|Rest], Acc, Session) ->
    case nkdomain_obj:get_name(ObjId) of
        {ok, #{
            name := Name,
            obj_name := ObjName,
            description := Desc,
            path := Path
        }} ->
            Id = get_domain_id(ObjId),
            Value = #{label=>Name, tooltip=>Desc, path=>Path},
            Item = nkadmin_util:menu_item(Id, menuEntry, Value, Session),
            Session2 = nkadmin_util:set_url_key(<<$/, Name/binary>>, Id, Session),
            get_domain_items(Rest, [{ObjName, Item}|Acc], Session2);
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [ObjId, Error]),
            get_domain_items(Rest, Acc, Session)
    end.


%% @private
get_domain_id(ObjId) ->
    << ?DOMAINS_ID/binary, $_, ObjId/binary>>.


%% @private
created_domain(ObjId, ParentId, Updates, #{domain_id:=ParentId}=Session) ->
    #{domain_ids:=DomList1} = nkadmin_util:get_key_data(?DOMAINS_ALL, Session),
    DomList2 = nklib_util:store_value(ObjId, DomList1),
    Session2 = nkadmin_util:set_key_data(?DOMAINS_ALL, #{domain_ids=>DomList2}, Session),
    {ok, Item, Session3} = get_domains(DomList2, Session2),
    {[Item|Updates], Session3};

created_domain(_ObjId, _ParentId, Updates, Session) ->
    {Updates, Session}.


%% @private
updated_domain(ObjId, Updates, Session) ->
    #{domain_ids:=DomList} = nkadmin_util:get_key_data(?DOMAINS_ALL, Session),
    case lists:member(ObjId, DomList) of
        true ->
            {ok, [Item], Session2} = get_domain_items([ObjId], [], Session),
            {[Item|Updates], Session2};
        false ->
            {Updates, Session}
    end.


%% @private
deleted_domain(ObjId, Updates, Session) ->
    #{domain_ids:=DomList} = nkadmin_util:get_key_data(?DOMAINS_ALL, Session),
    case lists:member(ObjId, DomList) of
        true ->
            DomList2 = DomList -- [ObjId],
            Session2 = nkadmin_util:set_key_data(?DOMAINS_ALL, #{domain_ids=>DomList2}, Session),
            {ok, Item, Session3} = get_domains(DomList2, Session2),
            {[Item|Updates], Session3};
        false ->
            {Updates, Session}
    end.


%% @private
selected_domain(ObjId, Updates, #{srv_id:=SrvId}=Session) ->
    case nkdomain_obj_lib:load(SrvId, ObjId, #{}) of
        #obj_id_ext{path=Path} ->
            {Updates2, Session2} = nkadmin_util:update_path_absolute(Path, Updates, Session),
            {Updates2, Session2};
        {error, Error} ->
            lager:error("NKLOG SHOW VIEW ERROR: ~p", [Error]),
            {Updates, Session}
    end.


%% ===================================================================
%% Alerts
%% ===================================================================

%% @private
get_alerts(Session) ->
    Session2 = nkadmin_util:set_url_key(<<"/alerts">>, ?DASHBOARD, Session),
    Item = nkadmin_util:menu_item(?ALERTS, menuEntry, #{badge=>3}, Session),
    {ok, Item, Session2}.


%% ===================================================================
%% Resources
%% ===================================================================

%% @private
get_resources_category(Types, Session) ->
    {ok, Items, Session2} = get_resource_items(Types, [], Session),
    Category = nkadmin_util:menu_item(?RESOURCES, menuCategory, #{items=>Items}, Session2),
    {ok, Category, Session2}.


%% @private
is_resource(Type, #{srv_id:=SrvId}) ->
    case SrvId:object_admin_info(Type) of
        #{class:=resource} = Info ->
            {true, Info};
        _ ->
            false
    end.


%% @private
get_resource_items([], [{_Weight, Item}], Session) ->
    {ok, [Item], Session};

get_resource_items([], Acc, Session) ->
    {ok, [Item || {_Weigth, Item} <- lists:keysort(1, Acc)], Session};

get_resource_items([Type|Rest], Acc, Session) ->
    case is_resource(Type, Session) of
        {true, #{tree_id:=Key}=Info} ->
            Weight = maps:get(weight, Info, 9000),
            Item = nkadmin_util:menu_item(Key, menuEntry, #{}, Session),
            #{resources:=Resources} = Session,
            Session2 = case lists:member(Type, Resources) of
                true -> Session;
                false -> ?ADMIN_SESSION(resources, [Type|Resources], Session)
            end,
            Class = nkdomain_util:class(Type),
            Session3 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session2),
            get_resource_items(Rest, [{Weight, Item}|Acc], Session3);
        _ ->
            get_resource_items(Rest, Acc, Session)
    end.


%% @private
update_resource(Type, Updates, #{resources:=Resources}=Session) ->
    case lists:member(Type, Resources) of
        true ->
            {Updates, Session};
        false ->
            case is_resource(Type, Session) of
                {true, _} ->
                    Resources2 = [Type|Resources],
                    {ok, Category, Session2} = get_resources_category(Resources2, Session),
                    {[Category|Updates], Session2};
                false ->
                    {Updates, Session}
            end
    end.




%% ===================================================================
%% Sessions
%% ===================================================================

%% @private
get_sessions_category(Types, Session) ->
    {ok, Items, Session2} = get_session_items(Types, [], Session),
    Category = nkadmin_util:menu_item(?SESSIONS, menuCategory, #{items=>Items}, Session2),
    {ok, Category, Session2}.


%% @private
is_session(Type, #{srv_id:=SrvId}) ->
    case SrvId:object_admin_info(Type) of
        #{class:=session} = Info ->
            {true, Info};
        _ ->
            false
    end.


%% @private
get_session_items([], [{_Weight, Item}], Session) ->
    {ok, [Item], Session};

get_session_items([], Acc, Session) ->
    {ok, [Item || {_Weigth, Item} <- lists:keysort(1, Acc)], Session};

get_session_items([Type|Rest], Acc, Session) ->
    case is_session(Type, Session) of
        {true, #{tree_id:=Key}=Info} ->
            Weight = maps:get(weight, Info, 9000),
            #{sessions:=Sessions} = Session,
            case maps:find(Type, Sessions) of
                {ok, 0} ->
                    get_session_items(Rest, Acc, Session);
                {ok, Counter} ->
                    Item = nkadmin_util:menu_item(Key, menuEntry, #{counter=>Counter}, Session),
                    Class = nkdomain_util:class(Type),
                    Session2 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session),
                    get_session_items(Rest, [{Weight, Item}|Acc], Session2);
                error ->
                    #{srv_id:=SrvId, domain_path:=DomainPath} = Session,
                    case SrvId:object_get_counter(Type, DomainPath) of
                        {ok, 0} ->
                            get_session_items(Rest, Acc, Session);
                        {ok, Counter} ->
                            Item = nkadmin_util:menu_item(Key, menuEntry, #{counter=>Counter}, Session),
                            Class = nkdomain_util:class(Type),
                            Session2 = ?ADMIN_SESSION(sessions, Sessions#{Type=>Counter}, Session),
                            Session3 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session2),
                            get_session_items(Rest, [{Weight, Item}|Acc], Session3)
                    end
            end;
        _ ->
            get_session_items(Rest, Acc, Session)
    end.


%% @private
update_session(Type, Counter, #{sessions:=Sessions}=Session) ->
    case maps:is_key(Type, Sessions) of
        true when Counter > 0 ->
            Session2 = Session#{sessions:=Sessions#{Type=>Counter}},
            {ok, [Item], Session3} = get_session_items([Type], [], Session2),
            {true, Item, Session3};
        true ->
            Sessions2 = maps:remove(Type, Sessions),
            Session2 = Session#{sessions:=Sessions2},
            {ok, Category, Session3} = get_sessions_category(maps:keys(Sessions2), Session2),
            {true, Category, Session3};
        false when Counter > 0 ->
            case is_session(Type, Session) of
                {true, _} ->
                    Sessions2 = Sessions#{Type=>Counter},
                    Session2 = Session#{sessions:=Sessions2},
                    {ok, Category, Session3} = get_sessions_category(maps:keys(Sessions2), Session2),
                    {true, Category, Session3};
                false ->
                    false
            end;
        false ->
            false
    end.



%% ===================================================================
%% Util
%% ===================================================================
