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
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args, Session),
    lager:Type("NkDOMAN Admin (~s) " ++ Txt, [Session#admin_session.session_id|Args])).

-define(OVERVIEW,       <<"domain_tree_overview">>).
-define(DASHBOARD,      <<"domain_tree_overview_dashboard">>).
-define(DOMAINS,        <<"domain_tree_overview_domains">>).
-define(DOMAINS_ID,     <<"domain_tree_overview_domains_id">>).
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

get_category(resources, #admin_session{db_types=Types}=Session) ->
    get_resources_category(Types, Session);

get_category(sessions, Session) ->
    Types = nkdomain_all_types:get_all_types(),
    get_sessions_category(Types, Session);

get_category(_Category, _Session) ->
    continue.


%% @doc
%% The counter_updated event is sent multiple times to the domain and all parent domains (in obj_id)
event(#nkevent{type = <<"counter_updated">>, obj_id=ObjId}=Event, Updates, #admin_session{domain_path=ObjId}=Session) ->
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
            #{domain_id:=DomainId} = Body,
            created_domain(ObjId, DomainId, Updates2, Session2);
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
element_action([?DASHBOARD], selected, _Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"/dashboard">>, #{}, Updates, Session),
    {ok, Updates2, Session2};

element_action([?DOMAINS], selected, _Value, Updates, Session) ->
    {ok, Updates, Session};

element_action([?DOMAINS_ALL], selected, _Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"/domains">>, #{}, Updates, Session),
    {ok, Updates2, Session2};

element_action([?DOMAINS_ID, ObjId], selected, _Value, Updates, Session) ->
    {Updates2, Session2} = selected_domain(ObjId, Updates, Session),
    {ok, Updates2, Session2};

element_action([?ALERTS], selected, _Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"/alerts">>, #{}, Updates, Session),
    {ok, Updates2, Session2};

element_action([?RESOURCES, Type], selected, _Value, Updates, Session) ->
    {true, Info} = is_resource(Type, Session),
    Path = <<$/, (nkdomain_util:class(Type))/binary>>,
    case Info of
        #{type_view_mod:=Mod} ->
            {Detail, Session2} = Mod:view(Session),
            {Updates3, Session3} = nkadmin_util:update_detail(Path, Detail, Updates, Session2),
            {ok, Updates3, Session3};
        _ ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {Updates2, Session2} = nkadmin_util:update_detail(Path, #{}, Updates, Session),
            {ok, Updates2, Session2}
    end;

element_action([?RESOURCES, Type, Name], selected, _Value, Updates, Session) ->
    #admin_session{srv_id=SrvId} = Session,
    {true, Info} = is_resource(Type, Session),
    Path = <<$/, (nkdomain_util:class(Type))/binary, $/, Name/binary>>,
    case Info of
        #{obj_view_mod:=Mod} ->
            case nkdomain_lib:load(SrvId, Path) of
                #obj_id_ext{}=ObjIdExt ->
                    {Detail, Session2} = Mod:view(ObjIdExt, Session),
                    {Updates3, Session3} = nkadmin_util:update_detail(Path, Detail, Updates, Session2),
                    {ok, Updates3, Session3};
                {error, Error} ->
                    ?LLOG(notice, "error loading object ~s: ~p", [Path, Error], Session),
                    {error, object_load_error, Session}
            end;
        _ ->
            ?LLOG(notice, "type with no supported view: ~s", [Type], Session),
            {Updates2, Session2} = nkadmin_util:update_detail(Path, #{}, Updates, Session),
            {ok, Updates2, Session2}
    end;

element_action([?SESSIONS, Type], selected, _Value, Updates, Session) ->
    Path = <<$/, (nkdomain_util:class(Type))/binary>>,
    {Updates2, Session2} = nkadmin_util:update_detail(Path, #{}, Updates, Session),
    {ok, Updates2, Session2};

element_action(_Id, _Action, _Value, Updates, Session) ->
    {ok, Updates, Session}.



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
find_domains(#admin_session{srv_id=SrvId, domain_id=DomainId}) ->
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

get_domain_items([ObjId|Rest], Acc, #admin_session{srv_id=SrvId}=Session) ->
    case nkdomain:get_name(SrvId, ObjId) of
        {ok, Map} ->
             #{
                name := Name,
                obj_name := ObjName,
                path := Path
            } = Map,
            Value1 = #{label=>Name, path=>Path},
            Value2 = case Map of
                 #{description:=Desc} ->
                     Value1#{tooltip=>Desc};
                _ ->
                    Value1
            end,
            Id = << ?DOMAINS_ID/binary, "__", ObjId/binary>>,
            Item = nkadmin_util:menu_item(Id, menuEntry, Value2, Session),
            Session2 = nkadmin_util:set_url_key(<<$/, ObjName/binary>>, Id, Session),
            get_domain_items(Rest, [{ObjName, Item}|Acc], Session2);
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [ObjId, Error], Session),
            get_domain_items(Rest, Acc, Session)
    end.


%% @private
created_domain(ObjId, DomainId, Updates, #admin_session{domain_id=DomainId}=Session) ->
    #{domain_ids:=DomList1} = nkadmin_util:get_key_data(?DOMAINS_ALL, Session),
    DomList2 = nklib_util:store_value(ObjId, DomList1),
    Session2 = nkadmin_util:set_key_data(?DOMAINS_ALL, #{domain_ids=>DomList2}, Session),
    {ok, Item, Session3} = get_domains(DomList2, Session2),
    {[Item|Updates], Session3};

created_domain(_ObjId, _DomainId, Updates, Session) ->
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
selected_domain(ObjId, Updates, #admin_session{srv_id=SrvId}=Session) ->
    case nkdomain_lib:load(SrvId, ObjId) of
        #obj_id_ext{path=Path} ->
            {Updates2, Session2} = nkadmin_util:update_detail(Path, #{}, Updates, Session),
            {Updates2, Session2};
        {error, Error} ->
            ?LLOG(notice, "could not load domain ~s: ~p", [ObjId, Error], Session),
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
is_resource(Type, #admin_session{srv_id=SrvId}) ->
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
        {true, Info} ->
            Weight = maps:get(weight, Info, 9000),
            Key = << ?RESOURCES/binary, "__", Type/binary>>,
            Item = nkadmin_util:menu_item(Key, menuEntry, #{}, Session),
            #admin_session{resources=Resources} = Session,
            Session2 = case lists:member(Type, Resources) of
                true ->
                    Session;
                false ->
                    Session#admin_session{resources=[Type|Resources]}
            end,
            Class = nkdomain_util:class(Type),
            Session3 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session2),
            get_resource_items(Rest, [{Weight, Item}|Acc], Session3);
        _ ->
            get_resource_items(Rest, Acc, Session)
    end.


%% @private
update_resource(Type, Updates, #admin_session{resources=Resources}=Session) ->
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
is_session(Type, #admin_session{srv_id=SrvId}) ->
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
    #admin_session{srv_id=SrvId, domain_path=DomainPath} = Session,
    case is_session(Type, Session) of
        {true, Info} ->
            case SrvId:object_get_counter(SrvId, Type, DomainPath) of
                {ok, 0} ->
                    get_session_items(Rest, Acc, Session);
                {ok, Counter} ->
                    Weight = maps:get(weight, Info, 9000),
                    Key = <<?SESSIONS/binary, "__", Type/binary>>,
                    Item = nkadmin_util:menu_item(Key, menuEntry, #{counter=>Counter}, Session),
                    Class = nkdomain_util:class(Type),
                    #admin_session{sessions=Sessions} = Session,
                    Session2 = Session#admin_session{sessions=Sessions#{Type=>Counter}},
                    Session3 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session2),
                    get_session_items(Rest, [{Weight, Item}|Acc], Session3)
            end;
        _ ->
            get_session_items(Rest, Acc, Session)
    end.


%% @private
update_session(Type, Counter, #admin_session{sessions=Sessions}=Session) ->
    case maps:is_key(Type, Sessions) of
        true when Counter > 0 ->
            Session2 = Session#admin_session{sessions=Sessions#{Type=>Counter}},
            {ok, [Item], Session3} = get_session_items([Type], [], Session2),
            {true, Item, Session3};
        true ->
            Sessions2 = maps:remove(Type, Sessions),
            Session2 = Session#admin_session{sessions=Sessions2},
            {ok, Category, Session3} = get_sessions_category(maps:keys(Sessions2), Session2),
            {true, Category, Session3};
        false when Counter > 0 ->
            case is_session(Type, Session) of
                {true, _} ->
                    Sessions2 = Sessions#{Type=>Counter},
                    Session2 = Session#admin_session{sessions=Sessions2},
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
