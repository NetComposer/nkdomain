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
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args, Session),
    lager:Type("NkDOMAN Admin (~s) " ++ Txt, [Session#admin_session.session_id|Args])).



%% ===================================================================
%% Public
%% ===================================================================

%% @doc 
categories(Data, Session) ->
    Admin = #{
        overview => 1000,
        resources => 1100,
        sessions => 1200,
        services => 1300,
        networks => 1400
    },
    {ok, maps:merge(Admin, Data), Session}.


%% @doc
get_category(overview, Session) ->
    case find_domains(Session) of
        {ok, DomainList} ->
            {ok, Dashboards, Session2} = get_dashboards(Session),
            {ok, Domains, Session3} = get_domains(DomainList, Session2),
            {ok, Objects, Session4} = get_objects(Session3),
            {ok, Alerts, Session5} = get_alerts(Session4),
            Items = [Dashboards, Domains, Objects, Alerts],
            Category = nkadmin_util:menu_item(?ADMIN_TREE_OVERVIEW, menuCategory, #{items=>Items}, Session5),
            {ok, Category, Session5};
        {error, Error} ->
            {error, Error, Session}
    end;

get_category(resources, #admin_session{db_types=Types}=Session) ->
    get_resources_category(Types, Session);

get_category(sessions, Session) ->
    Types = nkdomain_all_types:get_all_types(),
    get_sessions_category(Types, Session);

get_category(services, Session) ->
    Services = [sipstorm_c4],
    get_services_category(Services, Session);

get_category(_Category, _Session) ->
    continue.


%% @doc
%% The counter_updated event is sent multiple times to the domain and all parent domains (in obj_id)
event(#nkevent{type = <<"type_counter">>, obj_id=ObjId}=Event, Updates, #admin_session{domain_id=ObjId}=Session) ->
    #nkevent{body=#{type:=ObjType, counter:=Counter}}=Event,
    lager:warning("NKLOG ADMIN EVENT TYPE COUNTER ~p", [ObjType]),
    case update_session(ObjType, Counter, Session) of
        {true, Item, Session2} ->
            {continue, [Event, [Item|Updates], Session2]};
        false ->
            continue
    end;

event(#nkevent{type = <<"created">>, subclass=ObjType, obj_id=ObjId, body=Body}=Event, Updates, Session) ->
    {Updates2, Session2} = case ObjType of
        ?DOMAIN_DOMAIN ->
            #{domain_id:=DomainId} = Body,
            created_domain(ObjId, DomainId, Updates, Session);
        _ ->
            update_resource(ObjType, Updates, Session)
    end,
    {continue, [Event, Updates2, Session2]};

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


%% @doc See nkdomain_admin_detail:element_action/5
element_action([?ADMIN_TREE_DASHBOARD], selected, _Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"/dashboard">>, #{}, Updates, Session),
    {Updates3, Session3} = nkadmin_util:update_url(Updates2, Session2),
    {ok, Updates3, Session3};

%% "Domains & Groups"
element_action([?ADMIN_TREE_DOMAINS], selected, _Value, Updates, Session) ->
    {ok, Updates, Session};

%% "All Domains" Same as selecting the type "domain"
element_action([?ADMIN_TREE_ALL_OBJS], selected, _Value, Updates, Session) ->
    nkdomain_admin_detail:selected_type(?DOMAIN_DOMAIN, <<"/">>, Updates, Session);

element_action([?ADMIN_TREE_DOMAINS_ID, _ObjId, Path], selected, _Value, Updates, Session) ->
    nkdomain_admin_detail:selected_type(?DOMAIN_DOMAIN, Path, Updates, Session);

element_action([?ADMIN_TREE_ALERTS], selected, _Value, Updates, Session) ->
    {Updates2, Session2} = nkadmin_util:update_detail(<<"/alerts">>, #{}, Updates, Session),
    {Updates3, Session3} = nkadmin_util:update_url(Updates2, Session2),
    {ok, Updates3, Session3};

element_action([?ADMIN_TREE_RESOURCES, Type], selected, _Value, Updates, Session) ->
    nkdomain_admin_detail:selected_type(Type, <<"/">>, Updates, Session);

element_action([?ADMIN_TREE_SESSIONS, Type], selected, _Value, Updates, Session) ->
    nkdomain_admin_detail:selected_type(Type, <<"/">>, Updates, Session);

element_action(Id, Action, Value, Updates, Session) ->
    nkdomain_admin_detail:element_action(Id, Action, Value, Updates, Session).



%% ===================================================================
%% Dashboards
%% ===================================================================

%% @private
get_dashboards(Session) ->
    Session2 = nkadmin_util:set_special_url(<<"/dashboard">>, ?ADMIN_TREE_DASHBOARD, Session),
    {ok, nkadmin_util:menu_item(?ADMIN_TREE_DASHBOARD, menuEntry, #{}, Session), Session2}.



%% ===================================================================
%% Domains
%% ===================================================================


%% @private
find_domains(#admin_session{domain_id=DomainId}) ->
    Spec = #{
        filters => #{type => ?DOMAIN_DOMAIN}
    },
    case nkdomain_domain_obj:search_childs(DomainId, Spec) of
        {ok, _N, List} ->
            {ok, [ObjId || {_SrvId, ?DOMAIN_DOMAIN, ObjId, _Path} <- List]};
        {error, Error} ->
            {error, Error}
    end.


%% @private
%% We create a menuGroup (id ?DOMAINS) with entries for each domain
%% We set the url key "/domains"

get_domains(DomainList, Session) ->
    % We will generate a menu entry for each domain
    % Id will be ?DOMAINS_ID __ ObjId
    {ok, Items, Session2} = get_domain_items(DomainList, [], Session),
    %% Items2 = Items ++ [nkadmin_util:menu_item(?ADMIN_TREE_DOMAINS_ALL, menuEntry, #{}, Session2)],
    Element = nkadmin_util:menu_item(?ADMIN_TREE_DOMAINS, menuGroup, #{items=>Items}, Session2),
    {ok, Element, Session2}.


%% @private
get_domain_items([], [{_ObjName, Item}], Session) ->
    {ok, [Item], Session};

get_domain_items([], Acc, Session) ->
    {ok, [Item || {_ObjName, Item} <- lists:keysort(1, Acc)], Session};

get_domain_items([ObjId|Rest], Acc, Session) ->
    case nkdomain:get_name(ObjId) of
        {ok, Map} ->
             #{name := Name, obj_name := ObjName, path := Path} = Map,
            Value1 = #{label=>Name, path=>Path},
            Value2 = case Map of
                #{description:=Desc} ->
                    Value1#{tooltip=>Desc};
                _ ->
                    Value1
            end,
            Id = nkadmin_util:make_id([?ADMIN_TREE_DOMAINS_ID, ObjId, Path]),
            Item = nkadmin_util:menu_item(Id, menuEntry, Value2, Session),
            % Session2 = nkadmin_util:set_url_key(<<$/, ObjName/binary>>, Id, Session),
            get_domain_items(Rest, [{ObjName, Item}|Acc], Session);
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [ObjId, Error], Session),
            get_domain_items(Rest, Acc, Session)
    end.


%% @private
created_domain(ObjId, DomainId, Updates, #admin_session{domain_id=DomainId}=Session) ->
    #{domain_ids:=DomList1} = nkadmin_util:get_key_data(?ADMIN_TREE_ALL_OBJS, Session),
    DomList2 = nklib_util:store_value(ObjId, DomList1),
    Session2 = nkadmin_util:set_key_data(?ADMIN_TREE_ALL_OBJS, #{domain_ids=>DomList2}, Session),
    {ok, Item, Session3} = get_domains(DomList2, Session2),
    {[Item|Updates], Session3};

created_domain(_ObjId, _DomainId, Updates, Session) ->
    {Updates, Session}.


%% @private
updated_domain(ObjId, Updates, Session) ->
    #{domain_ids:=DomList} = nkadmin_util:get_key_data(?ADMIN_TREE_ALL_OBJS, Session),
    case lists:member(ObjId, DomList) of
        true ->
            {ok, [Item], Session2} = get_domain_items([ObjId], [], Session),
            {[Item|Updates], Session2};
        false ->
            {Updates, Session}
    end.


%% @private
deleted_domain(ObjId, Updates, Session) ->
    #{domain_ids:=DomList} = nkadmin_util:get_key_data(?ADMIN_TREE_ALL_OBJS, Session),
    case lists:member(ObjId, DomList) of
        true ->
            DomList2 = DomList -- [ObjId],
            Session2 = nkadmin_util:set_key_data(?ADMIN_TREE_ALL_OBJS, #{domain_ids=>DomList2}, Session),
            {ok, Item, Session3} = get_domains(DomList2, Session2),
            {[Item|Updates], Session3};
        false ->
            {Updates, Session}
    end.


%%%% @private
%%selected_domain(ObjId, Updates, Session) ->
%%    case nkdomain_lib:load(ObjId) of
%%        #obj_id_ext{path=Path} ->
%%            {Updates2, Session2} = nkadmin_util:update_detail(Path, #{}, Updates, Session),
%%            {Updates3, Session3} = nkadmin_util:update_url(Updates2, Session2),
%%            {Updates3, Session3};
%%        {error, Error} ->
%%            ?LLOG(notice, "could not load domain ~s: ~p", [ObjId, Error], Session),
%%            {Updates, Session}
%%    end.


%% ===================================================================
%% Objects
%% ===================================================================

%% @private
get_objects(Session) ->
    %Session2 = nkadmin_util:set_special_url(<<"/dashboard">>, ?ADMIN_TREE_DASHBOARD, Session),
    Item = nkadmin_util:menu_item(?ADMIN_TREE_ALL_OBJS, menuEntry, #{}, Session),
    {ok, Item, Session}.



%% ===================================================================
%% Alerts
%% ===================================================================

%% @private
get_alerts(Session) ->
    Session2 = nkadmin_util:set_special_url(<<"/alerts">>, ?ADMIN_TREE_DASHBOARD, Session),
    Item = nkadmin_util:menu_item(?ADMIN_TREE_ALERTS, menuEntry, #{badge=>3}, Session),
    {ok, Item, Session2}.


%% ===================================================================
%% Resources
%% ===================================================================

%% @private
get_resources_category(Types, Session) ->
    {ok, Items, Session2} = get_resource_items(Types, [], Session),
    Category = nkadmin_util:menu_item(?ADMIN_TREE_RESOURCES, menuCategory, #{items=>Items}, Session2),
    {ok, Category, Session2}.


%% @private
is_resource(Type, _Session) ->
    case ?CALL_NKROOT(object_admin_info, [Type]) of
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
            Key = <<?ADMIN_TREE_RESOURCES/binary, "__", Type/binary>>,
            Item = nkadmin_util:menu_item(Key, menuEntry, #{}, Session),
            #admin_session{resources=Resources} = Session,
            Session2 = case lists:member(Type, Resources) of
                true ->
                    Session;
                false ->
                    Session#admin_session{resources=[Type|Resources]}
            end,
            % Class = nkdomain_util:class(Type),
            %Session3 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session2),
            get_resource_items(Rest, [{Weight, Item}|Acc], Session2);
        false ->
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
    Category = nkadmin_util:menu_item(?ADMIN_TREE_SESSIONS, menuCategory, #{items=>Items}, Session2),
    {ok, Category, Session2}.


%% @private
is_session(Type, _Session) ->
    case ?CALL_NKROOT(object_admin_info, [Type]) of
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
    #admin_session{domain_id=DomainId} = Session,
    case is_session(Type, Session) of
        {true, Info} ->
            case nkdomain_domain_obj:get_counter(DomainId, Type) of
                {ok, 0} ->
                    get_session_items(Rest, Acc, Session);
                {ok, Counter} ->
                    Weight = maps:get(weight, Info, 9000),
                    Key = <<?ADMIN_TREE_SESSIONS/binary, "__", Type/binary>>,
                    Item = nkadmin_util:menu_item(Key, menuEntry, #{counter=>Counter}, Session),
                    #admin_session{sessions=Sessions} = Session,
                    Session2 = Session#admin_session{sessions=Sessions#{Type=>Counter}},
                    % Class = nkdomain_util:class(Type),
                    %Session3 = nkadmin_util:set_url_key(<<$/, Class/binary>>, Key, Session2),
                    get_session_items(Rest, [{Weight, Item}|Acc], Session2)
            end;
        false ->
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
%% Services
%% ===================================================================

%% @private
get_services_category(Ids, Session) ->
    {ok, Items, Session2} = get_service_items(Ids, [], Session),
    Category = nkadmin_util:menu_item(?ADMIN_TREE_SERVICES, menuCategory, #{items=>Items}, Session2),
    {ok, Category, Session2}.


%% @private
get_service_items([], [{_Weight, Item}], Session) ->
    {ok, [Item], Session};

get_service_items([], Acc, Session) ->
    {ok, [Item || {_Weigth, Item} <- lists:keysort(1, Acc)], Session};

get_service_items([Id|Rest], Acc, Session) ->
    Id2 = nklib_util:to_binary(Id),
    Key = <<?ADMIN_TREE_SERVICES/binary, "__", Id2/binary>>,
    Item = nkadmin_util:menu_item(Key, menuEntry, #{label=>Id2}, Session),

    get_service_items(Rest, [{1000, Item}|Acc], Session).
