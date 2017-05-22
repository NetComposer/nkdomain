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
-module(nkdomain_admin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([tree_categories/2, tree_get_category/2, event/3, element_action/4]).
-export([add_tree_resource/5, add_tree_session/7]).

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
tree_categories(Data, State) ->
    Admin = #{
        overview => 1000,
        resources => 1100,
        sessions => 1200,
        networks => 1300,
        services => 1400
    },
    {ok, maps:merge(Admin, Data), State}.


%% @doc
tree_get_category(overview, #{srv_id:=SrvId}=State) ->
    {ok, Domains, State2} = get_domains(SrvId, State),
    Entries = [
        nkadmin_util:menu_item(?DASHBOARD, menuSimple, State),
        nkadmin_util:menu_item(?DOMAINS, {menuGroup, Domains}, State),
        nkadmin_util:menu_item(?ALERTS, {menuBadge, 3}, State)
    ],
    Category = nkadmin_util:menu_item(?OVERVIEW, {menuCategory, Entries}, State),
    {ok, Category, State2};

tree_get_category(Category, State)
    when Category==resources; Category==sessions; Category==networks; Category==services ->
    #{types:=Types1} = State,
    get_category(Category, maps:keys(Types1), State);

tree_get_category(_Category, _State) ->
    continue.


%% @doc
event(#nkevent{class = ?DOMAIN_EVENT_CLASS, type = <<"counter_updated">>, obj_id=ObjId}=Event, Updates,
      #{domain_path:=ObjId}=State) ->
    #nkevent{subclass=ObjType, body=#{counter:=Counter}}=Event,
    % lager:error("NKLOG EVENT! ~p", [ObjType]),
    #{types:=Types, session_types:=SessTypes} = State,
    Types2 = Types#{ObjType => Counter},
    SessTypes2 = SessTypes#{ObjType => Counter},
    State2 = State#{types:=Types2, session_types:=SessTypes2},
    case maps:is_key(ObjType, Types) of
        true ->
            case do_get_category_entries(sessions, [ObjType], [], State2) of
                {ok, [], State3} ->
                    {continue, [Event, Updates, State3]};
                {ok, [{Data, _Weigth}], State3} ->
                    lager:warning("NKLOG IS MEMBER: ~p", [Data]),
                    {continue, [Event, [Data|Updates], State3]}
            end;
        false ->
            lager:warning("NKLOG IS NOT MEMBER"),
            {ok, Data, State3} = get_category(sessions, maps:keys(Types2), State2),
            {continue, [Event, [Data|Updates], State3]}
    end;

event(#nkevent{class = ?DOMAIN_EVENT_CLASS, subclass = ?DOMAIN_DOMAIN}=Event, Updates, State) ->
    #nkevent{obj_id=ObjId, type=Type, body=Body} = Event,
    Group = nkadmin_util:get_group(?DOMAINS_ID, State),
    case event_domain(ObjId, Type, Body, Group, State) of
        {ok, Element, State2} ->
            {continue, [Event, [Element|Updates], State2]};
        continue ->
            continue
    end;

event(#nkevent{class = ?DOMAIN_EVENT_CLASS, type = <<"created">>}=Event, Updates, State) ->
    #nkevent{subclass=ObjType} = Event,
    #{types:=Types} = State,
    case maps:is_key(ObjType, Types) of
        true ->
            continue;
        false ->
            Keys = maps:keys(Types),
            case get_category(resources, [ObjType|Keys], State) of
                {ok, Element, State2} when map_size(Element)==0 ->
                    {continue, [Event, Updates, State2]};
                {ok, Element, State2} ->
                    {continue, [Event, [Element|Updates], State2]}
            end
    end;

event(Event, Updates, State) ->
    {continue, [Event, Updates, State]}.


%% @doc
element_action(?DOMAINS_ALL, selected, _Value, State) ->
    {ok, #{detail=>#{id=>detail_domains_table, class=>table}}, State};

element_action(<<?DOMAINS_ID2, $_, ObjId/binary>>, selected, _Value, State) ->
    {ok, #{detail=>#{id=>detail_domain_page, class=>ui, obj_id=>ObjId}}, State};

element_action(ElementId, Action, Value, State) ->
    lager:error("NKLOG DOM ACT ~p", [{ElementId, Action, Value}]),
    {continue, [ElementId, Action, Value, State]}.


%% ===================================================================
%% Domains
%% ===================================================================

%% @private
get_domains(SrvId, #{domain_id:=DomainId}=State) ->
    Spec = #{
        filters => #{type => ?DOMAIN_DOMAIN}
    },
    {ok, _N, List} = nkdomain_domain_obj:find_childs(SrvId, DomainId, Spec),
    DomainList = [ObjId || {?DOMAIN_DOMAIN, ObjId, _Path} <- List],
    do_get_domains(SrvId, DomainList, [], State).


%% @private
update_domains(ObjIds, #{srv_id:=SrvId}=State) ->
    {ok, Domains, State2} = do_get_domains(SrvId, ObjIds, [], State),
    {ok, nkadmin_util:menu_item(?DOMAINS, {menuGroup, Domains}, State2), State2}.


%% @private
do_get_domains(_SrvId, [], Acc, State) ->
    List = [Element || {_ObjName, Element} <- lists:sort(Acc)] ++
           [nkadmin_util:menu_item(?DOMAINS_ALL, menuSimple, State)],
    {ok, List, State};

do_get_domains(SrvId, [ObjId|Rest], Acc, State) ->
    case nkdomain_obj:get_name(ObjId) of
        {ok, #{
            name := Name,
            obj_name := ObjName
        }} ->
            Id = get_domain_id(ObjId),
            Element = nkadmin_util:menu_item(Id, {menuSimple, ObjName, Name}, State),
            State2 = nkadmin_util:add_element(?DOMAINS_ID, ObjId, ok, State),
            do_get_domains(SrvId, Rest, [{ObjName, Element}|Acc], State2);
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [ObjId, Error]),
            do_get_domains(SrvId, Rest, Acc, State)
    end.


%% @private
get_domain_id(ObjId) ->
    << ?DOMAINS_ID/binary, $_, ObjId/binary>>.


%% @private
event_domain(ObjId, <<"updated">>, _Body, Group, State) ->
    case maps:is_key(ObjId, Group) of
        true ->
            ObjIds = maps:keys(Group),
            update_domains(ObjIds, State);
        false ->
            continue
    end;

event_domain(ObjId, <<"deleted">>, _Body, Group, State) ->
    case maps:is_key(ObjId, Group) of
        true ->
            ObjIds1 = maps:keys(Group),
            ObjIds2 = ObjIds1 -- [ObjId],
            State2 = nkadmin_util:remove_element(?DOMAINS_ID, ObjId, State),
            update_domains(ObjIds2, State2);
        false ->
            continue
    end;

event_domain(ObjId, <<"created">>, #{parent_id:=DomId}, Group, #{domain_id:=DomId}=State) ->
    ObjIds = [ObjId|maps:keys(Group)],
    update_domains(ObjIds, State);

event_domain(_ObjId, _Type, _Body, _Group, _State) ->
    continue.



%% ===================================================================
%% Categories
%% ===================================================================

get_category(Category, Types, State) ->
    case do_get_category_entries(Category, Types, [], State) of
        {ok, [], State2} ->
            {ok, #{}, State2};
        {ok, Entries, State2} ->
            Id = case Category of
                resources -> ?RESOURCES;
                sessions -> ?SESSIONS;
                networks -> ?NETWORKS;
                services -> ?SERVICES
            end,
            Entries2 = [E || {E, _} <- lists:keysort(2, Entries)],
            Data = nkadmin_util:menu_item(Id, {menuCategory, Entries2}, State2),
            {ok, Data, State2}
    end.


%% @private
do_get_category_entries(_Category, [], List, State) ->
    {ok, List, State};

do_get_category_entries(Category, [Type|Rest], List, State) ->
    case nkdomain_obj_util:call_type(object_admin_tree, [Category, List, State], Type) of
        ok ->
            do_get_category_entries(Category, Rest, List, State);
        {ok, List2} ->
            do_get_category_entries(Category, Rest, List2, State);
        {ok, List2, State2} ->
            do_get_category_entries(Category, Rest, List2, State2)
    end.


%% ===================================================================
%% Util
%% ===================================================================

add_tree_resource(resources, Key, Weigth, List, State) ->
    Item = nkadmin_util:menu_item(Key, menuSimple, State),
    {ok, [{Item, Weigth}|List]};

add_tree_resource(_Category, _Key, _Weigth, _List, _State) ->
    ok.


%% @doc
add_tree_session(sessions, Type, Module, Key, Weight, List, #{domain_path:=Domain, session_types:=Types}=State) ->
    Num = case maps:find(Type, Types) of
        {ok, Num0} ->
            Num0;
        error ->
            {ok, Num0} = nkdomain_type:get_global_counters(Module, Domain),
            Num0
    end,
    case Num of
        0 ->
            ok;
        _ ->
            Item = nkadmin_util:menu_item(Key, {menuBadge, Num}, State),
            {ok, [{Item, Weight}|List], State#{session_types:=Types#{Type=>Num}}}
    end;

add_tree_session(_Category, _Type, _Module, _Key, _Weigth, _List, _State) ->
    ok.