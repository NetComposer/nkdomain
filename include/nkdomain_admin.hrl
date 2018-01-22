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

-ifndef(NKDOMAIN_ADMIN_HRL_).
-define(NKDOMAIN_ADMIN_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================


-define(ID_ADMIN_TREE_OVERVIEW,        <<"domain_tree_overview">>).
-define(ID_ADMIN_TREE_DASHBOARD,       <<"domain_tree_overview_dashboard">>).
-define(ID_ADMIN_TREE_DOMAINS,         <<"domain_tree_overview_domains">>).        % "Domains & Groups"
-define(ID_ADMIN_TREE_DOMAINS_DOMAIN,  <<"domain_tree_overview_domains_domain">>).
-define(ID_ADMIN_TREE_ALL_OBJS,        <<"domain_tree_overview_all_objs">>).      % "All objects"
-define(ID_ADMIN_TREE_ALERTS,          <<"domain_tree_overview_alerts">>).
-define(ID_ADMIN_TREE_RESOURCES,       <<"domain_tree_resources">>).
-define(ID_ADMIN_TREE_SESSIONS,        <<"domain_tree_sessions">>).
-define(ID_ADMIN_TREE_NETWORKS,        <<"domain_tree_networks">>).
-define(ID_ADMIN_TREE_SERVICES,        <<"domain_tree_services">>).

-define(ID_ADMIN_DETAIL_TYPE_VIEW,     <<"domain_detail_type_view">>).
-define(ID_ADMIN_DETAIL_TYPE_FILTER,   <<"domain_detail_type_filter">>).
-define(ID_ADMIN_DETAIL_TYPE_NEW,      <<"domain_detail_type_new">>).
-define(ID_ADMIN_DETAIL_OBJ_VIEW,      <<"domain_detail_obj_view">>).
-define(ID_ADMIN_DETAIL_OBJ_SUBVIEW,   <<"domain_detail_obj_subview">>).

-define(ADMIN_OBJ_ID,               <<"__admin_obj_id">>).
-define(ADMIN_OBJ_TYPE,             <<"__admin_obj_type">>).
-define(ADMIN_ALL_OBJS,             <<"__admin_all_objs">>).


-endif.

