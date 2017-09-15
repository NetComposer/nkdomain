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


-define(ADMIN_TREE_OVERVIEW,        <<"domain_tree_overview">>).
-define(ADMIN_TREE_DASHBOARD,       <<"domain_tree_overview_dashboard">>).
-define(ADMIN_TREE_DOMAINS,         <<"domain_tree_overview_domains">>).        % "Domains & Groups"
-define(ADMIN_TREE_DOMAINS_ALL,     <<"domain_tree_overview_domains_all">>).    % "All domains"
-define(ADMIN_TREE_DOMAINS_ID,      <<"domain_tree_overview_domains_id">>).
-define(ADMIN_TREE_ALERTS,          <<"domain_tree_overview_alerts">>).
-define(ADMIN_TREE_RESOURCES,       <<"domain_tree_resources">>).
-define(ADMIN_TREE_SESSIONS,        <<"domain_tree_sessions">>).
-define(ADMIN_TREE_NETWORKS,        <<"domain_tree_networks">>).
-define(ADMIN_TREE_SERVICES,        <<"domain_tree_services">>).

-define(ADMIN_DETAIL_TYPE_VIEW,     <<"domain_detail_type_view">>).
-define(ADMIN_DETAIL_OBJ_VIEW,      <<"domain_detail_obj_view">>).
-define(ADMIN_DETAIL_OBJ_SUBVIEW,   <<"domain_detail_obj_subview">>).

-define(ADMIN_OBJ_ID,               <<"__admin_obj_id">>).
-define(ADMIN_OBJ_TYPE,             <<"__admin_obj_type">>).
-define(ADMIN_ALL_OBJS,             <<"__admin_all_objs">>).


-endif.

