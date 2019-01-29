%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

-ifndef(NKDOMAIN_HRL_).
-define(NKDOMAIN_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================

-define(PACKAGE_CLASS_DOMAIN, <<"Domains">>).
%-define(DOMAIN_PKG_ID, <<"domains-pkg">>).
-define(DOMAIN_PKG_ID_API, <<"domains-pkg-api">>).
-define(DOMAIN_PKG_ID_GRAPHIQL, <<"domains-pkg-graphiql">>).
-define(DOMAIN_PKG_ID_FILE, <<"nkdomain-file">>).
-define(DOMAIN_PKG_ID_NOTIFY, <<"nkdomain-notify">>).

-define(ROOT_SRV, 'nkdomain-root').

-define(GROUP_CORE, <<"core">>).
-define(GROUP_CORE_V1A1, <<"v1a1">>).
-define(GROUP_SEARCH, <<"search">>).
-define(GROUP_BULK, <<"bulk">>).

-define(RES_CORE_DOMAINS, <<"domains">>).
-define(RES_CORE_ACCESS_IDS, <<"accessids">>).
-define(RES_CORE_EVENTS, <<"events">>).
-define(RES_CORE_USERS, <<"users">>).
-define(RES_CORE_CONTACTS, <<"contacts">>).
-define(RES_CORE_TOKENS, <<"tokens">>).
-define(RES_CORE_CONFIGMAPS, <<"configmaps">>).
-define(RES_CORE_TASKS, <<"tasks">>).
-define(RES_CORE_SESSIONS, <<"sessions">>).
-define(RES_CORE_FILES, <<"files">>).
-define(RES_CORE_FILE_PROVIDERS, <<"fileproviders">>).
-define(RES_CORE_NODES, <<"nodes">>).

-define(KIND_CORE_DOMAIN, <<"Domain">>).
-define(KIND_CORE_EVENT, <<"Event">>).

-define(LINK_CORE_DOMAIN, <<"domain">>).
-define(LINK_CORE_FILE_PROVIDER, <<"file-provider">>).
-define(LINK_CORE_CONTACT_USER, <<"contact-user">>).





-endif.

