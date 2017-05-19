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

-module(nkdomain_i18n).
-behavior(nklib_i18n).

-export([i18n/0, reload/0]).


i18n() -> #{
    en => #{
        domain_tree_overview => "Overview",
        domain_tree_overview_dashboard => "Dashboard",
        domain_tree_overview_domains => "Domains & Groups",
        domain_tree_overview_domains_all => "All Domains & Groups",
        domain_tree_overview_alerts => "Alerts",

        domain_tree_resources => "Resources",
        domain_tree_resources_users => "Users",
        domain_tree_resources_configs => "Configurations",
        domain_tree_resources_chat_messages => "Chat Messages",
        domain_tree_resources_chat_conversations => "Conversations",

        domain_tree_sessions => "Sessions",
        domain_tree_sessions_login => "Login",
        domain_tree_sessions_tokens => "Tokens",
        domain_tree_sessions_chat_sessions => "Chat",
        domain_tree_sessions_admin => "Admin",

        domain_tree_networks => "Networks",
        domain_tree_services => "Services"
    },
    es => #{
        domain_tree_overview => "General",
        domain_tree_overview_dashboard => <<"Información"/utf8>>,
        domain_tree_overview_domains => "Dominios & Grupos",
        domain_tree_overview_domains_all => "Todos los Dominios & Grupos",
        domain_tree_overview_alerts => "Alertas",

        domain_tree_resources => "Recursos",
        domain_tree_resources_users => "Usuarios",
        domain_tree_resources_configs => "Configuraciones",
        domain_tree_resources_chat_messages => "Mensajes de chat",
        domain_tree_resources_chat_conversations => "Conversaciones",

        domain_tree_sessions => "Sesiones",
        domain_tree_sessions_login => "Login",
        domain_tree_sessions_tokens => "Tokens",
        domain_tree_sessions_chat_sessions => "Chat",
        domain_tree_sessions_admin => "Admin",

        domain_tree_networks => "Redes",
        domain_tree_services => "Servicios"
    }
}.


reload() ->
    ok = nklib_i18n:load(?MODULE).