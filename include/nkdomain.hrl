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

-ifndef(NKDOMAIN_HRL_).
-define(NKDOMAIN_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================

-define(ADD_TO_OBJ(Key, Val, Obj), maps:put(Key, Val, Obj)).
-define(ADD_TO_OBJ(Update, Obj), maps:merge(Obj, Update)).
-define(ADD_TO_OBJ_DEEP(Update, Obj), nklib_util:map_merge(Update, Obj)).
-define(REMOVE_FROM_OBJ(Key, Obj), maps:remove(Key, Obj)).

-record(obj_session, {
    type :: binary(),
    obj_id :: nkdomain:obj_id(),
    path :: nkdomain:path(),
    module :: module(),
    name :: nkdomain:name(),
    parent_id :: nkdomain:obj_id(),
    obj :: nkdomain:obj(),
    srv_id :: nkservice:id(),
    is_dirty :: boolean(),
    is_enabled :: boolean(),
    is_created :: boolean(),
    status :: nkdomain_obj:status(),
    meta :: map(),                      % Object load metadata
    data :: term(),                     % Type-specific metadata
    started :: nklib_util:m_timestamp(),
    childs :: #{nkdomain:type() => #{nkdomain:name() => nkdomain:obj_id()}},
    link_usages = #{} :: #{term() => ok},
    link_events = [] :: [term()]

}).

-record(obj_id_ext, {
    srv_id :: nkservice:id(),
    type :: nkdomain:type(),
    obj_id :: nkdomain:obj_id(),
    path :: nkdomain:path(),
    pid :: pid() | undefined
}).




-define(DOMAIN_USER, <<"user">>).
-define(DOMAIN_DOMAIN, <<"domain">>).
-define(DOMAIN_SESSION, <<"session">>).
-define(DOMAIN_TOKEN, <<"token">>).
-define(DOMAIN_CONFIG, <<"config">>).

-define(DOMAIN_MAIL, <<"mail">>).
-define(DOMAIN_MAIL_CONFIG, <<"mail.config">>).

-define(DOMAIN_EVENT_CLASS, <<"domain">>).
-define(DEF_TOKEN_TTL, 8*60*60).
-define(MAX_TOKEN_TTL, 24*60*60).

-endif.

