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

-define(NKROOT, nkroot).
-define(ADD_TO_OBJ(Key, Val, Obj), maps:put(Key, Val, Obj)).
-define(ADD_TO_OBJ(Update, Obj), maps:merge(Obj, Update)).
-define(ADD_TO_OBJ_DEEP(Update, Obj), nklib_util:map_merge(Update, Obj)).
-define(REMOVE_FROM_OBJ(Key, Obj), maps:remove(Key, Obj)).

-define(CALL_NKROOT(Fun, Args), apply(?NKROOT, Fun, Args)).

-define(STATE, nkstate_v1).

-record(obj_id_ext, {
    srv_id :: nkservice:id(),
    type :: nkdomain:type(),
    obj_id :: nkdomain:obj_id(),
    obj_name :: nkdomain:obj_name(),
    path :: nkdomain:path(),
    pid :: pid() | undefined
}).


-record(nkstate_v1, {
    srv_id :: nkservice:id(),
    id :: #obj_id_ext{},
    module :: module(),
    domain_id :: nkdomain:obj_id(),
    parent_id :: nkdomain:obj_id(),
    object_info :: nkdomain_obj:object_info(),
    obj :: nkdomain:obj(),
    is_dirty :: boolean(),
    is_enabled :: boolean(),
    started :: nklib_util:m_timestamp(),
    childs :: #{nkdomain:obj_id() => {nkdomain:type(), pid()}},
    usage_links :: nklib_links:link(),
    event_links :: nklib_links:link(),
    status :: nkdomain_obj:status(),
    session_events :: [binary()],
    session_link :: nklib_links:link(),
    meta :: map(),                      % Object load metadata
    session :: term(),                  % Session-specific metadata
    stop_reason = false :: false | nkservice:error(),
    unload_policy :: permanent | {expires, nklib_util:m_timestamp()} | {ttl, integer()},
    timer :: reference(),
    timelog = [] :: [map()],
    domain_pid :: pid(),
    domain_enabled :: boolean(),
    parent_pid :: pid(),
    parent_enabled :: boolean()
}).


-record(nkdomain_config_cache, {
    db_store :: binary(),
    file_store :: binary,
    email_provider :: binary()
}).



-define(DOMAIN_USER, <<"user">>).
-define(DOMAIN_DOMAIN, <<"domain">>).
-define(DOMAIN_SESSION, <<"session">>).
-define(DOMAIN_TOKEN, <<"token">>).
-define(DOMAIN_CONFIG, <<"config">>).
-define(DOMAIN_SERVICE, <<"service">>).
-define(DOMAIN_TASK, <<"task">>).
-define(DOMAIN_ALERT, <<"alert">>).
-define(DOMAIN_DEVICE, <<"device">>).




-define(DOMAIN_MAIL, <<"mail">>).
-define(DOMAIN_MAIL_PROVIDER, <<"mail.provider">>).
-define(DOMAIN_FILE, <<"file">>).
-define(DOMAIN_FILE_STORE, <<"file.store">>).

-define(DOMAIN_EVENT_CLASS, <<"domain">>).
-define(DEF_TOKEN_TTL, 8*60*60).
-define(MAX_TOKEN_TTL, 30*24*60*60).

-endif.

