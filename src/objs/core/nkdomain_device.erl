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

%% @doc Device Object

-module(nkdomain_device).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-export([create/2, attach_session/3, get_registered_user/1, find_device_uuid/2, find_sso/1]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Device "++Txt, Args)).



%% ===================================================================
%% Types
%% ===================================================================



-type create_opts() ::
#{
    obj_id => binary(),
    created_by => nkdomain:obj_id(),
    device_uuid => binary(),
    sso_device_ids => [binary()]
}.



%% ===================================================================
%% Public
%% ===================================================================

-spec create(nkdomain:id(), create_opts()) ->
    {ok, DeviceIdId::nkdomain:obj_id(), pid()} | {error, term()}.

create(Domain, Opts) ->
    DeviceObj = maps:with([device_id, sso_device_ids], Opts),
    Obj = #{
        type => ?DOMAIN_DEVICE,
        domain_id => Domain,
        created_by => maps:get(created_by, Opts, <<"admin">>),
        ?DOMAIN_DEVICE => DeviceObj
    },
    case nkdomain_obj_make:create(Obj) of
        {ok, #obj_id_ext{obj_id=DeviceId, pid=MsgPid}, []} ->
            {ok, DeviceId, MsgPid};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Attaches an user and session to the device
%% The device will be loaded as long as the session is
%% You can use get_users/1 to get all registered users
%% You can use find_sso/1 to find if any of my sso_device_ids have any registered user currently.
-spec attach_session(nkdomain:id(), nkdomain:id(), nkdomain:obj_id()) ->
    ok | {error, existing_session|term()}.

attach_session(DeviceId, User, SessId) ->
    case nkdomain_db:load(SessId) of
        #obj_id_ext{pid=Pid} ->
            case nkdomain_db:find(User) of
                #obj_id_ext{obj_id=UserId, type=?DOMAIN_USER} ->
                    sync_op(DeviceId, {attach_session, UserId, SessId, Pid});
                {error, object_not_found} ->
                    {error, user_not_found};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, invalid_session};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_device_uuid(Domain, DeviceUUID) ->
    case nkdomain_db:search(?DOMAIN_DEVICE, {find_device_uuid, Domain, DeviceUUID}) of
        {ok, _, Data} ->
            Data2 = [ObjId || #{<<"obj_id">>:=ObjId} <- Data],
            {ok, Data2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec find_sso(nkdomain:id()) ->
    {ok, UserId::nkdomain:obj_id()} | {error, no_session|term()}.

find_sso(DeviceId) ->
    sync_op(DeviceId, find_sso).


%% @private
-spec get_registered_user(nkdomain:id()) ->
    {ok, UserId::nkdomain:obj_id()} | {error, no_session|term()}.

get_registered_user(DeviceId) ->
    sync_op(DeviceId, get_registered_user).


%% @private
sync_op(Id, Op) ->
    nkdomain_obj:sync_op(Id, {nkdomain_device_obj, Op}).
