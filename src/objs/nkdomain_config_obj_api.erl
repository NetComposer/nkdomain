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

%% @doc User Object API
-module(nkdomain_config_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc
cmd('', create, #nkapi_req{data=Data}, #{srv_id:=SrvId}=State) ->
    #{subtype:=SubType, parent:=Parent, ?DOMAIN_CONFIG_ATOM:=Config} = Data,
    Name = maps:get(obj_name, Data, <<>>),
    case nkdomain_config_obj:create(SrvId, SubType, Parent, Name, Config) of
        {ok, ObjId, Path, _Pid} ->
            {ok, #{obj_id=>ObjId, path=>Path}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', find, #nkapi_req{data=Data}, #{srv_id:=SrvId}=State) ->
    #{subtype:=SubType, parent:=Parent} = Data,
    case nkdomain_config_obj:find_configs(SrvId, SubType, Parent) of
        {ok, List} ->
            Data2 = lists:map(
                fun({ObjId, Time, Config}) ->
                    #{obj_id=>ObjId, created_time=>Time, config=>Config}
                end,
                List),
            {ok, Data2, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(Sub, Cmd, Req, State) ->
    nkdomain_obj_api:api(Sub, Cmd, Req, ?DOMAIN_CONFIG, State).
