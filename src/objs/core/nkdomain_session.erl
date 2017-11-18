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

%% @doc Session Object

-module(nkdomain_session).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/4]).

-include("nkdomain.hrl").
%%-include("nkdomain_debug.hrl").
%%-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_link => {module(), pid()},
        session_id => binary(),
        data => #{
            login_meta => map(),
            local => binary(),
            remote => binary()
        }
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Creates a new session
-spec start(nkservice:id(), nkdomain:id(), nkdomain:id(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(SrvId, DomainId, UserId, Opts) ->
        Obj1 = #{
            type => ?DOMAIN_SESSION,
            domain_id => DomainId,
            parent_id => UserId,
            srv_id => SrvId,
            created_by => UserId,
            active => true,
            ?DOMAIN_SESSION => maps:get(data, Opts, #{})
        },
        Obj2 = case Opts of
            #{session_id:=SessId} ->
                Obj1#{obj_id => SessId};
            _ ->
                Obj1
        end,
        CreateOpts = maps:with([session_link], Opts),
        case nkdomain_obj_make:create(Obj2, CreateOpts) of
            {ok, #obj_id_ext{obj_id=SessId2, pid=Pid}, _} ->
                {ok, SessId2, Pid};
            {error, Error} ->
                {error, Error}
        end.
