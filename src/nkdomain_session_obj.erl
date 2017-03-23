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

%% @doc User Session Object

-module(nkdomain_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkdomain.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_id => binary(),
        referred_id => binary(),
        parent_id => binary(),
        register => pid(),
        local => binary(),
        remote => binary()
    }.


%% ===================================================================
%% Public
%% ===================================================================

create(SrvId, Opts) ->
    ParentId = maps:get(parent_id, Opts, <<"root">>),
    {SessId, Opts2} = nklib_util:add_id(Opts, session),
    case nkdomain_obj:load(SrvId, ParentId) of
        {ok, _ParentType, ParentId, ParentPath, _ParentPid} ->
            Session1 = #{
                type => ?DOMAIN_SESSION,
                path => list_to_binary([ParentPath, $/, ?DOMAIN_SESSION, "s/", SessId]),
                parent_id => ParentId
            },
            Session2 = case Opts of
                #{referred_id:=ReferredId} ->
                    Session1#{referred_id => ReferredId};
                _ ->
                    Session1
            end,
            Meta = #{
                obj_id => SessId,
                register => {?MODULE, self()},
                update_pid => true,
                remove_after_stop => true
            },
            case nkdomain_obj:create(SrvId, Session2, Meta) of
                {ok, SessId, _SessPid} ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_SESSION
    }.


%% @private
object_mapping() ->
    #{}.


%% @private
object_syntax(_Mode) ->
    #{}.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_session_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_session_obj_api:cmd(Sub, Cmd, Data, State).


