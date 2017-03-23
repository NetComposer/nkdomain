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
        pid => pid(),
        local => binary(),
        remote => binary()
    }.


%% ===================================================================
%% Public
%% ===================================================================

-spec create(nkservice:id(), create_opts()) ->
    {ok, SessId::nkdomain:obj_id(), pid()} | {error, term()}.

create(SrvId, Opts) ->
    case nkservice:get(SrvId, nkdomain_data) of
        #{domain_obj_id:=ParentId} ->
            Make = case Opts of
                #{session_id:=SessId} -> #{obj_id => SessId};
                _ -> #{}
            end,
            BaseList = [
                case Opts of
                    #{referred_id:=ReferredId} -> {referred_id, ReferredId};
                    _ -> []
                end,
                {?DOMAIN_SESSION,
                    maps:from_list(lists:flatten([
                        case Opts of
                            #{local:=Local} -> {local, Local};
                            _ -> []
                        end,
                        case Opts of
                            #{remote:=Remote} -> {remote, Remote};
                            _ -> []
                        end
                    ]))}
            ],
            Base = maps:from_list(lists:flatten(BaseList)),
            CreateList = [
                {update_pid, true},
                {remove_after_stop, true},
                case Opts of
                    #{pid:=Pid} -> {register, {?MODULE, Pid}};
                    _ -> []
                end
            ],
            Create = maps:from_list(lists:flatten(CreateList)),
            case nkdomain_obj_lib:make_obj(SrvId, ?DOMAIN_SESSION, ParentId, Base, Make) of
                {ok, Obj} ->
                    nkdomain_obj_lib:create(SrvId, Obj, Create);
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, missing_domain}
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
    #{
        local => #{type => keyword},
        remote => #{type => keyword}
    }.


%% @private
object_syntax(_Mode) ->
    #{
        local => binary,
        remote => binary
    }.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_session_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_session_obj_api:cmd(Sub, Cmd, Data, State).


