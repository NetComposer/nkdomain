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

%% @doc Config Object

-module(nkdomain_config_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/3, find_configs/3]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Config "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, Name, Obj) ->
    nkdomain_obj_lib:make_and_create(Srv, Name, Obj, #{}).


%% @doc
find_configs(Srv, SubType, Parent) ->
    case nkdomain_obj_lib:load(Srv, Parent, #{}) of
        #obj_id_ext{obj_id=ParentId} ->
            Search = #{
                filters => #{
                    type => ?DOMAIN_CONFIG,
                    parent_id => ParentId,
                    subtype => SubType
                },
                fields => [created_time, ?DOMAIN_CONFIG],
                sort => [#{created_time => #{order => desc}}]
            },
            case nkdomain_store:find(Srv, Search) of
                {ok, _N, Data, _Meta} ->
                    Data2 = lists:map(
                        fun(#{<<"obj_id">>:=ObjId, <<"created_time">>:=Time, ?DOMAIN_CONFIG:=Config}) ->
                            {ObjId, Time, Config}
                        end,
                        Data),
                    {ok, Data2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_CONFIG
    }.


%% @private
object_mapping() ->
    disabled.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    any.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Sub, Cmd, ?DOMAIN_CONFIG, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Req, State) ->
    nkdomain_obj_api:api(Sub, Cmd, Req, ?DOMAIN_CONFIG, State).




%% ===================================================================
%% Internal
%% ===================================================================




