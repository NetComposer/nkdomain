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

-export([create/5, find_configs/3]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
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
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:subtype(), nkdomain:id(), nkdomain:name(), map()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, SubType, Parent, Name, Config) ->
    Opts = #{
        type_obj => Config,
        subtype => SubType,
        name => Name
    },
    nkdomain_obj_lib:make_and_create(Srv, Parent, ?DOMAIN_CONFIG, Opts).



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
object_syntax(_) ->
    any.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_config_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_config_obj_api:cmd(Sub, Cmd, Data, State).




%% ===================================================================
%% Internal
%% ===================================================================




