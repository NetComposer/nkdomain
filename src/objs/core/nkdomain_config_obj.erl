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

-export([find_configs/3]).
-export([object_info/0, object_es_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_admin_info/0]).

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
find_configs(Srv, SubType, Parent) ->
    case nkdomain_lib:load(Srv, Parent) of
        #obj_id_ext{obj_id=DomainId} ->
            Search = #{
                filters => #{
                    type => ?DOMAIN_CONFIG,
                    domain_id => DomainId,
                    subtype => SubType
                },
                fields => [created_time, ?DOMAIN_CONFIG],
                sort => [#{created_time => #{order => desc}}]
            },
            case nkdomain:search(Srv, Search) of
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
object_info() ->
    #{
        type => ?DOMAIN_CONFIG,
        default_ttl => 5*60*1000
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 900
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        data => #{enabled => false}
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        vsn => binary,
        data => any,
        '__defaults' => #{vsn => <<"1">>}
    }.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_CONFIG, Syntax).




%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_CONFIG, Req).




%% ===================================================================
%% Internal
%% ===================================================================




