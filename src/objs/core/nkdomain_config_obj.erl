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

-export([create/3, find_configs/2]).
-export([object_info/0, object_es_mapping/0, object_parse/2,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_admin_info/0, object_schema_types/0]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Config "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

%% ===================================================================
%% API
%% ===================================================================

% Use domain_id, created_by, subtype
create(SubType, Opts, Data) ->
    Obj = Opts#{
        type => ?DOMAIN_CONFIG,
        domain_id => maps:get(domain_id, Opts, root),
        created_by => maps:get(created_by, Opts, admin),
        subtype => SubType,
        ?DOMAIN_CONFIG => Data
    },
    case nkdomain_obj_make:create(Obj) of
        {ok, #obj_id_ext{obj_id=ObjId}, _} ->
            {ok, ObjId};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_configs(Path, SubType) ->
    Filters = #{type=>?DOMAIN_CONFIG, subtype=>SubType},
    nkdomain_domain_obj:search_all_childs(Path, #{filters=>Filters}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?DOMAIN_CONFIG,
        schema_type => 'Config',
        default_ttl => 5*60*1000
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 900
    }.


%% @doc
object_schema_types() ->
    #{
        'Config' => #{
            fields => #{
            },
            is_object => true,
            comment => "A Config Object"
        }
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        data => #{enabled => false}
    }.


%% @private
object_parse(_Mode, _Obj) ->
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
    nkdomain_obj_cmd:cmd(Cmd, ?DOMAIN_CONFIG, Req).




%% ===================================================================
%% Internal
%% ===================================================================




