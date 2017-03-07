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

%% @doc User Object

-module(nkdomain_user).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_get_desc/0, object_get_mapping/0, object_get_syntax/0,
         object_store/1]).


-include("nkdomain.hrl").


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


object_get_desc() ->
    #{
        name => <<"user">>
    }.


object_get_mapping() ->
    #{
        name => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        surname => #{
            type => text,
            fields => #{keyword => #{type=>keyword}}
        },
        password => #{type => keyword}
    }.


object_get_syntax() ->
    #{
        name => binary,
        surname => binary,
        password => binary
    }.


object_store(#{?MODULE:=User}) ->
    Keys = maps:keys(object_get_mapping()),
    maps:with(Keys, User);

object_store(_) ->
    #{}.

