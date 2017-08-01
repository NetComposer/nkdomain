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

-module(nkdomain_user_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/2]).

-include("nkdomain.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

-define(ID, <<"domain_detail_user">>).


%% @doc
view(#obj_id_ext{obj_id=ObjId, pid=Pid}, Session) ->
    case nkdomain:get_obj(any, Pid) of
        {ok, Obj} ->
            Data = #{
                id => <<?ID/binary, "__", ObjId/binary>>,
                class => webix_ui,
                value => Obj
            },
            {Data, Session};
        {error, Error} ->
            {error, Error}
    end.
