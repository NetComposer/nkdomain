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

-ifndef(NKDOMAIN_DEBUG_HRL_).
-define(NKDOMAIN_DEBUG_HRL_, 1).

-define(DEBUG(Txt, Args, State),
    case erlang:get(object_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, Session),
    lager:Type(
        [
            {obj_id, Session#obj_session.obj_id},
            {type, Session#obj_session.type},
            {path, Session#obj_session.path}
        ],
        "NkDOMAIN Obj ~s ~s (~s) " ++ Txt,
        [
            Session#obj_session.type,
            Session#obj_session.path,
            Session#obj_session.obj_id | Args
        ]
    )).

-endif.
