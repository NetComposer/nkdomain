%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

-ifndef(NKDOMAIN_API_DEBUG_HRL_).
-define(NKDOMAIN_API_DEBUG_HRL_, 1).

-include("nkdomain.hrl").

-define(API_DEBUG(Txt, Args),
    case erlang:get(nkdomain_debug) of
        true -> ?API_LLOG(debug, Txt, Args);
        _ -> ok
    end).

-define(API_LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN API " ++ Txt, Args)).


-define(API_DEBUG(Txt, Args, ApiReq),
    case erlang:get(nkdomain_debug) of
        true -> ?API_LLOG(debug, Txt, Args, ApiReq);
        _ -> ok
    end).


-define(API_LLOG(Type, Txt, Args, ApiReq),
    lager:Type(
        [
            {verb, maps:get(verb, ApiReq, get)},
            {group, maps:get(group, ApiReq, <<>>)},
            {resuource, maps:get(resource, ApiReq, <<>>)},
            {name, maps:get(name, ApiReq, <<>>)}
        ],
        "NkDOMAIN API (~s ~s/~s/~s) " ++ Txt,
        [
            maps:get(verb, ApiReq, get),
            maps:get(group, ApiReq, <<>>),
            maps:get(resource, ApiReq, <<>>),
            maps:get(name, ApiReq, <<>>) |
            Args
        ]
    )).

-endif.

