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

%% @doc NkDomain service callback module
-module(nkdomain_push).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([send_push/3]).

-include("nkdomain.hrl").

-type msg() ::
    #{
        device_id => binary(),
        text => binary()
    }.


%% ===================================================================
%% Public functions
%% ===================================================================


%% @private
-spec send_push(nkservice:id(), binary(), msg()) ->
    ok | {error, term()}.

send_push(SrvId, ProvId, _Msg) ->
    #{nkdomain:=#{push_providers:=Providers}} = SrvId:config(),
    case find_provider(nklib_util:to_binary(ProvId), Providers) of
        {ok, #{url:=Url}} ->
            case httpc:request(get, {nklib_util:to_list(Url), []}, [], []) of
                {ok, {{_, 200, _}, _Hs, _B}} ->
                    ok
            end;
        not_found ->
            {error, provider_not_found}
    end.



find_provider(_Class, []) -> not_found;
find_provider(Class, [#{id:=Class}=Prov|_]) -> {ok, Prov};
find_provider(Class, [_Prov|Rest]) -> find_provider(Class, Rest).