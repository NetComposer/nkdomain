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

%% @doc NkDomain OpenAPI support
%% https://swagger.io/docs/specification/about/

-module(nkdomain_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([make_spec/1, get_modules/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-callback paths(nkservice:id()) -> map().

-callback schemas(nkservice:id()) -> map().

-callback parameters(nkservice:id()) -> map().

-callback responses(nkservice:id()) -> map().

-callback callbacks(nkservice:id()) -> map().

-optional_callbacks([parameters/1, responses/1, callbacks/1]).




%% ===================================================================
%% Internal
%% ===================================================================


%% @doc Generates OpenAPI specification
make_spec(SrvId) ->
    Base = nkdomain_openapi_core:base(SrvId),
    Modules = get_modules(SrvId),
    Spec1 = Base#{
        paths => get_callback_values(SrvId, paths, Modules),
        components => #{
            schemas => get_callback_components(SrvId, schemas, Modules),
            parameters => get_callback_components(SrvId, parameters, Modules),
            responses => get_callback_components(SrvId, responses, Modules)
        }
    },
    Spec2 = nkdomain_openapi_util:unident_descriptions(Spec1),
    Json = nklib_json:encode_sorted(Spec2),
    LogPath = filename:join(nkservice_app:get(logPath), "openapi.json"),
    ok = file:write_file(LogPath, Json),
    {ok, Json}.



%% @private
get_modules(SrvId) ->
    Modules = lists:foldl(
        fun(Module, Acc) ->
            Mod2 = <<(atom_to_binary(Module, utf8))/binary, "_openapi">>,
            Mod3 = binary_to_atom(Mod2, utf8),
            case code:ensure_loaded(Mod3) of
                {module, _} -> [Mod3 | Acc];
                {error, _} -> Acc
            end
        end,
        [],
        nkdomain_plugin:get_modules(SrvId)),
    [nkdomain_openapi_core|Modules].


%% @private
get_callback_values(SrvId, Fun, Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case erlang:function_exported(Module, Fun, 1) of
                true ->
                    Spec = Module:Fun(SrvId),
                    Map = case is_list(Spec) of
                        true ->
                            lists:foldl(
                                fun(M, Acc2) -> maps:merge(Acc2, M) end,
                                #{},
                                Spec);
                        false ->
                            Spec
                    end,
                    maps:merge(Acc, Map);
                false ->
                    Acc
            end
        end,
        #{},
        Modules).


%% @private Add "io.netc.api." to all keys
get_callback_components(SrvId, Fun, Modules) ->
    Map = get_callback_values(SrvId, Fun, Modules),
    List = lists:map(
        fun({Key, Val}) ->
            case nklib_util:to_binary(Key) of
                <<"io.netc.api.", _/binary>> ->
                    {Key, Val};
                Base ->
                    {<<"io.netc.api.", Base/binary>>, Val}
            end
        end,
        maps:to_list(Map)),
    maps:from_list(List).

