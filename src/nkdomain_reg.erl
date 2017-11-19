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

%% @doc Type registrations
-module(nkdomain_reg).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_type_module/1, get_all_type_modules/0, get_module_type/1, get_all_types/0]).
-export([get_schema_type_module/1, get_all_schema_types/0]).
-export([register_modules/1]).

-include("nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds a type's module
-spec get_type_module(nkdomain:type()) ->
    module() | undefined.

get_type_module(Type) ->
    nklib_types:get_module(nkdomain_type, Type).


%% @doc Finds a type's module
-spec get_schema_type_module(nkdomain:type()) ->
    module() | undefined.

get_schema_type_module(Type) ->
    nklib_types:get_module(nkdomain_schema_type, Type).


%% @doc Gets all registered modules
-spec get_all_type_modules() ->
    [module()].

get_all_type_modules() ->
    nklib_types:get_all_modules(nkdomain_type).


%% @doc Finds a module's type
-spec get_module_type(module()) ->
    nkdomain_type:type() | undefined.

get_module_type(Module) ->
    nklib_types:get_type(nkdomain_type, Module).


%% @doc Gets all registered types
-spec get_all_types() ->
    [nkdomain_type:type()].

get_all_types() ->
    nklib_types:get_all_types(nkdomain_type).


%% @doc Gets all registered types
-spec get_all_schema_types() ->
    [nkdomain_graphql:type()].

get_all_schema_types() ->
    nklib_types:get_all_types(nkdomain_schema_type).


%% @doc Gets the obj modules and reload schema
-spec register_modules([module()]) ->
    ok.

%% @doc Registers a module
register_modules([]) ->
    case nkdomain_app:get(load_schema) of
        true ->
            nkdomain_graphql:load_schema();
        false ->
            ok
    end;

register_modules([Module|Rest]) ->
    #{type:=Type} = Info = Module:object_info(),
    Type2 = to_bin(Type),
    _ = binary_to_atom(Type2, utf8),
    % Ensure we have the corresponding atoms loaded
    % We store the bin version of the service
    nklib_types:register_type(nkdomain_type, Type2, Module),
    case Info of
        #{schema_type:=SchemaType} ->
            SchemaType2 = to_bin(SchemaType),
            _ = binary_to_atom(SchemaType2, utf8),
            nklib_types:register_type(nkdomain_schema_type, SchemaType2, Module);
        _ ->
            ok
    end,
    register_modules(Rest).


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
