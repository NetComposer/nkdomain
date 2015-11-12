%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Gonzalez Florido.  All Rights Reserved.
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


-module(nkdomain).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([load_file/1, export/1, get_aliases/1, resolve/1, multi_resolve/1]).
-export_type([user_obj_id/0, obj_id/0, obj/0, class/0]).
-export_type([token/0]).

-include_lib("nklib/include/nklib.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type user_obj_id() :: binary().        %% "group:one@domain"
-type obj_id() :: binary().             %% "one@domain"
-type obj() :: map().
-type class() :: domain | group | user | nodeset | service | alias | atom().

-type token() :: binary().


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Loads a domain configuration from a YAML or JSON file
-spec load_file(string()|binary()) ->
    {ok, #{obj_id() => nkdomain_load:load_result()}} | {error, term()}.

load_file(File) ->
    nkdomain_load:load_file(File, #{token=>admin}).


%% @doc Exports a full domain specification as a map
-spec export(obj_id()) ->
    {ok, map()} | {error, term()} .

export(DomainId) ->
    nkdomain_obj:export(domain, DomainId).


%% @doc Finds all pointing objects for an alias
-spec get_aliases(binary()) ->
    [user_obj_id()].

get_aliases(Alias) ->
    case nkdomain_obj:get_obj(alias, Alias) of
        {ok, #{aliases:=Aliases}} -> Aliases;
        {error, _} -> []
    end.


%% @doc Resolves any id or alias
-spec resolve(binary()|string()) ->
    {ok, {class(), obj_id(), pid()}} | {error, term()}.

resolve(UserObjId) ->
    UserObjId1 = nklib_util:to_binary(UserObjId),
    case get_aliases(UserObjId1) of
        [] ->
            nkdomain_util:resolve(UserObjId1);
        [UserObjId2] -> 
            nkdomain_util:resolve(UserObjId2);
        _ ->
            {error, multiple_aliases}
    end.


%% @doc Resolves any id or alias, allowing multiple alias
-spec multi_resolve(binary()|string()) ->
    [{class(), obj_id(), pid()}].

multi_resolve(UserObjId) ->
    UserObjId1 = nklib_util:to_binary(UserObjId),
    case get_aliases(UserObjId1) of
        [] ->
            case nkdomain_util:resolve(UserObjId1) of
                {ok, Data} -> [Data];
                {error, _} -> []
            end;
        UserObjIdList ->
            lists:foldl(
                fun(Id, Acc) ->
                    case nkdomain_util:resolve(Id) of
                        {ok, {Class, ObjId, Pid}} -> 
                            [{Class, ObjId, Pid}|Acc];
                        {error, _} -> 
                            Acc
                    end
                end,
                [],
                UserObjIdList)
    end.


% %% @doc Register a new service class with a callback module
% %% The callback module must implement the following callbacks:
% %%
% %% -callback get_syntax() ->
% %%     map().
% %%
% %% -callback updated(id(), nkdomain_obj_service:service()) ->
% %%     ok.
% %%
% %% -callback removed(obj_id()) ->
% %%     ok.
% %%
% -spec register_service(Class::atom(), module()) ->
%     ok.

% register_service(ServiceClass, Module) when is_atom(ServiceClass) ->
%     {module, Module} = code:ensure_loaded(Module),
%     nklib_config:put(?MODULE, {srv, ServiceClass}, Module).


% %% @doc Gets a service class callback module
% -spec get_service_module(class()) ->
%     module().

% get_service_module(Class) ->
%     case nklib_config:get(?MODULE, {srv, Class}, not_found) of
%         not_found -> 
%             error({service_class_not_found, Class});
%         Module -> 
%             Module
%     end.

