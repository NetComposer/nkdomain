%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain
%%
%% - Expects domain 'nkdomain-root' (?ROOT_SRV) as the root for all domains
%% - Maps domains to services named after 'domain.nkdomain-root'
%% - Defines actor's group "core" (?CORE_GROUP) for all of our actors
%% - Defines an external API Url and translates to internal path:
%%   - /domains/core/v1/ -> /nkdomain-root/nkdomain-core/domain/root
%%   - /domains/core/v1/a.b.c -> /a.b.c.nkdomain-root/nkdomain-core/domain/a.b.c
%%   - /domains/core/v1/a.b.c/users/user1 -> /a.b.c.nkdomain-root/nkdomain-core/user/user1
%%
%% - The application starts a single service with a single instance of the Domains
%%
%% Actor activation
%% ----------------
%%
%% - Callback nkdomain:actor_activate is called, and will activate the
%%   parent domain first
%%
%%
%% Actor creation:
%% ---------------
%%
%% - The API is called directly (nkdomain_api:api/1,2) or
%%   throw http (nkdomain_api:rest_api/4)
%% - In any case, callback nkdomain_api is called.
%%   For 'core' group, nkdomain_core_v1:request/2 is called
%% - After finding resource and module, and parsed parameters, nkservice_actor:request/3
%%   is called. If not implemented, nkdomain_core:create/5 is called
%% - An Actor is extracted from the API's body (an nkdomain_api_actor can update it)
%% - The type-specific parser is checked, calling nkdomain_actor:parse/3
%% - nkservice_actor_db:create/3 is called (maybe activating the object)
%% - It will add missing fields and parse again with standard fields
%% - The it will either load (and save) the object or create it directly
%%
%%
%% Actor read
%% ----------
%%
%% - The API is called as above
%% - If not implemented in in resource's module, nkdomain_core:get/5 is called
%%
%%
%%
%%
%%




-module(nkdomain).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([delete_old_events/3, get_actor/1]).
-export([load_actors/1]).

-export_type([domain/0, verb/0, actor_config/0]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type verb() :: atom().     %% See nkdomain_core_v1

-type domain() :: binary().

-type actor_config() ::
    nkservice_actor:config() |
    #{
        group => nkservice_actor:group(),
        resource => nkservice_actor:resource(),
        versions => [nkservice_actor:vsn()],
        auto_activate => boolean(),                     %% Periodic automatic activation
        camel => binary(),
        singular => binary(),
        short_names => [binary()],
        verbs => [atom()],
        filter_fields => [nkservice_actor_search:field_name()],
        sort_fields => [nkservice_actor_search:field_name()],
        field_type => #{
            nkservice_actor_search:field_name() => nkservice_actor_search:field_type()
        },
        immutable_fields => [nkservice_actor_search:field_name()],
        module => module()      % Calculated
    }.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Deletes events older that RFC3339, for this domain and subdomains
delete_old_events(SrvId, Domain, Date) ->
    Opts = #{deep=>true},
    nkservice_actor:delete_old(SrvId, Domain, ?GROUP_CORE, ?RES_CORE_EVENTS, Date, Opts).


%% @doc
get_actor(Path) ->
    case nkservice_actor_util:is_actor_id(Path) of
        {true, #actor_id{domain=Domain}=ActorId} ->
            case nkdomain_register:get_domain_data(Domain) of
                {ok, DomSrvId, _DomUID} ->
                    nkservice_actor:get_actor({DomSrvId, ActorId});
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            {error, not_a_path}
    end.



%% ===================================================================
%% LOAD utility
%% ===================================================================

%% @doc
load_actors(Path) ->
    Files = filelib:fold_files(Path, "\\.yaml$", false, fun(F, Acc) -> [F|Acc] end, []),
    load_actor_files(Files).


%% @private
load_actor_files([]) ->
    ok;

load_actor_files([File|Rest]) ->
    {ok, Data} = file:read_file(File),
    case catch nklib_yaml:decode(Data) of
        {error, {yaml_decode_error, Error}} ->
            lager:warning("Error processing file '~s': ~p", [File, Error]),
            {error, {yaml_decode_error, Error}};
        Objs ->
            lager:warning("Loading actors in file '~s'", [File]),
            case load_actor_objs(Objs) of
                ok ->
                    load_actor_files(Rest);
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private
load_actor_objs([]) ->
    ok;

load_actor_objs([Obj|Rest]) ->
    case Obj of
        #{<<"metadata">> := #{<<"name">>:=_}} ->
            Op = #{
                verb => update,
                body => Obj
            },
            case nkdomain_api:request(?ROOT_SRV, Op) of
                {error, Error, _} ->
                    {error, Error};
                {created, Obj2, _} ->
                    #{<<"metadata">>:=#{<<"uid">>:=UID, <<"selfLink">>:=Self}}= Obj2,
                    lager:warning("Actor ~s (~s) create", [Self, UID]),
                    load_actor_objs(Rest);
                {ok, Obj2, _} ->
                    #{<<"metadata">>:=#{<<"uid">>:=UID, <<"selfLink">>:=Self}}= Obj2,
                    lager:warning("Actor ~s (~s) update", [Self, UID]),
                    load_actor_objs(Rest)
            end;
        _ ->
            % We don't want automatic generation of names in bulk loading
            {error, {field_missing, <<"metadata.name">>}}
    end.


