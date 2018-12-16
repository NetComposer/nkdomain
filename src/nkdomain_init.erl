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

%% @doc NkDomain initialization
-module(nkdomain_init).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([check_database/2, check_admin/3]).

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Plugin: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").
-include_lib("nkpacket/include/nkpacket.hrl").



%% @private
check_database(SrvId, Domain) ->
    case check_base_domain(SrvId, Domain, 10) of
        {ok, DomUID} ->
            check_admin(SrvId, Domain, DomUID);
        {error, Error} ->
            {error, Error}
    end.


%% @private
check_base_domain(SrvId, Domain, Tries) when Tries > 0->
    ActorId = #actor_id{
        domain = Domain,
        group = ?GROUP_CORE,
        vsn = ?GROUP_CORE_V1A1,
        resource = ?RES_CORE_DOMAINS,
        name = ?ROOT_DOMAIN
    },
    try
        % It will read and activate the service also
        case nkservice_actor_db:read({SrvId, ActorId}, #{}) of
            {ok, #actor{id=#actor_id{uid=DomUID}}, _Meta} ->
                {ok, DomUID};
            {error, actor_not_found} ->
                case create_base_domain(SrvId, ActorId) of
                    {ok, DomUID} ->
                        {ok, DomUID};
                    {error, Error} ->
                        {error, Error}
                end;
            {error, Error} ->
                error(Error)
        end
    catch
        Class:CError:Trace ->
            ?LLOG(notice, "couldn't find service '~s' in database (~p tries left)"
                  "\n(~p:~p:~p)", [SrvId, Tries, Class, CError, Trace]),
            timer:sleep(1000),
            check_base_domain(SrvId, Domain, Tries-1)
    end;

check_base_domain(_SrvId, _Domain, _Tries) ->
    {error, database_not_available}.


%% @private
create_base_domain(SrvId, ActorId) ->
    #actor_id{domain=Domain, name=Name} = ActorId,
    Actor = #actor{
        id = ActorId,
        data = #{
            <<"kind">> => ?KIND_CORE_DOMAIN,
            <<"spec">> => #{}
        },
        metadata = #{
            <<"name">> => Name,
            <<"domain">> => Domain
        }
    },
    case create_actor(SrvId, Actor) of
        {ok, #actor{id=#actor_id{uid=DomainUID}}, _Meta} ->
            ?LLOG(warning, "created domain '~s' in database", [Domain]),
            {ok, DomainUID};
        {error, Error} ->
            ?LLOG(error, "could not create domain '~s' in database: ~p", [Domain, Error]),
            {error, Error}
    end.


%% @private
check_admin(SrvId, Domain, DomUID) ->
    ActorId = #actor_id{
        domain = Domain,
        group = ?GROUP_CORE,
        vsn = ?GROUP_CORE_V1A1,
        resource = ?RES_CORE_USERS,
        name = <<"admin">>
    },
    case nkservice_actor_db:read({SrvId, ActorId}, #{}) of
        {ok, _Actor, _Meta} ->
            ok;
        {error, actor_not_found} ->
            case create_admin(SrvId, ActorId, DomUID) of
                ok ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            error(Error)
    end.


%% @private
create_admin(SrvId, ActorId, DomUID) ->
    #actor_id{domain=Domain} = ActorId,
    Config = nkdomain_plugin:get_config(SrvId),
    Pass1 = maps:get(adminPass, Config, <<>>),
    Pass2 = nkdomain_user_actor:store_pass(SrvId, Pass1),
    DomainLink = nkdomain_actor_util:link_key2(?GROUP_CORE, ?LINK_CORE_DOMAIN),
    Actor = #actor{
        id = ActorId,
        data = #{
            <<"kind">> => <<"User">>,
            <<"spec">> => #{
                <<"password">> => Pass2
            }
        },
        metadata = #{
            <<"name">> => <<"admin">>,
            <<"domain">> => Domain,
            <<"links">> => #{DomainLink => DomUID}
        }
    },
    case create_actor(SrvId, Actor) of
        {ok, _Actor, _Meta} ->
            ?LLOG(warning, "created domain '~s' admin user in database", [Domain]),
            ok;
        {error, Error} ->
            ?LLOG(error, "could not create service '~s' admin user in database: ~p", [Domain, Error]),
            {error, Error}
    end.



%% @private
%% We cannot call add_common_fields/1 yet
create_actor(SrvId, Actor) ->
    #actor{id=#actor_id{group=Group, resource=Res}} = Actor,
    {ok, Config} = nkdomain_actor_util:get_config(SrvId, Group, Res),
    Actor2 = nkservice_actor_util:put_create_fields(Actor),
    #actor{id=#actor_id{uid=UID}, metadata=Meta} = Actor2,
    Meta2 = Meta#{<<"uid">> => UID},
    Actor3 = Actor2#actor{metadata=Meta2},
    nkservice_actor_db:create(SrvId, Actor3, #{actor_config=>Config}).
