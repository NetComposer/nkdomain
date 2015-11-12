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

-module(resolve_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(nkdomain, [resolve/1, multi_resolve/1]).

resolve_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkdomain_app:start(),
            nkdomain:register_service(admin, test_srv_admin),
            nkdomain:register_service(dns, test_srv_dns)
        end,
		fun(_) -> 
			ok 
		end,
	    fun(_) ->
		    [
				fun() -> basic() end,
                fun() -> error() end
			]
		end
  	}.



basic() ->
    YamlData1 = data1:yaml(),
    {ok, _} = nkdomain_load:load(yaml, YamlData1, #{replace=>true}),

    {ok, {domain, <<"root">>, _}} = resolve("root"),
    {ok, {user, <<"root1@root">>, _}} = {ok, U1} = resolve("user:root1"),
    {ok, U1} = resolve("user:root1@root"),
    {ok, U1} = resolve("user_root1@domain.com"),
    {ok, {user, <<"root2@root">>, _}} = {ok, U2} = 
        resolve("user:root2@root"),
    {ok, U2} = resolve("user_root2@domain.com"),
    {error, multiple_aliases} = resolve("shared@shared.com"),
    [U2, U1] = multi_resolve("shared@shared.com"),
    {error, not_found} = resolve("group1@root"),
    [] = multi_resolve("group2@root"),
    {ok, {nodeset, <<"group1@root">>, _}} = resolve("nodeset:group1@root"),
    {ok, {service, <<"dns@root">>, _}} = resolve("service:dns"),
    {ok, {group, <<"people@root">>, _}} = resolve("group:people"),
    {ok, {group, <<"admins.people@root">>, _}} = resolve("group:admins.people"),
    {ok, {group, <<"b1.b.zones@root">>, _}} = resolve("group:b1.b.zones@root"),

    {ok, {domain, <<"domainA">>, _}} = resolve("domainA"),
    {ok, {user, <<"user1@domainA">>, _}} = {ok, U3} = 
        resolve("user:user1@domainA"),
    {ok, {nodeset, <<"group1@domainA">>, _}} = 
        resolve("nodeset:group1@domainA"),
    {ok, {service, <<"admin@domainA">>, _}} = 
        resolve("service:admin@domainA"),

    {ok, {domain, <<"proy1.domainA">>, _}} = resolve("proy1.domainA"),
    {ok, {domain, <<"proy1.domainA">>, _}} = resolve("nekso.net"),
    {ok, {user, <<"user1@proy1.domainA">>, _}} = {ok, U4} = 
        resolve("user:user1@proy1.domainA"),
    {ok, U4} = resolve("user:user1@nekso.net"),
    true = element(3, U3) /= element(3, U4),
    true = element(3, U1) /= element(3, U4),
    {ok, {service, <<"dns@proy1.domainA">>, _}} = {ok, S1} = 
        resolve("service:dns@proy1.domainA"),
    {ok, S1} = resolve("service:dns@nekso.net"), 
    ok.


error() ->
    {error, not_found} = resolve("root2"),
    {error, not_found} = resolve("user:user4"),
    {error, not_found} = resolve("alias.com"),
    {error, not_found} = resolve("user@alias.com"),
    {error, invalid_class} = resolve("user2:user4"),
    {error, invalid_obj_id} = resolve("user::user4"),
    {error, invalid_class} = resolve("user2:user4@root"),
    {error, invalid_obj_id} = resolve("user::user4@root"),
    ok.


