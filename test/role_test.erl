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

-module(role_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkdomain_role, [get_roles/1, get_role_objs/2, find_role_objs/2, has_role/3]).
-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

role_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkdomain_app:start()
		end,
		fun(_) -> 
			ok 
		end,
	    fun(_) ->
		    [
		    	fun() -> basic() end,
		    	fun() -> load1() end,
		    	fun() -> load2() end
			]
		end
  	}.


basic() ->
    {ok, _} = nkdomain_load:load(yaml, data1:yaml(), #{replace=>true}),
    {ok, [owner, <<"admin">>, <<"role1">>]} = get_roles(root),
    {ok, [owner]} = get_roles("user:admin@root"),
    {ok, [owner, <<"admin">>]} = get_roles("user:root1@root"),
    {ok, [owner, <<"admin">>]} = get_roles("user:root2@root"),
	{ok, [owner, <<"user">>]} = get_roles("nodeset:group1@root"),
	{ok, [owner]} = get_roles("service:admin@root"),
	{ok, [owner, <<"user">>]} = get_roles("service:test_srv_dns@root"),

	{ok, []} = get_role_objs("member", root),
	{ok, [#{<<"member">> := <<"group:admins.people@root">>}, 
			<<"user:admin@root">>, <<"user:root2@root">>]} = 
		get_role_objs("admin", root),

	% User admin is duplicated
	{ok, [<<"user:root2@root">>, <<"user:admin@root">>, 
		  <<"user:admin@root">>, <<"user:admin@domainA">>]} = 
		find_role_objs("admin", root),

	{ok, true} = has_role("user:root2@root", "admin", root),
	{ok, false} = has_role("user:root1@root", "admin", root),
	{ok, true} = has_role("user:admin@root", "admin", root),
	{ok, true} = has_role("user:admin@domainA", "admin", root),

	{ok, true} = has_role("user_root2@domain.com", "admin", root),
	{ok, false} = has_role("user_root1@domain.com", "admin", root),
	{ok, false} = has_role("user_root1@none", "admin", root),

	{ok, true} = has_role("user:root1@root", "admin", "user:root2@root"),

	{ok, [<<"domain:domainA">>]} = get_role_objs("user", "nodeset:group1@root"),
	{ok, true} = has_role("domain:domainA", "user", "nodeset:group1@root"),
	{ok, true} = has_role("domain_a.com", "user", "nodeset:group1@root"),
	{ok, false} = has_role("nekso.net", "user", "nodeset:group1@root"),
	{ok, [<<"user:admin@root">>, <<"user:admin@domainA">>]} = 
			find_role_objs("user", "nodeset:group2@root"),
	{ok, true} = has_role("user:admin@root", "user", "nodeset:group2@root"),
	{ok, true} = has_role("admin@nekso.net", "user", "nodeset:group2@root"),
	{error, multiple_aliases} = has_role("shared@shared.com", "user", "nodeset:group2@root"),

	{ok, L1} = find_role_objs("admin", "domainA"),
	[<<"user:admin@domainA">>, <<"user:admin@root">>, <<"user:root2@root">>] = 
		lists:usort(L1),
	ok.


load1() ->
    Yaml1 = data1:yaml(),
    {ok, _} = nkdomain_load:load(yaml, Yaml1, #{replace=>true}),
	{ok, #{
		<<"domainA">> := not_modified,
      	<<"proy1.domainA">> := not_modified,
      	<<"root">> := not_modified 
    }} = R1 = nkdomain_load:load(yaml, Yaml1, #{}),
	R1 = nkdomain_load:load(yaml, Yaml1, #{replace=>true}),

	{ok,[
		#{<<"member">> := <<"group:admins.people@root">>},
        <<"user:admin@root">>, <<"user:root2@root">>]} =
		get_role_objs("admin", "root"),
	{ok, <<"domain:root">>, RO} = nkdomain_role:get_role_op("root"),
	{ok, {Cache1, _, _}} = 
		nkrole:proxy_op(<<"domain:root">>, {get_cached, <<"admin">>}, RO),

	{ok, [<<"user:root1@root">>]} = get_role_objs("role1", "root"),

	{ok,[
		#{<<"member">> := <<"group:admins.people@root">>},
     	#{<<"member">> := <<"group:all.people@root">>}]} = 
		get_role_objs("member", "group:people@root"),
	{ok, {Cache2, [], [_, _]}} = 
		nkrole:proxy_op(<<"group:people@root">>, {get_cached, <<"member">>}, RO),

	{ok,[<<"user:admin@domainA">>,<<"user:admin@root">>]} = 
		get_role_objs("member", "group:admins.people@root"),
	{ok, {Cache3, [_, _], []}} = 
		nkrole:proxy_op(<<"group:admins.people@root">>, {get_cached, <<"member">>}, RO),

	{ok,[<<"user:admin@root">>]} = 
			get_role_objs("member", "group:all.people@root"),

	
	% Now we update

    Update1 = data1:update1(),
	{ok, #{<<"root">>:=loaded}} = nkdomain_load:load(yaml, Update1, #{}),
	{ok, #{<<"root">>:=not_modified}} = nkdomain_load:load(yaml, Update1, #{}),

	% This object has changed, and the cache has been modified
	{ok,[<<"user:root2@root">>]} = get_role_objs("admin", "root"),
	{ok, {Cache1B, _, _}} = nkrole:proxy_op(<<"domain:root">>, {get_cached, <<"admin">>}, RO),
	false = Cache1==Cache1B,

	{ok, []} = get_role_objs("role1", "root"),


	% This object has changed, and the cache has been modified
	{ok,[
		#{<<"member">> := <<"group:admins.people@root">>},
     	<<"user:user1@proy1.domainA">>]} = 
		get_role_objs("member", "group:people@root"),
	{ok, {Cache2B, [<<"user:user1@proy1.domainA">>], [_]}} = 
		nkrole:proxy_op(<<"group:people@root">>, {get_cached, <<"member">>}, RO),
	false = Cache2==Cache2B,

	% Has not changed
	{ok,[<<"user:admin@domainA">>,<<"user:admin@root">>]} = 
		get_role_objs("member", "group:admins.people@root"),
	{ok, {Cache3, [_, _], []}} = 
		nkrole:proxy_op(<<"group:admins.people@root">>, {get_cached, <<"member">>}, RO),

	% Has changed
	{ok,[<<"user:root1@root">>, <<"user:root2@root">>, <<"user:user1@proy1.domainA">>]} = 
		get_role_objs("member", "group:all.people@root"),
	ok.


load2() ->
    {ok, _} = nkdomain_load:load(yaml, data1:yaml(), #{replace=>true}),
	{ok,[
		#{<<"member">> := <<"group:admins.people@root">>},
        <<"user:admin@root">>, <<"user:root2@root">>]} =
		get_role_objs("admin", "root"),

    {ok,[<<"user:admin@domainA">>,<<"user:admin@root">>]} =
    	get_role_objs("member", "group:admins.people@root"),
    {ok,[<<"user:admin@root">>]} = 
    	get_role_objs("member", "group:all.people@root"),

	% Now we update, but forcing
    Update1 = data1:update1(),
	{ok, #{<<"root">>:=loaded}} = 
		nkdomain_load:load(yaml, Update1, #{replace=>true}),

	{ok,
		#{
			desc := <<"Root Object">>,
      		groups := #{<<"people">>:=_},
      		nodesets := Nodesets,
		    services := #{<<"admin">>:=_},
      		status := ready,
      		users := #{<<"admin">>:=_, <<"root1">>:=_, <<"root2">>:=_}
      	}
    } = nkdomain_orig_obj:get_obj(domain, "root"),
    0 = maps:size(Nodesets),

    {ok, [<<"user:root2@root">>]} = find_role_objs("admin", "root"),
    {ok, []} = get_role_objs("member", "group:admins.people@root"),
    ok.





