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

-module(basic_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkdomain_app:start()
		end,
		fun(_) -> 
			ok 
		end,
	    fun(_) ->
		    [
				fun() -> load() end,
                fun() -> data() end,
                fun() -> update() end
			]
		end
  	}.



load() ->
    nkdomain_service_mngr:register(admin, test_srv_admin),
    nkdomain_service_mngr:register(dns, test_srv_dns),
	YamlData1 = data1:yaml(),
	{ok, _} = nkdomain_load:load(yaml, YamlData1, #{replace=>true}),
    {ok, Res1} = nkdomain_load:load(yaml, YamlData1, #{}),
    {ok, Res1} = nkdomain_load:load(yaml, YamlData1, #{replace=>true}),
    #{
        <<"root">> := not_modified,
        <<"domainA">> := not_modified,
        <<"proy1.domainA">> := not_modified
    } = Res1,
    {ok, RootUp1} = nkdomain_obj:get_updated(domain, root),
    {ok, DomAUp1} = nkdomain_obj:get_updated(domain, "domainA"),
    {ok, Proy1Up1} = nkdomain_obj:get_updated(domain, "proy1.domainA"),

    YamlData2 = 
        re:replace(YamlData1, "desc: Domain A", "desc: Domain 'A'", [{return, binary}]),
    {ok, Res2} = nkdomain_load:load(yaml, YamlData2, #{}),
    #{
        <<"root">> := not_modified,
        <<"domainA">> := loaded,
        <<"proy1.domainA">> := not_modified
    } = Res2,
    {ok, RootUp1} = nkdomain_obj:get_updated(domain, root),
    {ok, DomAUp2} = nkdomain_obj:get_updated(domain, "domainA"),
    {ok, Proy1Up1} = nkdomain_obj:get_updated(domain, "proy1.domainA"),
    true = DomAUp2 > DomAUp1,

    {ok, Exp1} = nkdomain:export(<<"root">>),
    Json1 = nklib_json:encode_pretty(#{<<"root">>=>Exp1}),

    {ok, #{<<"root">> := not_modified}} = nkdomain_load:load(json, Json1, #{}),
    {ok, RootUp2} = nkdomain_obj:get_updated(domain, root),

    Json2 = re:replace(Json1, "\"status\": \"ready\"", "\"status\": \"standby\"", 
                       [{return, binary}]),
    {ok, #{<<"root">> := loaded}} = nkdomain_load:load(json, Json2, #{}),
    {ok, RootUp3} = nkdomain_obj:get_updated(domain, root),
    true = RootUp3 > RootUp2,

    [
        {{alias, <<"admin@domain_a.com">>}, _} ,
        {{alias, <<"admin@nekso.net">>}, _},
        {{alias, <<"domain_a.com">>}, _},
        {{alias, <<"nekso.net">>}, _},
        {{alias, <<"shared@shared.com">>}, _},
        {{alias, <<"user_root1@domain.com">>}, _},
        {{alias, <<"user_root2@domain.com">>}, _},
        {{domain, <<"domainA">>}, _},
        {{domain, <<"proy1.domainA">>}, _},
        {{domain, <<"root">>}, _},
        {{group, <<"a.zones@root">>}, _},
        {{group, <<"a1.a.zones@root">>}, _},
        {{group, <<"a2.a.zones@root">>}, _},
        {{group, <<"a@proy1.domainA">>}, _},
        {{group, <<"admins.people@root">>}, _},
        {{group, <<"all.nodes@root">>}, _},
        {{group, <<"all.people@root">>}, _},
        {{group, <<"b.zones@root">>}, _},
        {{group, <<"b1.b.zones@root">>}, _},
        {{group, <<"b2.b.zones@root">>}, _},
        {{group, <<"b@proy1.domainA">>}, _},
        {{group, <<"nodes@root">>}, _},
        {{group, <<"people@root">>}, _},
        {{group, <<"zones@root">>}, _},
        {{nodeset, <<"group1@domainA">>}, _},
        {{nodeset, <<"group1@root">>}, _},
        {{nodeset, <<"group2@root">>}, _},
        {{service, <<"admin@domainA">>}, _},
        {{service, <<"admin@root">>}, _},
        {{service, <<"dns@proy1.domainA">>}, _},
        {{service, <<"dns@root">>}, _},
        {{user, <<"admin@domainA">>}, _},
        {{user, <<"admin@root">>}, _},
        {{user, <<"root1@root">>}, _},
        {{user, <<"root2@root">>}, _},
        {{user, <<"user1@domainA">>}, _},
        {{user, <<"user1@proy1.domainA">>}, _},
        {{user, <<"user2@proy1.domainA">>}, _}
    ] = 
        lists:sort(element(2, nkdist:get_procs(nkdomain_obj))),
    ok.


data() ->
	{ok, Root1} = nkdomain_obj:get_obj(domain, "root"),
    #{
        desc := <<"Root Object">>,
        alias := [],
        status := standby,
        groups := #{<<"nodes">>:=_, <<"people">>:=_, <<"zones">>:=_},
        users := #{<<"admin">>:=_, <<"root1">>:=_, <<"root2">>:=_},
        nodesets := #{<<"group1">>:=_, <<"group2">>:=_},
        services := #{<<"admin">>:=_, <<"dns">>:=_},
        owner := <<"root">>,
        roles := #{
            <<"admin">> := [
                #{<<"member">> := <<"group:admins.people@root">>},
                <<"user:admin@root">>, <<"user:root2@root">>
            ],
            <<"role1">> := [<<"user:root1@root">>]}
    } = Root1,

    {ok, User1} = nkdomain_obj:get_obj(user, <<"admin@root">>),
    #{
        alias := [<<"admin@nekso.net">>],
        name := <<"Global">>,
        surname := <<"Admin">>,
        password := <<"NKD!!AJGN2rWPBLqXgnqRpaUWO695u6R!">>,
        owner := <<"root">>,
        roles := #{}
    } = User1,
    [<<"user:root1@root">>, <<"user:root2@root">>] = 
        nkdomain:get_aliases(<<"shared@shared.com">>),
    {ok, User1Pid} = nkdomain_obj:get_pid(user, <<"admin@root">>),
    User1Pid ! timeout,
    timer:sleep(50),
    false = is_process_alive(User1Pid),
    [<<"user:root1@root">>, <<"user:root2@root">>] = 
        nkdomain:get_aliases(<<"shared@shared.com">>),
    {ok, User1Pid_2} = nkdomain_obj:get_pid(user, <<"root1@root">>),
    false = User1Pid == User1Pid_2,
    {ok, User1} = nkdomain_obj:get_obj(user, <<"admin@root">>),

    [<<"domainA">>, <<"proy1.domainA">>, <<"root">>] = lists:sort(nkdomain_util:get_all(domain)),

    [<<"domain:proy1.domainA">>] = nkdomain:get_aliases(<<"nekso.net">>),

    [
        <<"admin@domainA">>,
        <<"admin@root">>,
        <<"root1@root">>,
        <<"root2@root">>,
        <<"user1@domainA">>,
        <<"user1@proy1.domainA">>,
        <<"user2@proy1.domainA">>
    ] = 
        lists:sort(nkdomain_util:get_all(user)),

    [
        <<"group1@domainA">>,
        <<"group1@root">>,
        <<"group2@root">>
    ] =
        lists:sort(nkdomain_util:get_all(nodeset)),

    [
        <<"admin@domainA">>,
        <<"admin@root">>,
        <<"dns@proy1.domainA">>,
        <<"dns@root">>
    ] = 
        lists:sort(nkdomain_util:get_all(service)),

    [
        <<"a.zones@root">>,
        <<"a1.a.zones@root">>,
        <<"a2.a.zones@root">>,
        <<"a@proy1.domainA">>,
        <<"admins.people@root">>,
        <<"all.nodes@root">>,
        <<"all.people@root">>,
        <<"b.zones@root">>,
        <<"b1.b.zones@root">>,
        <<"b2.b.zones@root">>,
        <<"b@proy1.domainA">>,
        <<"nodes@root">>,
        <<"people@root">>,
        <<"zones@root">>
    ] = 
        lists:sort(nkdomain_util:get_all(group)),

    [
        <<"admin@domain_a.com">>,
        <<"admin@nekso.net">>,
        <<"domain_a.com">>,
        <<"nekso.net">>,
        <<"shared@shared.com">>,
        <<"user_root1@domain.com">>,
        <<"user_root2@domain.com">>
    ] = 
        lists:sort(nkdomain_util:get_all(alias)),

    {ok, #{groups := #{<<"admins">>:=_, <<"all">>:=_}}} = 
        nkdomain_obj:get_obj(group, "people@root"),

    {ok,
        #{
            roles := #{<<"member">>:=[<<"user:admin@domainA">>,<<"user:admin@root">>]}
    }} = 
        nkdomain_obj:get_obj(group, "admins.people@root"),

    {ok, #{}} = nkdomain_obj:get_obj(group, "b@proy1.domainA"),
    {error, not_found} = nkdomain_obj:get_obj(group, "c@proy1.domainA"),
    {error, not_found} = nkdomain_obj:get_obj(group, "b@proy2.domainA"),
    ok.


update() ->
    State1 = do_update(root, #{status=>ready}),
    true = State1==loaded orelse State1==not_modified,
    not_modified = do_update(root, #{status=>ready}),

    {error, {syntax_error, <<"root.status">>}} = do_update(root, #{status=>invalid}),

    _ = do_update(root, 
        [{users, [{user_t1, [{alias, ["user_t1@domain.com", "shared@shared.com"]}]}]}]),
    {ok, #{alias:=[<<"shared@shared.com">>, <<"user_t1@domain.com">>]}} = 
        nkdomain_obj:get_obj(user, "user_t1@root"),

    [<<"user:root1@root">>,<<"user:root2@root">>,<<"user:user_t1@root">>] = 
        nkdomain:get_aliases("shared@shared.com"),

    {ok, #{meta:=<<"core;id=group1">>}} = nkdomain_obj:get_obj(nodeset, <<"group1@root">>),
    _ = do_update(root, #{nodesets=>#{group3=>#{meta=>"meta3"}}}, #{}),
    {ok, #{meta:=<<"meta3">>}} = nkdomain_obj:get_obj(nodeset, <<"group3@root">>),
    {error, not_found} = nkdomain_obj:get_obj(nodeset, <<"group4@root">>),
    [<<"group1@domainA">>, <<"group1@root">>, 
     <<"group2@root">>, <<"group3@root">>] = lists:sort(nkdomain_util:get_all(nodeset)),

    [<<"user:user_t1@root">>] = nkdomain:get_aliases("user_t1@domain.com"),
    {error, {syntax_error, <<"root.users.user_t1.remove">>}} = 
        do_update(root, #{users=>#{user_t1=>#{alias=>"alias", remove=>true}}}),
    loaded = do_update(root, #{users=>#{user_t1=>#{remove=>true}}}),
    {error, not_found} = nkdomain_obj:get_obj(user, "user_t1@root"),
    % The alias will be removed after a timeout
    {ok, #{aliases:=[]}} = nkdomain_obj:get_obj(alias, "user_t1@domain.com"),
    [<<"user:root1@root">>,<<"user:root2@root">>] = 
        nkdomain:get_aliases("shared@shared.com"),

    {ok, #{}} = nkdomain_obj:get_obj(group, "people@root"),
    {ok, #{}} = nkdomain_obj:get_obj(group, "admins.people@root"),
    {ok, #{}} = nkdomain_obj:get_obj(group, "all.people@root"),
    loaded = do_update("root", #{groups=>#{people=>#{remove=>true}}}),
    {error, not_found} = nkdomain_obj:get_obj(group, "people@root"),
    {error, not_found} = nkdomain_obj:get_obj(group, "admins.people@root"),
    {error, not_found} = nkdomain_obj:get_obj(group, "all.people@root"),
    {ok, #{groups:=#{<<"nodes">>:=_, <<"zones">>:=_}}} = 
        nkdomain_obj:get_obj(domain, "root"),
    [
        <<"a.zones@root">>,
        <<"a1.a.zones@root">>,
        <<"a2.a.zones@root">>,
        <<"a@proy1.domainA">>,
        <<"all.nodes@root">>,
        <<"b.zones@root">>,
        <<"b1.b.zones@root">>,
        <<"b2.b.zones@root">>,
        <<"b@proy1.domainA">>,
        <<"nodes@root">>,
        <<"zones@root">>
    ] = 
        lists:sort(nkdomain_util:get_all(group)),

    ok = nkdomain_obj:remove_obj(domain, root),
    [<<"domainA">>, <<"proy1.domainA">>] = 
        lists:sort(nkdomain_util:get_all(domain)),
    [<<"a@proy1.domainA">>,<<"b@proy1.domainA">>] =
        lists:sort(nkdomain_util:get_all(group)),
    [<<"group1@domainA">>] = 
        lists:sort(nkdomain_util:get_all(nodeset)),
    [<<"admin@domainA">>, <<"dns@proy1.domainA">>] = 
        lists:sort(nkdomain_util:get_all(service)),
    [<<"admin@domainA">>, <<"user1@domainA">>, 
     <<"user1@proy1.domainA">>, <<"user2@proy1.domainA">>] =
        lists:sort(nkdomain_util:get_all(user)),

    % Aliases all not yet removed
    [
        <<"admin@domain_a.com">>,
        <<"admin@nekso.net">>,
        <<"domain_a.com">>,
        <<"nekso.net">>,
        <<"shared@shared.com">>,
        <<"user_root1@domain.com">>,
        <<"user_root2@domain.com">>,
        <<"user_t1@domain.com">>
    ] =  
        lists:sort(nkdomain_util:get_all(alias)),
    refresh_aliases(),

    removed = do_update(<<"proy1.domainA">>, #{remove=>true}),
    [<<"domainA">>] = nkdomain_util:get_all(domain),
    [] = nkdomain_util:get_all(group),
    [<<"group1@domainA">>] = nkdomain_util:get_all(nodeset),
    [<<"admin@domainA">>] = nkdomain_util:get_all(service),
    [<<"admin@domainA">>, <<"user1@domainA">>] = nkdomain_util:get_all(user),
    refresh_aliases(),
    [<<"admin@domain_a.com">>,<<"domain_a.com">>] = 
        lists:sort(nkdomain_util:get_all(alias)),

    ok = nkdomain_obj:remove_obj(domain, domainA),
    [] = nkdomain_util:get_all(domain),
    [] = nkdomain_util:get_all(group),
    [] = nkdomain_util:get_all(nodeset),
    [] = nkdomain_util:get_all(service),
    [] = nkdomain_util:get_all(user),
    refresh_aliases(),
    [] = lists:sort(nkdomain_util:get_all(alias)),
    {ok, []} = nkdist:get_procs(nkdomain_obj),
    ok.





%%%%%% Internal

do_update(Dom, Data) ->
    do_update(Dom, Data, #{}).


do_update(Dom, Data, Opts) ->
    case nkdomain_load:load(map, maps:put(Dom, Data, #{}), Opts) of
        {ok, Map} ->
            [{_, Result}] = maps:to_list(Map),
            Result;
        Other ->
            Other
    end.


refresh_aliases() ->
    lists:foreach(
        fun(AliasId) ->
            {ok, Pid} = nkdomain_obj:get_pid(alias, AliasId),
            Pid ! timeout
        end,
        nkdomain_util:get_all(alias)),
    timer:sleep(100).



     