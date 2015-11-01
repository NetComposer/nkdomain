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

-module(token_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkdomain_role, [get_roles/1, get_role_objs/2, find_role_objs/2, has_role/3]).
-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

tokens_test_() ->
  	{setup, 
    	fun() -> 
    		ok = nkdomain_app:start(),
		    nkdomain_service_mngr:register(admin, test_srv_admin),
		    nkdomain_service_mngr:register(dns, test_srv_dns)
  		end,
		fun(_) -> 
			ok 
		end,
	    fun(_) ->
		    [
		    	fun() -> basic() end
			]
		end
  	}.


basic() ->
	YamlData1 = data1:yaml(),
	timer:sleep(2000),		% Wait for NkBASE
	{ok, _} = nkdomain_load:load(yaml, YamlData1, #{replace=>true}),

	stop_tokens(),
	{error, not_found} = nkdomain_obj_token:new_token(user, <<"admin@root1">>, #{}),
	{error, no_password} = nkdomain_obj_token:new_token(user, <<"admin@root">>, #{}),
	{error, invalid_password} = 
		nkdomain_obj_token:new_token(user, <<"admin@root">>, #{password=>"123"}),
	{ok, Token1} = 
		nkdomain_obj_token:new_token(user, <<"admin@root">>, #{password=>"1234"}),

	[Token1] = nkdomain_obj_user:get_tokens("admin@root"),
	{ok, user, <<"admin@root">>} = nkdomain_obj_token:check_token(Token1, #{}),

	Data2 = #{valid_time=>500, ip=>{1,2,3,4}, port=>1234, password=>"1234"},
	{ok, Token2} = nkdomain_obj_token:new_token(user, <<"admin@root">>, Data2),
	[Token1, Token2] = nkdomain_obj_user:get_tokens("admin@root"),
	{error, invalid_connection} = nkdomain_obj_token:check_token(Token2, #{}),
	{ok, user, <<"admin@root">>} = 
		nkdomain_obj_token:check_token(Token1, #{ip=>{1,2,3,4}, port=>1234}),

	{error, invalid_password} = 
		nkdomain_obj_token:new_token(user, <<"root1@root">>, #{password=>"123"}),
	{ok, Token3} = 
		nkdomain_obj_token:new_token(user, <<"root1@root">>, 
									 #{valid_time=>500, password=>"4321"}),
	[Token3] = nkdomain_obj_user:get_tokens("root1@root"),

	timer:sleep(600),
	[Token1] = nkdomain_obj_user:get_tokens("admin@root"),
	[] = nkdomain_obj_user:get_tokens("root1@root"),
	ok = nkdomain_obj_token:renew_token(Token1, #{valid_time=>100}),
	timer:sleep(200),
	[] = nkdomain_obj_user:get_tokens("admin@root"),
	ok.



stop_tokens() ->
	{ok, All} = nkdist:get_procs(nkdomain_obj),
	lists:foldl(
		fun
			({{token, Id}, Pid}, Acc) -> Pid ! token_timeout, [Id|Acc];
			(_, Acc) -> Acc
		end,
		[],
		All).
