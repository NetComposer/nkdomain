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

-module(parse_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).
-include_lib("nklib/include/nklib.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
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
		    	fun() -> yaml() end,
		    	fun() -> map() end,
		    	fun() -> json() end
			]
		end
  	}.


yaml() ->
	{error, {invalid_yaml, no_domains}} = p_yaml("abc"),
	{error, {invalid_yaml, {scanner, _}}} = p_yaml("root: :"),
	{error, {invalid_name, <<"a@b">>}} = p_yaml("a@b:"),
	B1 = "root:\n groups:\n",
	{error, {invalid_name, <<"root.groups.a.b">>}} = p_yaml(B1++"    a.b:"),
	B2 = "root:\n groups:\n  g1:\n",
	{error, {syntax_error, <<"root.groups.g1.disabled">>}} = p_yaml(B2++"   disabled: none"),
	{ok, _} = p_yaml(B2++"   remove: true"),
	{error, {syntax_error, <<"root.groups.g1.remove">>}} = 
		p_yaml(B2++"   remove: true\n   disabled: true"),
	{error, {syntax_error,<<"root.groups.g1.groups.g11.disabled:none">>}} = 
		p_yaml(B2++"   groups:\n    g11:\n     disabled:none"),
	{error, {syntax_error,<<"root.groups.g1.groups.g11.disabled">>}} = 
		p_yaml(B2++"   groups:\n    g11:\n     disabled: none"),
	B3 = "root:\n roles:\n  member:\n",
	{error, {syntax_error, <<"root.roles.member.none:a@b">>}} = 
		p_yaml(B3++"    - none:a@b"),
	{error, {syntax_error, <<"root.roles.member.member:none:a@b">>}} = 
		p_yaml(B3++"    - member: none:a@b"),

	{ok, _} = p_yaml(data1:yaml()),
	ok.


map() ->
	{error, {invalid_map, no_domains}} = p_map(#{}),
	{error, {invalid_name, <<"a@b">>}} = p_map(#{"a@b"=>#{}}),
	E01 = #{root=>#{groups=>#{<<"a.b">>=>#{}}}},
	{error, {invalid_name, <<"root.groups.a.b">>}} = p_map(E01),
	E02 = #{<<"root">>=>#{"groups"=>#{g1=>#{"disabled"=>none}}}},
	{error, {syntax_error, <<"root.groups.g1.disabled">>}} = p_map(E02),
	E03 = #{<<"root">>=>#{"groups"=>#{g1=>#{"disabled"=>"true"}}}},
	{ok, _} = p_map(E03),
	E04 = #{root=>#{groups=>#{g1=>#{remove=>true}}}},
	{ok, _} = p_map(E04),
	E05 = #{root=>#{groups=>#{g1=>#{remove=>true, disabled=>true}}}},
	{error, {syntax_error, <<"root.groups.g1.remove">>}} = p_map(E05),
	E06 = #{root=>#{groups=>#{g1=>#{groups=>#{g11=>#{disabled=>none}}}}}},
	{error, {syntax_error,<<"root.groups.g1.groups.g11.disabled">>}} = p_map(E06),
	E07 = #{root=>#{roles=>#{member=>["none:a@b"]}}},
	{error, {syntax_error, <<"root.roles.member.none:a@b">>}} = p_map(E07),
	E08 = #{root=>#{roles=>#{member=>[#{member=>'none:a@b'}]}}},
	{error, {syntax_error, <<"root.roles.member.member:none:a@b">>}} = p_map(E08),
    {ok, E09A} = p_yaml(data1:yaml()),
    {ok, E09A} = p_map(maps:from_list(E09A)),

	{ok, _} = p_map(#{root=>#{services=>#{admin=>#{path=>"my path"}}}}),
	{error,{syntax_error,<<"root.services.admin.hosts">>}} = 
		p_map(#{root=>#{services=>#{admin=>#{hosts=>"my host"}}}}),
	{error,{syntax_error,<<"root.services.dns.path">>}} = 
		p_map(#{root=>#{services=>#{dns=>#{path=>"my path"}}}}),
	{ok, _} = p_map(#{root=>#{services=>#{dns=>#{hosts=>"my host"}}}}),
	ok.




json() ->
	{error, {invalid_json, no_domains}} = p_json(#{}),
	{error, {invalid_name, <<"a@b">>}} = p_json(#{<<"a@b">>=>#{}}),
	E01 = #{root=>#{groups=>#{<<"a.b">>=>#{}}}},
	{error, {invalid_name, <<"root.groups.a.b">>}} = p_json(E01),
	E02 = #{<<"root">>=>#{<<"groups">>=>#{g1=>#{disabled=>none}}}},
	{error, {syntax_error, <<"root.groups.g1.disabled">>}} = p_json(E02),
	E03 = #{<<"root">>=>#{groups=>#{g1=>#{disabled=>"true"}}}},
	{ok, _} = p_json(E03),
	E04 = #{root=>#{groups=>#{g1=>#{remove=>true}}}},
	{ok, _} = p_json(E04),
	E05 = #{root=>#{groups=>#{g1=>#{remove=>true, disabled=>true}}}},
	{error, {syntax_error, <<"root.groups.g1.remove">>}} = p_json(E05),
	E06 = #{root=>#{groups=>#{g1=>#{groups=>#{g11=>#{disabled=>none}}}}}},
	{error, {syntax_error,<<"root.groups.g1.groups.g11.disabled">>}} = p_json(E06),
	E07 = #{root=>#{roles=>#{member=>["none:a@b"]}}},
	{error, {syntax_error, <<"root.roles.member.none:a@b">>}} = p_json(E07),
	E08 = #{root=>#{roles=>#{member=>[#{member=>'none:a@b'}]}}},
	{error, {syntax_error, <<"root.roles.member.member:none:a@b">>}} = p_json(E08),
    {ok, E09A} = p_yaml(data1:yaml()),
    {ok, E09A} = p_json(maps:from_list(E09A)).







%%%%%%%%%%%%%%%

p_yaml(Data) ->
	nkdomain_load:parse(yaml, Data).

p_map(Data) ->
	nkdomain_load:parse(map, Data).

p_json(Data) when is_map(Data) ->
	Json = nklib_json:encode_pretty(Data),
	nkdomain_load:parse(json, Json);

p_json(Json) ->
	nkdomain_load:parse(json, Json).
