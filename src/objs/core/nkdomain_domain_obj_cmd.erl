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

%% @doc User Object API
-module(nkdomain_domain_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

%% ===================================================================
%% API
%% ===================================================================

cmd(<<"check_name">>, #nkreq{data=#{name:=Name}}) ->
    {ok, #{name=>nkdomain_util:name(Name)}};

cmd(<<"find">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_db:find(Id) of
                #obj_id_ext{obj_id=ObjId} ->
                    Filters1 = maps:get(filters, Data, #{}),
                    Filters2 = Filters1#{domain_id=>ObjId},
                    Data2 = maps:remove(id, Data#{filters=>Filters2}),
                    {ok, EsOpts} = nkdomain_store_es_util:get_opts(),
                    case nkdomain_store_es_search:search(Data2, EsOpts) of
                        {ok, Total, List, _Aggs, _Meta} ->
                            {ok, #{total=>Total, data=>List}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

cmd(<<"find_all">>, #nkreq{data=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case nkdomain_db:find(Id) of
                #obj_id_ext{path=Path} ->
                    Filters1 = maps:get(filters, Data, #{}),
                    Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
                    Data2 = maps:remove(id, Data#{filters=>Filters2}),
                    {ok, EsOpts} = nkdomain_store_es_util:get_opts(),
                    case nkdomain_store_es_search:search(Data2, EsOpts) of
                        {ok, Total, List, _Aggs, _Meta} ->
                            {ok, #{total=>Total, data=>List}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        Error ->
            Error
    end;

%%cmd(<<"find_types">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            case nkdomain_db:aggs({type, Id, #{}}) of
%%                {ok, Total, List} ->
%%                    {ok, #{total=>Total, data=>maps:from_list(List)}};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        Error ->
%%            Error
%%    end;
%%
%%cmd(<<"find_all_types">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            case nkdomain_db:aggs({type, Id, #{deep=>true}}) of
%%                {ok, Total, List} ->
%%                    {ok, #{total=>Total, data=>maps:from_list(List)}};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        Error ->
%%            Error
%%    end;
%%
%%cmd(<<"find_childs">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            Search = nkdomain_db:search({paths, Id, #{}}),
%%            nkdomain_api_util:search(Search);
%%        Error ->
%%            Error
%%    end;
%%
%%cmd(<<"find_all_childs">>, #nkreq{data=Data}=Req) ->
%%    case get_domain(Data, Req) of
%%        {ok, Id} ->
%%            Search = nkdomain_db:search({paths, Id, #{deep=>true}}),
%%            nkdomain_api_util:search(Search);
%%        Error ->
%%            Error
%%    end;

cmd(<<"unload_childs">>, #nkreq{data=#{wait_time := WaitTime, max_retries := MaxRetries}=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            unload_childs(Id, MaxRetries, WaitTime);
        Error ->
            Error
    end;

cmd(<<"delete_childs_of_type">>, #nkreq{data=#{type := Type, wait_time := WaitTime, max_retries := MaxRetries}=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            delete_childs_of_type(Id, MaxRetries, WaitTime, Type);
        Error ->
            Error
   end;

cmd(<<"delete_types">>, #nkreq{data=#{types := Types, 
                                      wait_time := WaitTime, 
                                      max_retries := MaxRetries}=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            delete_types(Id, MaxRetries, WaitTime, Types);
        Error ->
            Error
   end;


cmd(<<"clear">>, #nkreq{data=#{wait_time := WaitTime, 
                               max_retries := MaxRetries}=Data}=Req) ->
    case get_domain(Data, Req) of
        {ok, Id} ->
            case unload_childs(Id, MaxRetries, WaitTime) of 
                ok ->
                    case nkdomain:get_types(Id) of 
                        {ok, _, Types, _} ->
                            delete_types(Id, MaxRetries, WaitTime, [ T || {T, _} <- Types]);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
   end;

cmd(<<"create_child">>, #nkreq{data=#{ path := Path, data := ChildData }=Data}=Req) ->
   Obj = ChildData#{ path => Path },
   case nkdomain_obj_make:create(Obj) of 
	{ok, #obj_id_ext{obj_id=ObjId}, _} ->
	    {ok, #{ id => ObjId }};
	Error -> 
	    Error
   end; 

cmd(<<"create_link">>, #nkreq{data=#{ from := ChildPath, to_parent := ParentPath }=Data}=Req) ->
   Obj = #{
        path => ChildPath,
        parent_id => ParentPath
   },	
   case nkdomain_obj_make:create(Obj) of 
	{ok, #obj_id_ext{obj_id=ObjId}, _} ->
	    {ok, #{ id => ObjId }};
	Error -> 
	    Error
   end; 

cmd(<<"clean">>, _Req) ->
    nkdomain:clean();

cmd(<<"set_config">>, #nkreq{user_id=UserId, data=#{ domain_id:=DomainId, key := Key, value := Value }}=Req) ->
    #nkreq{user_state=#{nkdomain_obj_ids:=#{<<"domain">>:=UserDomain}}} = Req,
    case nkdomain_admin_util:is_authorized(UserId) of
        true ->
            case nkdomain_api_util:is_subdomain(UserDomain, DomainId) of
                {ok, true} ->
                    nkdomain_domain:set_config(DomainId, Key, Value),
                    {ok, #{}, Req};
                {ok, false} ->
                    {error, unauthorized, Req};
                {error, Error} ->
                    {error, Error, Req}
            end;
        false ->
            {error, unauthorized, Req}
    end;    

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:cmd(Cmd, ?DOMAIN_DOMAIN, Req).


%% ===================================================================
%% Internal
%% ===================================================================

get_domain(Data, Req) ->
    nkdomain_api_util:get_id(?DOMAIN_DOMAIN, Data, Req).


delete_types(Id, _, _, []) -> {ok, #{}};
delete_types(Id, MaxRetries, WaitTime, [T|Rem]) ->
    case delete_childs_of_type(Id, MaxRetries, WaitTime, T) of 
        {ok, _} ->
            delete_types(Id, MaxRetries, WaitTime, Rem);
        Other ->
            Other
    end.

delete_childs_of_type(Id, MaxRetries, WaitTime, Type) ->
    nkdomain:remove_path_type(Id, Type),
    case nkdomain_api_util:wait_for_condition(MaxRetries, WaitTime, 
                                              nkdomain_api_util:has_childs_type_condition_fun(Id, Type)) of
        {ok, true} ->
            lager:info("Childs of type ~s deleted from domain ~s", [Type, Id]),
            {ok, #{}};
        {ok, false} ->
            {error, object_has_childs};
        {error, Error} ->
            {error, Error}
    end.

unload_childs(Id, MaxRetries, WaitTime) ->
    case nkdomain_db:load(Id) of
        #obj_id_ext{path=DomainPath, type=?DOMAIN_DOMAIN} ->
            ActionFun = fun() ->
                                nkdomain_domain:unload_childs(Id)
                        end,
            case nkdomain_api_util:wait_for_condition(MaxRetries, WaitTime, ActionFun, nkdomain_api_util:is_not_loaded_condition_fun(DomainPath)) of
                {ok, true} ->
                    lager:info("Childs unloaded for domain ~s", [DomainPath]),
                    ok;
                {ok, false} ->
                    {error, object_has_childs};
                {error, Error} ->
                    {error, Error}
            end;
        #obj_id_ext{} ->
            {error, {domain_unknown, Id}}
    end.
