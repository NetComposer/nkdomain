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

-module(nkdomain_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([syntax_callback/2, update_callback/3]).

-export([make_user_id/1, get_module/1, get_all/1, meta_get_all/1]).
-export([register_classes/1, resolve/1, get_parts/1]).
-include_lib("nklib/include/nklib.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets a service's syntax
-spec syntax_callback(nkdomain:class(), [atom()]) ->
    map().

syntax_callback(service, Plugins) ->
    {Mod, Fun} = nkdomain_app:get(syntax_callback),
    case nklib_util:apply(Mod, Fun, [Plugins]) of
        not_exported ->
            #{};
        Other ->
            lager:warning("Invalid response calling ~p:~p: ~p",
                          [Mod, Fun, Other]),
            #{}
    end.


%% @doc Gets update callback
-spec update_callback(nkdomain:class(), nkdomain:obj_id(), nkdomain:obj()|removed) ->
    ok | {error, callback_error}.

update_callback(Class, ObjId, Obj) ->
    case nkdomain_app:get({update, Class}) of
        {Mod, Fun} ->
            case nklib_util:apply(Mod, Fun, [ObjId, Obj]) of
                ok ->
                    ok;
                not_exported ->
                    ok;
                Other ->
                    lager:error("Error calling ~p:~p: ~p", [Mod, Fun, Other]),
                    {error, {callback_error, Other}}
            end;
        _ ->
            ok
    end.



%% @doc Generates a valid user_obj_id() from a pair of class and obj_id() or
%% any valid term
-spec make_user_id(string()|binary()|{nkdomain:class(), nkdomain:obj_id()}) ->
    {ok, nkdomain:user_obj_id()} | {error, term()}.

make_user_id({Class, ObjId}) when is_atom(Class), is_binary(ObjId) ->
    BinClass = atom_to_binary(Class, utf8),
    {ok, <<BinClass/binary, $:, ObjId/binary>>};

make_user_id(Term) ->
    case get_parts(Term) of
        {ok, {Class, ObjId}} -> 
            make_user_id({Class, ObjId});
        {error, Error} ->
            {error, Error}
    end.



%% @private
get_all(Class) ->
    {ok, List} = nkbase:list_keys(nkdomain, Class, #{filter_deleted=>true}),
    List.


%% @private
meta_get_all(Class) ->
    Itr = riak_core_metadata:iterator({nkdomain, Class}, []),
    meta_get_all(Itr, []).


%% @private
meta_get_all(Itr, Acc) ->
    case riak_core_metadata:itr_done(Itr) of
        true -> 
            Acc;
        false ->
            Itr1 = riak_core_metadata:itr_next(Itr),
            Acc1 = case riak_core_metadata:itr_key_values(Itr) of
                {_, ['$deleted']} -> Acc;
                {Key, _V} -> [Key|Acc]
            end,
            meta_get_all(Itr1, Acc1)
    end.


%% @private
register_classes([]) ->
    ok;

register_classes([Class|Rest]) ->
    Mod = get_module(Class),
    code:ensure_loaded(Mod),
    Base = #{
        n => 3, 
        backend => leveldb, 
        reconcile => lww, 
        indices=>[{domain, {func, fun index_domain/2}}]
    },
    Data = case erlang:function_exported(Mod, get_backend, 1) of
        true -> Mod:get_backend(Base);
        false -> Base
    end,
    ok = nkbase:register_class(nkdomain, Class, Data),
    register_classes(Rest).


%% @private
%% Search with nkbase:search(nkdomain, Class, {domain, all})
index_domain({nkdomain, _Class, Key}, _Obj) ->
    case binary:split(Key, <<"@">>) of
        [_User, Domain] -> Domain;
        [Domain] -> Domain
    end.


%% @doc Extracts class and obj_id and gets obj's pid() from an user_obj_id()
-spec resolve(binary()) ->
    {ok, nkdomain:class(), nkdomain:obj_id(), pid()} | {error, term()}.

resolve(UserObjId) ->
    case get_parts(UserObjId) of
        {ok, {Class, ObjId}} ->
            case nkdomain_obj:get_pid(Class, ObjId) of
                {ok, Pid} ->
                    {ok, {Class, ObjId, Pid}};
                {error, not_found} ->
                    case binary:split(ObjId, <<"@">>) of
                        [Name, Domain] ->
                            case nkdomain:get_aliases(Domain) of
                                [<<"domain:", Domain2/binary>>] ->
                                    ObjId2 = <<Name/binary, $@, Domain2/binary>>,
                                    case nkdomain_obj:get_pid(Class, ObjId2) of
                                        {ok, Pid} -> {ok, 
                                            {Class, ObjId2, Pid}};
                                        {error, Error} -> 
                                            {error, Error}
                                    end;
                                _ ->
                                    {error, not_found}
                            end;
                        _ ->
                            {error, not_found}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private Extracts class and obj_id from a obj_user_id
%% Ids without '@' are of class 'domain' if no class is found.
%% Ids without class are 'alias'
-spec get_parts(nkdomain:user_obj_id()) ->
    {ok, {nkdomain:class(), nkdomain:obj_id()}} | {error, term()}.

get_parts(Id) ->
    Id1 = nklib_util:to_list(Id),
    case lists:member($@, Id1) of
        false ->
            case lists:member($:, Id1) of
                false ->
                    % It has no domain and no class
                    {ok, {domain, list_to_binary(Id1)}};
                true ->
                    % It has no domain and has a class
                    case nklib_parse:uris(Id) of
                        [#uri{scheme=domain, user= <<>>, domain=Domain}] ->
                            {ok, {domain, Domain}};
                        [#uri{scheme=Class, user= <<>>, domain=User}] ->
                            case catch get_module(Class) of
                                {'EXIT', _} ->
                                    {error, invalid_class};
                                _ ->
                                    {ok, {Class, <<User/binary, "@root">>}}
                            end;
                        _ ->
                            {error, invalid_obj_id}
                    end
            end;
        true ->
            case lists:member($:, Id1) of
                false ->
                    % It has domain but no class, it is an alias
                    {ok, {alias, nklib_util:to_binary(Id)}};
                true ->
                    % It has domain and class
                    case nklib_parse:uris(Id) of
                        [#uri{scheme=Class, user=User, domain=Domain}] ->
                            case catch get_module(Class) of
                                {'EXIT', _} ->
                                    {error, invalid_class};
                                _ ->
                                    {ok, {Class, <<User/binary, $@, Domain/binary>>}}
                            end;
                        _ ->
                            {error, invalid_obj_id}
                    end
            end
    end.


%% @private
get_module(domain) -> nkdomain_obj_domain;
get_module(group) -> nkdomain_obj_group;
get_module(user) -> nkdomain_obj_user;
get_module(token) -> nkdomain_obj_token;
get_module(alias) -> nkdomain_obj_alias;
get_module(service) -> nkdomain_obj_service;
get_module(nodeset) -> nkdomain_obj_nodeset.
