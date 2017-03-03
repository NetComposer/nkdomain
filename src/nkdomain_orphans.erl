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

%% @doc Utility functions to find and manage orphans
-module(nkdomain_orphans).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([check_aliases/0, get_orphans/0]).
-include_lib("nklib/include/nklib.hrl").


%% ===================================================================
%% Public
%% ===================================================================

%% @private
-spec check_aliases() ->
    [{binary(), [nkdomain:user_obj_id()]}].

check_aliases() ->
    Itr = riak_core_metadata:iterator({nkdomain, alias}, []),
    check_aliases(Itr, []).


%% @private
check_aliases(Itr, Acc) ->
    case riak_core_metadata:itr_done(Itr) of
        true -> 
            Acc;
        false ->
            Itr1 = riak_core_metadata:itr_next(Itr),
            Acc1 = case riak_core_metadata:itr_key_values(Itr) of
                {_, ['$deleted']} -> 
                    Acc;
                {Key, _V} ->
                    {ok, #{aliases:=Aliases1}} = nkdomain_obj2:get_obj(alias, Key),
                    Aliases2 = lists:filter(
                        fun(ObjUserId) ->
                            {ok, {Class, ObjId}} = nkdomain_util:get_parts(ObjUserId),
                            case nkdomain_obj2:get_obj(Class, ObjId) of
                                {ok, Data} -> 
                                    case Data of
                                        #{alias:=ObjAliases} ->
                                            not lists:member(Key, ObjAliases);
                                        _ ->
                                            true
                                    end;
                                {error, not_found} -> 
                                    true;
                                {error, Error} ->
                                    lager:warning("Error checking orphans for ~p ~s: ~p", 
                                                  [Class, ObjId, Error]),
                                    false
                            end
                        end,
                        Aliases1),
                    case Aliases2 of
                        [] -> Acc;
                        _ -> [{Key, Aliases2}|Acc]
                    end
            end,
            check_aliases(Itr1, Acc1)
    end.


%% @private
-spec get_orphans() ->
    [{nkdomain:class(), nkdomain:obj_id()}].

get_orphans() ->
    lists:foldl(
        fun(Class, Acc) ->
            Itr = riak_core_metadata:iterator({nkdomain, Class}, []),
            get_class_orphans(Class, Itr, Acc)
        end,
        [],
        [domain, group, user, nodeset, service]).


%% @private
get_class_orphans(Class, Itr, Acc) ->
    case riak_core_metadata:itr_done(Itr) of
        true -> 
            Acc;
        false ->
            Itr1 = riak_core_metadata:itr_next(Itr),
            Acc1 = case riak_core_metadata:itr_key_values(Itr) of
                {_, ['$deleted']} -> 
                    Acc;
                {Key, _V} ->
                    case binary:split(Key, <<"@">>) of
                        [Name, Domain] -> ok;
                        [Domain] -> Name = <<>>
                    end,
                    case check_orphan(Class, Name, Domain) of
                        true -> Acc;
                        false -> [{Class, Key}|Acc]
                    end
            end,
            get_class_orphans(Class, Itr1, Acc1)
    end.


%% @private
check_orphan(domain, Name, Domain) ->
    case binary:split(Domain, <<".">>) of
        [_] ->
            do_check_orphan(domain, Name, domain, Domain);
        [Head, Rest] ->
            check_orphan(domain, Head, Rest)
    end;

check_orphan(group, Name, Domain) ->
    case binary:split(Name, <<".">>) of
        [_] ->
            do_check_orphan(group, Name, domain, Domain);
        [Name1, Rest] ->
            do_check_orphan(group, Name1, group, <<Rest/binary, $@, Domain/binary>>)
    end;

check_orphan(Class, Name, Domain) ->
    do_check_orphan(Class, Name, domain, Domain).



%% @private
do_check_orphan(Class, Name, BaseClass, BaseName) ->
    Ok = case nkdomain_obj2:do_call(BaseClass, BaseName, {check_orphan, Class, Name}, #{}) of
        {ok, Bool} -> 
            Bool;
        {error, not_found} -> 
            false;
        {error, Error} -> 
            lager:warning("Error checking orphans for ~p ~s: ~p", 
                          [BaseClass, BaseName, Error]),
            true
    end,
    lager:info("Checking ~p:~s at ~p:~s: ~p", [Class, Name, BaseClass, BaseName, Ok]),
    Ok.




