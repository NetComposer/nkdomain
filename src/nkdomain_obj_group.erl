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

-module(nkdomain_obj_group).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj).

-export([init/2, load/4, removed/2, export/2, handle_call/4]).

-include("nkdomain.hrl").


-type group() ::
    nkdomain_obj:base_obj() |
    #{
        groups => [{nkdomain:obj_id(), integer()}]
    }.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

-record(state, {
	id :: nkdomain:obj_id()
}).


%% @private
-spec init(nkdomain:obj_id(), group()) ->
    {ok, nkdomain_obj:init_opts(), group(), #state{}}.

init(GroupId, Group) ->
    Base = #{groups => #{}},
    Group1 = maps:merge(Base, Group),
    {ok, #{}, Group1, #state{id=GroupId}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), group(), #state{}) ->
    {ok, group(), #state{}} | removed | {error, term()}.

load(Data, Opts, Group, State) ->
    do_load(maps:to_list(Data), Opts, Group, State).


%% @private
-spec removed(group(), #state{}) ->
    ok.

removed(Group, State) ->
    do_load([{groups, #{}}], #{replace=>true}, Group, State),
    ok.


%% @doc
-spec export(nkdomain:obj_id(), group()) ->
    map().

export(GroupId, Group) ->
    Groups = maps:fold(
        fun(Id, _Hash, Acc) ->
            Id1 = list_to_binary([Id, ".", GroupId]),
            case nkdomain_obj:export(group, Id1) of
                {ok, Map} ->
                    maps:put(Id, Map, Acc);
                {error, Error} ->
                    lager:notice("Could not read group ~s: ~p", [Id1, Error]),
                    Acc
            end
        end,
        #{},
        maps:get(groups, Group, #{})),
    case maps:size(Groups) of
        0 -> Group;
        _ -> Group#{groups=>Groups}
    end.


%% @private
-spec handle_call(term(), {pid(), term()}, nkdomain:user(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}}.

handle_call({check_orphan, group, Name}, _From, Group, State) ->
    Data = maps:get(groups, Group, []),
    Member = lists:keymember(Name, 1, Data),
    {reply, {ok, Member}, State};

handle_call({check_orphan, _Class, _Name}, _From, _Domain, State) ->
    {reply, {ok, false}, State};

handle_call(Msg, _From, _Obj, State) -> 
    lager:warning("Module ~p received unexpected call: ~p (~p)", [?MODULE, Msg, State]),
    {noreply, State}.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_load([], _Opts, Group, State) ->
    {ok, Group, State};

do_load([{groups, Map}|Rest], Opts, Group, #state{id=GroupId}=State) ->
    Ids1 = maps:get(groups, Group, []),
    Ids2 = do_load_groups(GroupId, maps:to_list(Map), Opts, Ids1),
    Ids3 = case Opts of
        #{replace:=true} ->
            maps:fold(
                fun(Id, Hash, Acc) ->
                    case maps:is_key(Id, Map) of
                        true -> 
                            maps:put(Id, Hash, Acc);
                        false ->
                            do_remove_group(GroupId, Id),
                            Acc
                    end
                end,
                #{},
                Ids2);        
        _ ->
            Ids2
    end,
    do_load(Rest, Opts, Group#{groups=>Ids3}, State);

do_load([{Key, Val}|Rest], Opts, Group, State) ->
    do_load(Rest, Opts, maps:put(Key, Val, Group), State).


%% @private
do_load_groups(_GroupId, [], _Opts, Acc) ->
    Acc;

do_load_groups(GroupId, [{Name, Data}|Rest], Opts, Acc) ->
    Name1 = list_to_binary([Name, ".", GroupId]),
    Acc1 = case nkdomain_obj:load(group, Name1, Data, Opts) of
        not_modified ->
            Acc;
        {loaded, NewData} ->
            Hash = erlang:phash2(NewData, 4294967296),
            maps:put(Name, Hash, Acc);
        removed ->
            maps:remove(Name, Acc);
        {error, Error} ->
            lager:notice("Could not load group ~s: ~p", [Name1, Error]),
            maps:remove(Name, Acc)
    end,
    do_load_groups(GroupId, Rest, Opts, Acc1).



%% @private
do_remove_group(GroupId, Name) ->
    Name1 = list_to_binary([Name, ".", GroupId]),
    case nkdomain_obj:remove_obj(group, Name1) of
        ok -> 
            ok;
        {error, Error} ->
            lager:warning("Could not remove group ~s: ~p", [Name1, Error])
    end.





