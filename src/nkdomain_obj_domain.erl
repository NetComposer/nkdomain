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

%% @doc Domain Object
%% Users are stored as user@users.domain, etc.

-module(nkdomain_obj_domain).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(nkdomain_obj).

-export([init/2, load/4, removed/2, export/2, handle_call/4]).

-include("nkdomain.hrl").


-type domain_status() :: ready | standby | stopping | stopped.

%% Internal object
-type domain() ::
    nkdomain_obj:base_opt() |
    #{
        status => domain_status(),
        alias => [binary()],
        groups => [{nkdomain:obj_id(), integer()}],
        users => [{nkdomain:obj_id(), integer()}],
        nodesets => [{nkdomain:obj_id(), integer()}],
        services => [{nkdomain:obj_id(), integer()}]
    }.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(state, {
    id :: nkdomain:obj_id(),
    status :: nkdomain:domain_status()
}).

%% @private
-spec init(nkdomain:obj_id(), domain()) ->
    {ok, nkdomain_obj:init_opts(), domain(), #state{}}.

init(DomainId, Domain) ->
    Base = #{
        groups => #{},
        users => #{},
        nodesets => #{},
        services => #{},
        alias => []
    },
    Domain1 = maps:merge(Base, Domain),
    {ok, #{}, Domain1, #state{id=DomainId, status=ready}}.


%% @private
-spec load(map(), nkdomain_load:load_opts(), nkdomain:obj(), #state{}) ->
    {ok, nkdomain:obj(), #state{}} | removed | {error, #state{}}.

load(Data, Opts, Domain, #state{id=_DomainId}=State) ->
    try
        % case get_father(DomainId) of
        %     none ->
        %         case maps:get(token, Opts, none) of
        %             admin -> ok;
        %             _ -> throw(not_allowed_to_update_root)
        %         end;
        %     Father ->
        %         _FatherPid = case nkdomain_obj:get_pid(?O_DOMAIN, Father) of
        %             {ok, FatherPid0} -> FatherPid0;
        %             {error, Error} -> throw(Error)
        %         end,
        %         case Opts of
        %             #{token:=admin} -> ok;
        %             #{token:=_Token} -> ok; % check token
        %             _ -> throw(not_allowed)
        %         end
        % end,
        % Put users first, they probably define aliases
        List = maps:to_list(Data),
        Load = case Data of
            #{users:=Users} ->
                %% Process users first
                [{users, Users}|lists:keydelete(users, 1, List)];
            _ ->
                List
        end,
        do_load(Load, Opts, Domain, State)
    catch
        throw:Throw -> {error, Throw, State}
    end.



%% @private
-spec removed(domain(), #state{}) ->
    ok | {error, term()}.

removed(Domain, State) ->
    RemoveData = [
        {alias, []}, 
        {groups, #{}}, 
        {users, #{}}, 
        {nodesets, #{}}, 
        {services, #{}}
    ],
    do_load(RemoveData, #{replace=>true}, Domain, State),
    ok.


%% @doc
-spec export(nkdomain:obj_id(), domain()) ->
    map().

export(DomainId, Domain) ->
    lists:foldl(
        fun(Key, Acc) -> export_key(Key, DomainId, Acc) end,
        Domain, 
        [groups, users, nodesets, services]).


%% @private
-spec handle_call(term(), {pid(), term()}, nkdomain:user(), #state{}) ->
    {noreply, tuple()} | {stop, term(), #state{}}.

%% Called from nkdomain_orphans
handle_call({check_orphan, domain, _}, _From, _Domain, State) ->
    {reply, {ok, true}, State};

handle_call({check_orphan, Class, Name}, _From, Domain, State)
        when Class==group; Class==user; Class==service; Class==nodeset ->
    Key = get_key(Class),
    Data = maps:get(Key, Domain, []),
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

% -spec get_father(nkdomain:domain_name()) ->
%     none | nkdomain:domain_name().

% get_father(Domain) ->
%     case binary:split(Domain, <<".">>, [global]) of
%         [<<"root">>] ->
%             none;
%         [_] ->
%             <<"root">>;
%         [_|Rest] ->
%             case lists:reverse(Rest) of
%                 [<<"root">>] -> <<"root">>;
%                 [<<"root">>|Rest2] -> bjoin(lists:reverse(Rest2));
%                 Rest2 -> bjoin(lists:reverse(Rest2))
%             end
%     end.

% %% @private
% bjoin(Terms) ->
%     nklib_util:bjoin(Terms, <<".">>).


%% @private
-spec do_load(list(), nkdomain_load:load_opts(), domain(), #state{}) ->
    {ok, domain(), #state{}} | removed.

do_load([], _Opts, Domain, State) ->
    {ok, Domain, State};

do_load([{status, Status}|Rest], Opts, Domain, #state{id=DomainId}=State) ->
    State1 = update_status(DomainId, Status, State),
    do_load(Rest, Opts, Domain#{status=>Status}, State1);

do_load([{Key, Data}|Rest], Opts, Domain, State) 
        when Key==groups; Key==users; Key==nodesets; Key==services ->
    Domain1 = load_class(Key, Data, Domain, Opts, State),
    do_load(Rest, Opts, Domain1, State);

do_load([{alias, Aliases}|Rest], Opts, Domain, #state{id=DomainId}=State) ->
    Aliases1 = do_load_aliases(DomainId, Aliases, []),
    DomainAliases = maps:get(alias, Domain, []),
    Aliases2 = case Opts of
        #{replace:=true} ->
            do_remove_aliases(DomainId, DomainAliases -- Aliases1),
            lists:usort(Aliases1);
        _ ->
            lists:usort(DomainAliases++Aliases1)
    end,
    do_load(Rest, Opts, Domain#{alias=>Aliases2}, State);

do_load([{Key, Val}|Rest], Opts, Domain, State) ->
    do_load(Rest, Opts, maps:put(Key, Val, Domain), State).


%% @private
load_class(Key, Data, Domain, Opts, #state{id=DomainId}) ->
    Ids1 = maps:get(Key, Domain, []),
    Ids2 = do_load_class(Key, DomainId, maps:to_list(Data), Opts, Ids1),
    Ids3 = case Opts of
        #{replace:=true} ->
            maps:fold(
                fun(Id, Hash, Acc) ->
                    case maps:is_key(Id, Data) of
                        true -> 
                            maps:put(Id, Hash, Acc);
                        false ->
                            do_remove_class(Key, DomainId, Id),
                            Acc
                    end
                end,
                #{},
                Ids2);
        _ ->
            Ids2
    end,
    maps:put(Key, Ids3, Domain).


%% @private
update_status(_DomainId, Status, #state{status=Status}=State) ->
    State;

update_status(DomainId, NewStatus, #state{status=OldStatus}=State) ->
    lager:notice("Domain '~s' updating status from '~p' to '~p'", 
                 [DomainId, OldStatus, NewStatus]),
    State#state{status=NewStatus}.


%% @private
do_load_class(_Key, _DomainId, [], _Opts, Acc) ->
    Acc;

do_load_class(Key, DomainId, [{Name, Data}|Rest], Opts, Acc) ->
    Class = get_class(Key),
    ObjId = list_to_binary([Name, "@", DomainId]),
    Acc1 = case nkdomain_obj:load(Class, ObjId, Data, Opts) of
        not_modified ->
            Acc;
        {loaded, NewData} ->
            % We stored the hash to detect changes
            Hash = erlang:phash2(NewData, 4294967296),
            maps:put(Name, Hash, Acc);
        removed ->
            maps:remove(Name, Acc);
        {error, Error} ->
            lager:notice("Could not load ~p ~s: ~p", [Key, ObjId, Error]),
            maps:remove(Name, Acc)
    end,
    do_load_class(Key, DomainId, Rest, Opts, Acc1).


%% @private
do_remove_class(Key, DomainId, Name) ->
    Class = get_class(Key),
    ObjId = list_to_binary([Name, "@", DomainId]),
    case nkdomain_obj:remove_obj(Class, ObjId) of
        ok -> 
            ok;
        {error, Error} ->
            lager:error("Could not remove ~p ~s: ~p", [Key, ObjId, Error])
    end.


%% @private
do_load_aliases(_DomainId, [], Acc) ->
    lists:reverse(Acc);

do_load_aliases(DomainId, [Alias|Rest], Acc) ->
    Acc1 = case nkdomain_obj_alias:add_alias(Alias, <<"domain:", DomainId/binary>>) of
        ok ->
            [Alias|Acc];
        {error, Error} ->
            lager:warning("Could not load alias ~s: ~p", [Alias, Error]),
            Acc
    end,
    do_load_aliases(DomainId, Rest, Acc1).

%% @private
do_remove_aliases(_DomainId, []) ->
    ok;

do_remove_aliases(DomainId, [Alias|Rest]) ->
    case nkdomain_obj_alias:remove_alias(Alias, <<"domain:", DomainId/binary>>) of
        ok ->
            ok;
        {error, Error} ->
            lager:warning("Could not remove alias ~s: ~p", [Alias, Error])
    end,
    do_remove_aliases(DomainId, Rest).



%% @private
export_key(Key, DomainId, Domain) ->
    Data = maps:fold(
        fun(Id, _Hash, Acc) ->
            Class = get_class(Key),
            ObjId = list_to_binary([Id, "@", DomainId]),
            case nkdomain_obj:export(Class, ObjId) of
                {ok, Map} ->
                    maps:put(Id, Map, Acc);
                {error, Error} ->
                    lager:notice("Could not read ~p ~s: ~p", [Class, ObjId, Error]),
                    Acc
            end
        end,
        #{},
        maps:get(Key, Domain, #{})),
    case maps:size(Data) of
        0 -> Domain;
        _ -> maps:put(Key, Data, Domain)
    end.


%% @private
get_class(groups) -> group;
get_class(users) -> user;
get_class(services) -> service;
get_class(nodesets) -> nodeset.

%% @private
get_key(group) -> groups;
get_key(user) -> users;
get_key(service) -> services;
get_key(nodeset) -> nodesets.






