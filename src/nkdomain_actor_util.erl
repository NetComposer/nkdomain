%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain Actor utilities
-module(nkdomain_actor_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([pre_create/2, create/2]).
-export([event/2, api_event/2]).
-export([get_config/2, get_config/3, find_resource/3]).
-export([find_and_sync_op/5, find_and_sync_op/6]).
-export([add_link/3, link_type/2, link_type_extra/3]).
-export([add_labels/4]).
-export([get_public_self/3]).
-export([callback_syntax/0, http_callback/4]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN Actor: "++Txt, Args)).




%% ===================================================================
%% Actor utilities
%% ===================================================================

%% @doc
%% Links not expanded
pre_create(SrvId, Actor) ->
    try
        #actor{id=ActorId} = Actor,
        #actor_id{group=Group, vsn=Vsn, resource=Res} = ActorId,
        Config = case get_config(SrvId, Group, Res) of
            {ok, Config0} ->
                Config0;
            {error, ConfigError} ->
                throw({error, ConfigError})
        end,
        Actor2 = case nkdomain_api_lib:add_domain_link(SrvId, Actor) of
            {ok, ActorWithDomainLink} ->
                ActorWithDomainLink;
            {error, DomainLinksError} ->
                throw({error, DomainLinksError})
        end,
        #actor{metadata=Meta} = Actor2,
        MetaSyntax = nkservice_actor_syntax:meta_syntax(),
        Meta2 = case nklib_syntax:parse(Meta, MetaSyntax, #{path=><<"metadata">>}) of
            {ok, ParsedMeta, _} ->
                ParsedMeta;
            {error, Error} ->
                throw({error, Error})
        end,
        Actor3 = Actor2#actor{metadata=Meta2},
        ApiReq = #{
            verb => create,
            group => Group,
            vsn => Vsn,
            resource => Res,
            subresource => [],
            params => #{}
        },
        Actor4 = nkservice_actor_util:put_create_fields(Actor3),
        Actor5 = case nkdomain_actor:parse(SrvId, Actor4, Config, ApiReq) of
            {ok, ActorParsed} ->
                % Kind is added now
                ActorParsed;
            {error, ParseError} ->
                throw({error, ParseError})
        end,
        Actor6 = nkdomain_api_lib:expand_api_links(Actor5),
        {ok, Actor6, Config}
    catch
        throw:Throw ->
            Throw
    end.


%% @doc
create(SrvId, Actor) ->
    case pre_create(SrvId, Actor) of
        {ok, Actor2, Config} ->
            nkservice_actor_db:create(SrvId, Actor2, #{actor_config=>Config});
        {error, Error} ->
            {error, Error}
    end.


%% @doc Generates and sends a in-actor erlang event
-spec event(nkservice_actor:event(), #actor_st{}) ->
    #actor_st{}.

event(Event, ActorSt) ->
    nkservice_actor_srv:do_event(Event, ActorSt).


%% @doc Generates and sends a in-actor API event
-spec api_event(nkdomain_api:api_event(), #actor_st{}) ->
    #actor_st{}.

api_event(ApiEvent, #actor_st{srv=SrvId, actor=Actor}=ActorSt) ->
    EvActor = nkdomain_api_events:make_event(SrvId, ApiEvent, Actor),
    nkdomain_api_events:send_event_st(EvActor, ActorSt).


%% @doc Gets the actor config
get_config(SrvId, #actor_id{group=Group, resource=Resource}) ->
    get_config(SrvId, Group, Resource).


%% @doc Gets the actor config
get_config(SrvId, Group, Resource) ->
    case catch nkdomain_plugin:get_resource_config(SrvId, Group, Resource) of
        Map when is_map(Map) ->
            {ok, Map};
        {'EXIT', _} ->
            {error, {resource_invalid, Group, Resource}}
    end.

%%
%%%% @private
%%-spec parse_actor(#actor{}, nklib_syntax:syntax(), nkdomain_api:request()) ->
%%    {ok, #actor{}} | nklib_syntax:error().
%%
%%parse_actor(#actor{id=ActorId, data=Data}=Actor, Syntax, #{vsn:=Vsn}) ->
%%    case parse(Data, Syntax) of
%%        {ok, Data2} ->
%%            {ok, Actor#actor{data=Data2, id=ActorId#actor_id{vsn=Vsn}}};
%%        {error, Error} ->
%%            {error, Error}
%%    end.

%%
%%%% @private
%%-spec parse(map(), nklib_syntax:syntax()) ->
%%    {ok, map()} | nklib_syntax:error().
%%
%%parse(Data, Syntax) ->
%%    % lager:error("NKLOG SYN Data:~p\n Syntax:~p", [Data, Syntax]),
%%    case nklib_syntax:parse(Data, Syntax#{<<"kind">>=>binary}) of
%%        {ok, Data2, []} ->
%%            {ok, Data2};
%%        {ok, _, [Field | _]} ->
%%            {error, {field_unknown, Field}};
%%        {error, {syntax_error, Field}} ->
%%            % lager:error("NKLOG Data ~p Syntax ~p", [Data, Syntax]),
%%            {error, {field_invalid, Field}};
%%        {error, {field_missing, Field}} ->
%%            {error, {field_missing, Field}};
%%        {error, Error} ->
%%            lager:error("Unexpected parse error at ~p: ~p", [?MODULE, Error]),
%%            {error, Error}
%%    end.


%% @private
find_and_sync_op(SrvId, Id, Group, Type, Op) ->
    find_and_sync_op(SrvId, Id, Group, Type, Op, 5000).


%% @private
find_and_sync_op(SrvId, Id, FoundGroup, FoundRes, Op, Timeout) ->
    Path = nkdomain_api_lib:api_path_to_actor_path(Id),
    case nkservice_actor:activate({SrvId, Path}) of
        {ok, #actor_id{group=FoundGroup, resource=FoundRes}=ActorId, _} ->
            nkservice_actor_srv:sync_op({SrvId, ActorId}, Op, Timeout);
        {ok, _, _} ->
            {error, actor_not_found};


%%        {ok, #actor_id{group=FoundGroup, resource=FoundRes2}=ActorId, _} when is_list(FoundRes) ->
%%            case lists:member(FoundRes2, FoundRes) of
%%                true ->
%%                    nkservice_actor_srv:sync_op({SrvId, ActorId}, Op, Timeout);
%%                false ->
%%                    {error, actor_not_found}
%%            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Gets the type for an alternative type id
-spec find_resource(nkservice:id(), nkservice_actor:group(), binary()) ->
    {singular|camel|short_name, nkservice_actor:resource()} | undefined.

%% @private
find_resource(SrvId, Group, Id) ->
    nkdomain_plugin:find_resource(SrvId, Group, Id).


%% @private
add_link(#actor_id{uid=UID}, LinkType, Actor) ->
    add_link(UID, LinkType, Actor);

add_link(UID, LinkType, #actor{metadata=Meta}=Actor) when is_binary(UID), UID /= <<>> ->
    Links1 = maps:get(<<"links">>, Meta, #{}),
    Links2 = Links1#{UID => LinkType},
    Actor#actor{metadata=Meta#{<<"links">>=>Links2}}.


%% @doc
link_type(Group, Resource) ->
    <<"io.netc.", Group/binary, $., Resource/binary>>.


%% @doc
link_type_extra(Group, Resource, Pos) ->
    link_type(Group, <<Resource/binary, $., (nklib_util:to_binary(Pos))/binary>>).


%% @doc
add_labels(Prefix, List, Value, #actor{metadata=Meta}=Actor) ->
    Labels1 = maps:get(<<"labels">>, Meta, #{}),
    Labels2 = lists:foldl(
        fun(Term, Acc) ->
            Key = list_to_binary([Prefix, Term]),
            Acc#{Key => to_bin(Value)}
        end,
        Labels1,
        List),
    Meta2 = Meta#{<<"labels">> => Labels2},
    Actor#actor{metadata=Meta2}.


 %% @doc
get_public_self(#actor_id{domain=Domain}=ActorId, Vsn, ExtUrl) ->
    #actor_id{group=Group, resource=Res, name=Name} = ActorId,
    <<
        ExtUrl/binary, "/apis/", Group/binary, "/", Vsn/binary, "/domains/", Domain/binary,
        $/, Res/binary, $/, Name/binary
    >>.


%% @doc
callback_syntax() ->
    #{
        <<"url">> => binary,
        <<"method">> => lower,
        <<"insecure">> => boolean,
        <<"redirects">> => {integer, 0, 10},
        '__mandatory' => [<<"url">>],
        '__defaults' => #{
            <<"method">> => <<"post">>,
            <<"insecure">> => false,
            <<"redirects">> => 0
        }
    }.



%% @doc
-spec http_callback(#actor_id{}, map(), [{binary(), binary()}], map()|binary()) ->
    {ok, integer(), [{binary(), binary()}]} | {error, term()}.

http_callback(ActorId, Callback, Hds, Body) ->
    case nklib_syntax:parse(Callback, callback_syntax()) of
        {ok, Parsed, _} ->
            #{
                <<"url">> := Url,
                <<"method">> := Method,
                <<"insecure">> := Insecure,
                <<"redirects">> := Redirects
            } = Parsed,
            #actor_id{domain = Domain} = ActorId,
            {Hds2, Body2} = case is_map(Body) of
                true ->
                    {
                        [{<<"content-type">>, <<"application/json">>}|Hds],
                        nklib_json:encode(Body)
                    };
                false ->
                    {Hds, Body}
            end,
            Opts1 = [{pool, Domain}, {insecure, Insecure}],
            Opts2 = case Redirects of
                0 ->
                    Opts1;
                _ ->
                    [{follow_redirect, true}, {max_redirect, Redirects}|Opts1]
            end,
            case hackney:request(Method, Url, Hds2, Body2, Opts2) of
                {ok, StatusCode, RespHeaders, ClientRef} ->
                    hackney:skip_body(ClientRef),
                    {ok, StatusCode, RespHeaders};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.
            

%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
