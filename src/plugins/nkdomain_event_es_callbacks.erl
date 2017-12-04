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

%% @doc Elasticsearch Event plugin
-module(nkdomain_event_es_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([error/1]).
-export([object_db_init/1, object_db_event_send/1]).
-export([object_db_event_search/1]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(_) -> continue.





%% ===================================================================
%% Offered callbacks
%% ===================================================================


object_db_event_search(Spec) ->
    case nkdomain_event_es_util:get_opts() of
        {ok, EsOpts} ->
            nkdomain_event_es_util:db_search(Spec, EsOpts);
        _ ->
            continue
    end.



%% ===================================================================
%% Implemented callbacks
%% ===================================================================

%% @doc Initializes database
-spec object_db_init(nkservice:state()) ->
    {ok, nkservice:state()} | {error, term()}.

object_db_init(State) ->
    case nkdomain_event_es_util:get_index_opts() of
        {ok, IndexOpts, EsOpts} ->
            case nkdomain_event_es_util:db_init(IndexOpts, EsOpts) of
                ok ->
                    {continue, [State]};
                {error, Error} ->
                    {error, {object_db_init, Error}}
            end;
        _ ->
            continue
    end.


%% @doc Sends an event to DB
-spec object_db_event_send(nkevent:event()) ->
    ok | continue.

object_db_event_send(Event) ->
    case nkdomain_event_es_util:get_opts() of
        {ok, EsOpts} ->
            spawn_link(
                fun() ->
                    case nkdomain_event_es_util:db_save(Event, EsOpts) of
                        {ok, _EventId, _Meta} ->
                            % ?LLOG(info, "sent event ~s ~p", [EventId, Meta]),
                            ok;
                        {error, Error} ->
                            ?LLOG(notice, "could not send event: ~p", [Error]),
                            ok
                    end
                end),
            ok;
        _ ->
            continue
    end.




%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).
