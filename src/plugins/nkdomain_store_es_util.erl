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

%% @doc Elasticsearch plugin
-module(nkdomain_store_es_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_opts/0, get_index_opts/0, db_init/2, normalize/1, normalize_multi/1]).
-export([reload_index/1, delete_index/0]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_opts() ->
    case get_index_opts() of
        {ok, _IndexOpts, EsOpts} ->
            {ok, EsOpts};
        _ ->
            not_found
    end.


%% @doc
get_index_opts() ->
    case ?CALL_NKROOT(config_nkdomain_nkroot, []) of
        #nkdomain_config_cache{db_store={elastic, IndexOpts, EsOpts}} ->
            {ok, IndexOpts, EsOpts};
        _ ->
            not_found
    end.


%% @doc
db_init(IndexOpts, EsOpts) ->
    case nkelastic:update_or_create_index(IndexOpts, EsOpts) of
        {ok, _} ->
            db_init_mappings(EsOpts);
        {error, Error} ->
            {error, {create_index, Error}}
    end.


%% @doc
%% TODO: each service could have their own type
db_init_mappings(EsOpts) ->
    Modules = nkdomain_all_types:get_all_modules(),
    Base = ?CALL_NKROOT(object_es_mapping, []),
    Mappings = do_get_mappings(Modules, Base),
    case nkelastic:add_mapping(Mappings, EsOpts) of
        {ok, _} ->
            db_init_root(EsOpts);
        {error, Error} ->
            {error, {update_mappings, Error}}
    end.


%% @private
do_get_mappings([], Acc) ->
    Acc;

do_get_mappings([Module|Rest], Acc) ->
    Mapping = case ?CALL_NKROOT(object_es_mapping, [Module]) of
        not_exported ->
            #{enabled => false};
        not_indexed ->
            #{enabled => false};
        Map when is_map(Map) ->
            #{
                type => object,
                dynamic => false,
                properties => Map
            }
    end,
    #{type:=Type} = Module:object_info(),
    do_get_mappings(Rest, Acc#{Type => Mapping}).


%% @private
db_init_root(EsOpts) ->
    case nkelastic:get(<<"root">>, EsOpts) of
        {ok, #{<<"type">>:=?DOMAIN_DOMAIN}, _} ->
            db_init_admin(EsOpts);
        {error, object_not_found} ->
            Now = nkdomain_util:timestamp(),
            Obj = #{
                type => ?DOMAIN_DOMAIN,
                obj_id => <<"root">>,
                path => <<"/">>,
                srv_id => <<>>,
                obj_name => <<>>,
                domain_id => <<>>,
                parent_id => <<>>,
                description => <<"NetComposer">>,
                created_time => Now,
                created_by => <<"admin">>,
                updated_time => Now,
                updated_by => <<"admin">>,
                ?DOMAIN_DOMAIN => #{}
            },
            {ok, _} = nkelastic:put(<<"root">>, Obj, EsOpts),
            ?LLOG(warning, "created ROOT domain", []),
            db_init_admin(EsOpts);
        {error, Error} ->
            {error, {object_create, Error}}
    end.


%% @private
db_init_admin(EsOpts) ->
    case nkelastic:get(<<"admin">>, EsOpts) of
        {ok, #{<<"type">>:=?DOMAIN_USER}, _} ->
            ok;
        {error, object_not_found} ->
            Now = nkdomain_util:timestamp(),
            Obj = #{
                type => ?DOMAIN_USER,
                obj_id => <<"admin">>,
                path => <<"/users/admin">>,
                srv_id => <<>>,
                obj_name => <<"admin">>,
                domain_id => <<"root">>,
                parent_id => <<"root">>,
                description => <<"Admin User">>,
                created_time => Now,
                created_by => <<"admin">>,
                updated_time => Now,
                updated_by => <<"admin">>,
                ?DOMAIN_USER => #{
                    name => <<"Admin">>,
                    surname => <<"User">>,
                    password => nkdomain_user_obj:user_pass("netcomposer")
                }
            },
            {ok, _} = nkelastic:put(<<"admin">>, Obj, EsOpts),
            ?LLOG(warning, "created ADMIN user. Password is 'netcomposer'. Change it NOW", []),
            ok;
        {error, Error} ->
            {error, {object_create, Error}}
    end.


%% @private
normalize(Text) ->
    nklib_parse:normalize(Text, #{unrecognized=>keep}).


%% @doc
normalize_multi(Text) ->
    norm_multi(nklib_util:to_list(Text), [], []).


%% @private
norm_multi([Ch|Rest], Chars, Words) when Ch==32; Ch==$-; Ch==$_; Ch==$/;
                                        Ch==$(; Ch==$); Ch==$,; Ch==$;; Ch==$: ->
    case Chars of
        [] ->
            norm_multi(Rest, [], Words);
        _ ->
            Word = normalize(lists:reverse(Chars)),
            norm_multi(Rest, [], [Word|Words])
    end;

norm_multi([Ch|Rest], Chars, Words) ->
    norm_multi(Rest, [Ch|Chars], Words);

norm_multi([], Chars, Words) ->
    case Chars of
        [] ->
            lists:reverse(Words);
        _ ->
            Word = normalize(lists:reverse(Chars)),
            lists:reverse([Word|Words])
    end.


%% @doc
reload_index(SrvId) ->
    {ok, _} = SrvId:object_db_init(#{id=>SrvId}),
    ok.


%% @doc CAUTION!
delete_index() ->
    {ok, EsOpts} = nkdomain_store_es_util:get_opts(),
    nkelastic:delete_index(EsOpts).



%% ===================================================================
%% Public
%% ===================================================================


%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).
