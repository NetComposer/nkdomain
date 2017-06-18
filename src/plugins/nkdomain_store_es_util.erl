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

-export([get_opts/1, reload/1, db_init/2]).

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Store ES "++Txt, Args)).

-include("nkdomain.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
get_opts(SrvId) ->
    case SrvId:config_nkdomain() of
        {elastic, _IndexOpts, EsOpts} -> {ok, EsOpts};
        _ -> not_found
    end.


%% @doc
reload(SrvId) ->
    {elastic, IndexOpts, EsOpts} = SrvId:config_nkdomain(),
    db_init(IndexOpts, EsOpts).



%% @doc
db_init(IndexOpts, Opts) ->
    case nkelastic:update_or_create_index(IndexOpts, Opts) of
        {ok, _} ->
            db_init_mappings(Opts);
        {error, Error} ->
            {error, {create_index, Error}}
    end.


%% @doc
db_init_mappings(#{srv_id:=SrvId}=Opts) ->
    Modules = nkdomain_all_types:get_all_modules(),
    Base = SrvId:object_es_mapping(),
    Mappings = do_get_mappings(SrvId, Modules, Base),
    case nkelastic:add_mapping(Mappings, Opts) of
        {ok, _} ->
            db_init_root(Opts);
        {error, Error} ->
            {error, {update_mappings, Error}}
    end.


%% @private
do_get_mappings(_SrvId, [], Acc) ->
    Acc;

do_get_mappings(SrvId, [Module|Rest], Acc) ->
    #{type:=Type} = Module:object_info(),
    Mapping = case SrvId:object_es_mapping(SrvId, Type) of
        not_exported ->
            #{enabled => false};
        Map when is_map(Map) ->
            #{
                type => object,
                dynamic => false,
                properties => Map
            }
    end,
    do_get_mappings(SrvId, Rest, Acc#{Type => Mapping}).


%% @private
db_init_root(Opts) ->
    case nkelastic:get(<<"root">>, Opts) of
        {ok, #{<<"type">>:=?DOMAIN_DOMAIN}, _} ->
            db_init_admin(Opts);
        {error, object_not_found} ->
            Now = nkdomain_util:timestamp(),
            Obj = #{
                type => ?DOMAIN_DOMAIN,
                obj_id => <<"root">>,
                path => <<"/">>,
                parent_id => <<>>,
                description => <<"NetComposer">>,
                created_time => Now,
                created_by => <<"admin">>,
                updated_time => Now,
                updated_by => <<"admin">>
            },
            {ok, _} = nkelastic:put(<<"root">>, Obj, Opts),
            ?LLOG(warning, "created ROOT domain", []),
            db_init_admin(Opts);
        {error, Error} ->
            {error, {object_create, Error}}
    end.


%% @private
db_init_admin(#{srv_id:=SrvId}=Opts) ->
    case nkelastic:get(<<"admin">>, Opts) of
        {ok, #{<<"type">>:=?DOMAIN_USER}, _} ->
            ok;
        {error, object_not_found} ->
            Opts = #{
                type => ?DOMAIN_USER,
                parent_id => <<"root">>,
                created_by => <<"admin">>,
                obj_name => <<"admin">>,
                obj_id => <<"admin">>,
                ?DOMAIN_USER => #{
                    name => <<"Admin">>,
                    surname => <<"User">>,
                    password => nkdomain_user_obj:user_pass("netcomposer")
                }
            },
            {ok, _, _} = nkdomain_obj_make:make_and_create(SrvId, Opts, #{}),
            ?LLOG(warning, "created ADMIN user. Password is 'netcomposer'. Change it NOW", []),
            ok;
        {error, Error} ->
            {error, {object_create, Error}}
    end.



%% ===================================================================
%% Public
%% ===================================================================


%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).
