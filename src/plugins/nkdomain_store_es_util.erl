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

-export([get_opts/0, get_index_opts/0, reload_index/0, delete_index/0, read_obj/1]).
-export([domain_filter/2, get_obj_id/1, get_path/1]).
-export([base_mappings/0, unparse/1]).
-export([db_init/2, normalize/1, normalize_multi/1]).
-export([print_index/0, put1/0, get1/0]).

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
            IndexOpts2 = IndexOpts#{
                'mapper.dynamic' => false
            },
            {ok, IndexOpts2, EsOpts};
        _ ->
            not_found
    end.


reload_index() ->
    {ok, _} = ?CALL_NKROOT(object_db_init, [#{id=>?NKROOT}]),
    ok.


%% @doc CAUTION! CAUTION!
delete_index() ->
    {ok, EsOpts} = get_opts(),
    nkelastic:delete_index(EsOpts).


%% @doc
read_obj(Id) ->
    {ok, E} = get_opts(),
    nkelastic:get(to_bin(Id), E).


%% @private Checks for 'deep' option and uses path or domain_id
domain_filter(Id, Opts) ->
    case Opts of
        #{deep:=true} ->
            case get_path(Id) of
                {ok, Path} ->
                    {ok, [{path, subdir, Path}]};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            case get_obj_id(Id) of
                {ok, ObjId} ->
                    {ok, [{domain_id, eq, ObjId}]};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private Finds obj_id
get_obj_id(Id) ->
    case nkdomain_util:is_path(Id) of
        {true, <<"/">>} ->
            {ok, <<"root">>};
        {true, Path} ->
            case nkdomain_store_es_callbacks:object_db_find_obj(Path, false) of
                {ok, _Type, ObjId, _Path} ->
                    {ok, ObjId};
                {error, Error} ->
                    {error, Error}
            end;
        {false, ObjId} ->
            {ok, ObjId}
    end.


%% @private Finds path
get_path(Id) ->
    case nkdomain_util:is_path(Id) of
        {true, Path} ->
            {ok, Path};
        {false, <<"root">>} ->
            {ok, <<"/">>};
        {false, ObjId} ->
            case nkdomain_store_es_callbacks:object_db_find_obj(ObjId, false) of
                {ok, _Type, _ObjId, Path} ->
                    {ok, Path};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @private
print_index() ->
    {ok, E} = get_opts(),
    {ok, I, _} = nkelastic:get_index(E),
    io:format("\n~s\n", [nklib_json:encode_pretty(I)]).


put1() ->
    {ok, E} = get_opts(),
    {ok, O, _} = nkelastic:get(<<"admin">>, E),
    O2 = O#{<<"fieldXX">> => <<"test">>},
    % Since mapping for global fields is 'strict' it should fail
    {error, {strict_mapping, _}} = nkelastic:put(<<"admin">>, O2, E),
    % Mapping for 'user' fields is 'false' so it is accepted but not mapped
    #{<<"user">> := U1} = O,
    U2 = U1#{<<"fieldYY">> => 1},
    {ok, _} = nkelastic:put(<<"admin">>, O#{<<"user">>:=U2}, E),
    U3 = U1#{<<"fieldYY">> =>  <<"test">>},
    {ok, _} = nkelastic:put(<<"admin">>, O#{<<"user">>:=U3}, E),
    U4 = maps:remove(<<"fieldYY">>, U3),
    {ok, _} = nkelastic:put(<<"admin">>, O#{<<"user">>:=U4}, E),
    ok.


get1() ->
    {ok, E} = get_opts(),
    nkelastic:get(<<"admin">>, E).





%% ===================================================================
%% Syntax
%% ===================================================================


%% @doc ES base base mapping
base_mappings() ->
    #{
        vsn => #{type => keyword},
        obj_id => #{type => keyword},
        srv_id => #{type => keyword},
        type => #{type => keyword},
        path => #{type => keyword},
        obj_name => #{type => keyword},
        domain_id => #{type => keyword},
        parent_id => #{type => keyword},
        subtype => #{type => keyword},
        created_by => #{type => keyword},
        created_time => #{type => date},
        updated_by => #{type => keyword},
        updated_time => #{type => date},
        enabled => #{type => boolean},
        active => #{type => boolean},
        expires_time => #{type => date},
        is_deleted => #{type => boolean},
        deleted_time => #{type => date},        %% TODO Change to date
        roles => #{
            type => object,
            dynamic => false,
            properties => #{
                role => #{type => keyword},
                direct => #{type => keyword},
                indirect => #{
                    type => object,
                    dynamic => false,
                    properties => #{
                        role => #{type => keyword},
                        obj_id => #{type => keyword}
                    }
                }
            }
        },
        name => #{
            type => text,
            analyzer => standard,
            fields => #{keyword => #{type=>keyword}}
        },
        name_norm => #{type=>text},
        description => #{
            type => text,
            analyzer => standard,
            fields => #{keyword => #{type=>keyword}}
        },
        description_norm => #{type=>text},
        tags => #{type => keyword},
        aliases => #{type => keyword},
        icon_id => #{type => keyword},
        next_status_time => #{type => date},
        in_alarm => #{type => boolean},
        alarms => #{
            type => object,
            dynamic => false,
            properties => #{
                reason => #{type => text},
                severity => #{type => keyword},
                time => #{type => date},
                body => #{enabled => false}
           }
        }
    }.



%% @doc Called to serialize an object to ES format
-spec unparse(nkdomain:obj()) ->
    map().

unparse(#{type:=Type}=Obj) ->
    BaseKeys = maps:keys(base_mappings()),
    BaseMap1 = maps:with(BaseKeys, Obj),
    BaseMap2 = case BaseMap1 of
        #{pid:=Pid} ->
            BaseMap1#{pid:=base64:encode(term_to_binary(Pid))};
        _ ->
            BaseMap1
    end,
    BaseMap3 = case BaseMap2 of
        #{name:=Name} ->
            %BaseMap2#{name_norm=>normalize_multi(Name)};
            % Ignore unrecognized characters
            BaseMap2#{name_norm=>normalize_multi(Name, #{unrecognized=>skip})};
        _ ->
            BaseMap2
    end,
    BaseMap4 = case BaseMap3 of
        #{description:=Desc} ->
            BaseMap3#{description_norm=>normalize_multi(Desc, #{unrecognized=>skip})};
        _ ->
            BaseMap3
    end,
    BaseMap5 = case BaseMap4 of
        #{srv_id:=?NKROOT} ->
            maps:remove(srv_id, BaseMap4);
        _ ->
            BaseMap4
    end,
    case nkdomain_util:type_apply(Type, object_es_mapping, []) of
        not_exported ->
            BaseMap5#{Type => #{}};
        not_indexed ->
            ModData = maps:get(Type, Obj, #{}),
            BaseMap5#{Type => ModData};
        Map when is_map(Map) ->
            case nkdomain_util:type_apply(Type, object_es_unparse, [Obj, BaseMap5]) of
                not_exported ->
                    ModData = maps:get(Type, Obj, #{}),
                    ModKeys = maps:keys(Map),
                    ModMap = maps:with(ModKeys, ModData),
                    BaseMap5#{Type => ModMap};
                Value when is_map(Value) ->
                    Value
            end
    end.


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc
db_init(IndexOpts, EsOpts) ->
    case nkelastic:update_or_create_index(IndexOpts, EsOpts) of
        {ok, _} ->
            db_init_mappings(EsOpts);
        {error, Error} ->
            {error, {create_index, Error}}
    end.


%% @doc
db_init_mappings(EsOpts) ->
    Modules = nkdomain_reg:get_all_type_modules(),
    Base = base_mappings(),
    Mappings = do_get_mappings(Modules, Base),
    % io:format("ES Mappings\n~s\n\n", [nklib_json:encode_pretty(Mappings)]),
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
    Mapping = case nkdomain_util:type_apply(Module, object_es_mapping, []) of
        not_exported ->
            #{enabled => false};
        not_indexed ->
            #{enabled => false};
        Map when is_map(Map) ->
            #{
                type => object,
                dynamic => false,
                properties => Map#{
                    vsn => #{type => keyword}
                }
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
                    password => nkdomain_user:user_pass("netcomposer")
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


%% @private
normalize(Text, Opts) ->
    nklib_parse:normalize(Text, Opts).


%% @doc
normalize_multi(Text) ->
    norm_multi(nklib_util:to_list(Text), [], [], #{unrecognized=>keep}).


%% @doc
normalize_multi(Text, Opts) ->
    norm_multi(nklib_util:to_list(Text), [], [], Opts).
    

%% @private
norm_multi([Ch|Rest], Chars, Words, Opts) when Ch==32; Ch==$-; Ch==$_; Ch==$/;
                                        Ch==$(; Ch==$); Ch==$,; Ch==$;; Ch==$: ->
    case Chars of
        [] ->
            norm_multi(Rest, [], Words, Opts);
        _ ->
            Word = normalize(lists:reverse(Chars), Opts),
            norm_multi(Rest, [], [Word|Words], Opts)
    end;

norm_multi([Ch|Rest], Chars, Words, Opts) ->
    norm_multi(Rest, [Ch|Chars], Words, Opts);

norm_multi([], Chars, Words, Opts) ->
    case Chars of
        [] ->
            lists:reverse(Words);
        _ ->
            Word = normalize(lists:reverse(Chars), Opts),
            lists:reverse([Word|Words])
    end.


%%%% @doc
%%stored_srv(<<>>) -> atom_to_binary(?NKROOT, latin1);
%%stored_srv(Srv) -> Srv.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).
