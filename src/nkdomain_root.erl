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

%% @doc Root Domain
-module(nkdomain_root).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/0, start/0, stop/0, admin_create/0]).

-include("nkdomain.hrl").


%% ===================================================================
%% Private
%% ===================================================================

%% @doc
create() ->
    Obj = #{
        type => ?DOMAIN_DOMAIN,
        obj_id => <<"root">>,
        path => <<"/">>,
        parent_id => <<>>,
        description => <<"NetComposer">>,
        created_time => nklib_util:m_timestamp()
    },
    nkdomain_store_es:object_store_save_raw(root, <<"root">>, Obj).



%% @doc Starts the root service
start() ->
    Spec1 = #{
        plugins => [nkdomain, nkapi, nkchat, nkdomain_store_es],
        domain => <<"root">>,
        domain_elastic_url => nkdomain_app:get(elastic_url),
        debug => [
            %% {nkapi_client, #{nkpacket=>true}},
            nkapi_server,
            nkelastic,
            %% {nkelastic, full},
            {nkdomain_obj, all}
        ]
    },
    User = nkdomain_app:get(elastic_user),
    Pass = nkdomain_app:get(elastic_pass),
    Spec2 = case is_binary(User) and is_binary(Pass) of
        true ->
            Spec1#{
                domain_elastic_user => User,
                domain_elastic_pass => Pass
            };
        false ->
            Spec1
    end,
    Spec3 = case nkdomain_app:get(api_server) of
        undefined ->
            Spec2;
        ApiServer ->
            Spec2#{api_server => ApiServer}
    end,
    case nkservice:start(root, Spec3) of
        {ok, _} ->
            lager:info("Root service started");
        {error, Error} ->
            lager:error("Could not start root service: ~p (~p)", [Error, Spec3]),
            error(start_root_error)
    end.


%% @doc
stop() ->
    nkdomain_obj:unload_all(),
    timer:sleep(1000),
    nkservice:stop(root).


%% @doc
admin_create() ->
    Opts = #{
        obj_id => <<"admin">>,
        name => <<"admin">>,
        type_obj => #{
            name => <<"Admin">>,
            surname => <<"User">>,
            password => "1234"
        }
    },
    case nkdomain_obj_lib:make_obj(root, <<"root">>, ?DOMAIN_USER, Opts) of
        {ok, Obj} ->
            case nkdomain_obj_lib:create(root, Obj, #{}) of
                #obj_id_ext{type = ?DOMAIN_USER} ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.
