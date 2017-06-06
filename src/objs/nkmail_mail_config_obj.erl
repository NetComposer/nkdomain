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

%% @doc Config Object

-module(nkmail_mail_config_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/3, load_providers/0]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3]).

-include("nkdomain.hrl").
-include_lib("nkmail/include/nkmail.hrl").
-include_lib("nkapi/include/nkapi.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkMAIL Config "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================


%% @doc
-spec create(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, Name, Obj) ->
    nkdomain_obj_lib:make_and_create(Srv, Name, Obj, #{}).
%%    Provider2 = Provider#{id=>Name},
%%    case nkmail:parse_provider(Srv, Provider2) of
%%        {ok, Provider3} ->
%%            Opts = #{
%%                obj_name => Name,
%%                type_obj => Provider3,
%%                subtype => <<"config">>
%%            },
%%            nkdomain_obj_lib:make_and_create(Srv, Parent, ?DOMAIN_MAIL_CONFIG, Opts);
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @doc
-spec load_providers() ->
    ok.

load_providers() ->
    ProvIds = nkmail_app:get_provider_ids(),
    lists:foreach(
        fun(Id) ->
            Provider = nkmail_app:get_provider(Id),
            case create(root, Id, #{type=>?DOMAIN_CONFIG, ?MAIL_CONFIG=>Provider}) of
                {ok, ObjId, _Path, _Pid} ->
                    ?LLOG(info, "Loaded provider ~s (~s)", [Id, ObjId]);
                {error, Error} ->
                    ?LLOG(warning, "Could not load provider ~s: ~p", [Id, Error])
            end
        end,
        ProvIds).






%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_CONFIG,
        subtype => ?MAIL_CONFIG
    }.


%% @private
object_mapping() ->
    disabled.


%% @private
object_parse(SrvId, _Mode, Obj) ->
    #{path:=Path, ?MAIL_CONFIG:=Config} = Obj,
    case nkmail:parse_provider(SrvId, Config#{id=>Path}) of
        {ok, Provider} ->
            {type_obj, Provider};
        {error, Error} ->
            {error, Error}
    end.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?MAIL_CONFIG, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


object_api_cmd(Cmd, Req, State) ->
    nkdomain_obj_api:api(Cmd, ?MAIL_CONFIG, Req, State).





