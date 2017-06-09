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

-module(nkdomain_mail_provider_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/3, load_providers/0, get_provider/2, find/2, delete_all/2]).
-export([object_get_info/0, object_admin_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/2]).

-include("nkdomain.hrl").
-include_lib("nkmail/include/nkmail.hrl").
-include_lib("nkapi/include/nkapi.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkMAIL Provider "++Txt, Args)).


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
    nkdomain_obj_lib:make_and_create(Srv, Name, Obj#{subtype=>?DOMAIN_CONFIG}, #{}).


%% @doc
-spec load_providers() ->
    ok.

load_providers() ->
    ProvIds = nkmail_app:get_provider_ids(),
    lists:foreach(
        fun(Id) ->
            Provider = nkmail_app:get_provider(Id),
            case create(root, Id, #{type=>?DOMAIN_MAIL_PROVIDER, parent_id=>root, ?DOMAIN_MAIL_PROVIDER=>Provider}) of
                {ok, #{obj_id:=ObjId, path:=Path}, _Pid} ->
                    ?LLOG(info, "Loaded provider ~s (~s)", [Path, ObjId]);
                {error, object_already_exists} ->
                    ?LLOG(info, "Provider ~s already loaded", [Id]);
                {error, Error} ->
                    ?LLOG(warning, "Could not load provider ~s: ~p", [Id, Error])
            end
        end,
        ProvIds).


%% @doc
get_provider(SrvId, Id) ->
    case nkdomain_obj_lib:load(SrvId, Id, #{}) of
        #obj_id_ext{pid=Pid} ->
            case nkdomain_obj:get_obj(Pid) of
                {ok, #{?DOMAIN_MAIL_PROVIDER:=Provider}} ->
                    {ok, Provider};
                {error, _} ->
                    continue
            end;
        _ ->
            continue
    end.

%% @private
find(SrvId, Root) ->
    nkdomain_domain_obj:find(SrvId, Root, #{filters=>#{type=>?DOMAIN_MAIL_PROVIDER}}).


%% @private
delete_all(SrvId, Root) ->
    nkdomain_store:delete_all_childs_type(SrvId, Root, ?DOMAIN_MAIL_PROVIDER).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_MAIL_PROVIDER,
        subtype => [?DOMAIN_CONFIG]
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 9000,
        tree_id => <<"domain_tree_resources_mail.providers">>
    }.


%% @private
object_mapping() ->
    disabled.


%% @private
object_parse(SrvId, _Mode, Obj) ->
    #{?DOMAIN_MAIL_PROVIDER:=Config} = Obj,
    case nkmail:parse_provider(SrvId, Config, #{path=>?DOMAIN_MAIL_PROVIDER}) of
        {ok, Provider, UnknownFields} ->
            {type_obj, Provider, UnknownFields};
        {error, Error} ->
            {error, Error}
    end.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_MAIL_PROVIDER, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


object_api_cmd(Cmd, Req) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_MAIL_PROVIDER, Req).





