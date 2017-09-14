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

-module(nkdomain_mail_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([object_info/0, object_admin_info/0, object_parse/2, object_es_mapping/0,
         object_api_syntax/2, object_api_cmd/2]).

-include_lib("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Mail "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?DOMAIN_MAIL
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 8500
    }.



%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword}
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
        vsn => binary,
        '__defaults' => #{vsn => <<"1">>}
    }.


%% @private
object_api_syntax(<<"send">>, Syntax) ->
    MailSyntax = nkmail_util:msg_syntax(),
    maps:merge(Syntax, MailSyntax);

object_api_syntax(_Cmd, Syntax) ->
    Syntax.


%% @private
object_api_cmd(<<"send">>, #nkreq{data=Data}) ->
    case get_provider(Data) of
        {ok, _, Provider} ->
            case nkmail:send(?NKROOT, Provider, Data) of
                {ok, Meta} ->
                    {ok, #{result=>Meta}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

object_api_cmd(_Cmd, _Req) ->
    {error, not_implemented2}.




%% ===================================================================
%% Internal
%% ===================================================================



%% @private
get_provider(#{provider_id:=ProviderId}) ->
    do_get_provider(ProviderId);

get_provider(_Obj) ->
    case ?CALL_NKROOT(config_nkdomain_nkroot, []) of
        #nkdomain_config_cache{email_provider=ProviderId} ->
            do_get_provider(ProviderId);
        _ ->
            {error, provider_id_missing}
    end.


%% @private
do_get_provider(ProviderId) ->
    case nkdomain_lib:load(ProviderId) of
        #obj_id_ext{obj_id=ProviderObjId, type = ?DOMAIN_MAIL_PROVIDER} ->
            case nkdomain:get_obj(ProviderObjId) of
                {ok, #{?DOMAIN_MAIL_PROVIDER:=Data}} ->
                    {ok, ProviderObjId, Data};
                _ ->
                    {error, provider_id_invalid}
            end;
        _ ->
            {error, provider_id_invalid}
    end.




