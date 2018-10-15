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

-export([create/4, send_msg/3]).
-export([object_info/0, object_admin_info/0, object_parse/2, object_es_mapping/0]).

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

create(SrvId, Domain, UserId, Req) ->
    From = maps:get(from, Req, <<>>),
    To = maps:get(to, Req, <<>>),
    Subject = maps:get(subject, Req, <<>>),
    Body = maps:get(body, Req, <<>>),
    ContentType = maps:get(content_type, Req, <<"text/plain">>),
    MoreTags = maps:get(tags, Req, []),
    Sent = maps:get(sent, Req, <<"false">>),
    Tags = [
        <<(?TAG_MAIL_FROM)/binary, ":", (nklib_util:to_binary(From))/binary>>,
        <<(?TAG_MAIL_TO)/binary, ":", (nklib_util:to_binary(To))/binary>>,
        <<(?TAG_MAIL_SUBJECT)/binary, ":", (nklib_util:to_binary(Subject))/binary>>,
        <<(?TAG_MAIL_BODY)/binary, ":", (nklib_util:to_binary(Body))/binary>>,
        <<(?TAG_MAIL_CONTENT_TYPE)/binary, ":", (nklib_util:to_binary(ContentType))/binary>>,
        <<(?TAG_MAIL_SENT)/binary, ":", (nklib_util:to_binary(Sent))/binary>>
        | MoreTags
    ],
    MailObj = #{
        type => ?DOMAIN_MAIL,
        srv_id => SrvId,
        domain_id => Domain,
        created_by => UserId,
        name => Subject,
        tags => Tags
    },
    case nkdomain_obj_make:create(MailObj) of
        {ok, #obj_id_ext{obj_id=MailId}, []} ->
            {ok, MailId};
        {error, Error} -> 
            {error, Error}
    end.


%% @doc Sends a mail msg
-spec send_msg(nkservice:id(), nkdomain:id(), nkmail:msg()) ->
    {ok, Meta::map()} | {error, term()}.

send_msg(SrvId, ProviderId, Msg) ->
    case nkdomain:get_obj(ProviderId) of
        {ok, #{?DOMAIN_MAIL_PROVIDER:=Provider}} ->
            Msg2 = Msg#{provider=>Provider},
            nkmail:send(SrvId, Msg2);
        {error, object_not_found} ->
            {error, provider_not_found};
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        schema_type => 'MailMessage',
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
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
    }.





%% ===================================================================
%% Internal
%% ===================================================================

