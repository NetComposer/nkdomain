%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain
-module(nkdomain_s3_2).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all, nowarn_export_all]).

-include("nkdomain.hrl").
-include_lib("nklib/include/nklib.hrl").


%% ===================================================================
%% Sample
%% ===================================================================



s1() ->
    Config = #{
        key => "",
        secret => "",
        region => 'eu-west-1'
    },
    {Method, Url, Hds} = sns_request(Config),
    case hackney:request(Method, Url, Hds, <<>>, [with_body]) of
        {ok, 200, _RespHds, Body} ->
            {MessageId, RequestId} = parse_sns(Body),
            {ok, MessageId, RequestId};
        {ok, Code, _, Body} ->
            {error, {http_error, {Code, Body}}};
        {error, Error} ->
            {error, {http_hackney_error, Error}}
    end.



parse_sns(Text) ->
    [_, Text2] = binary:split(Text, <<"MessageId>">>),
    [MessageId, _] = binary:split(Text2, <<"</MessageId">>),
    [_, Text3] = binary:split(Text, <<"RequestId>">>),
    [RequestId, _] = binary:split(Text3, <<"</RequestId">>),
    {MessageId, RequestId}.





%% ===================================================================
%% Internal
%% ===================================================================



%% @private
sns_request(Config) ->
    Config2 = Config#{
        method => post,
        region => 'eu-west-1',
        service => sns,
        params => #{
            "Action" => "Publish",
            "Version" => "2010-03-31",
            "Message" => <<"Méssage"/utf8>>,
            "PhoneNumber" => "+34674186274"
        }
    },
    nklib_aws:request_v4(Config2).



