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

%% @doc NkDomain HTTP API processing
-module(nkdomain_api_http).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([rest_api/4]).
-export([event_stream_start/1, event_stream_heartbeat/1, event_stream_stop/2, new_event/2]).
-behavior(nkdomain_api_watch).

-include("nkdomain.hrl").
-include("nkdomain_api.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").

-define(MAX_BODY_SIZE, 1000000).
-define(MAX_UPLOAD_SIZE, 100000000).


%% ===================================================================
%% REST API Invocation
%% ===================================================================


%% @private
%% SrvId is the service receiving the request
rest_api(SrvId, Verb, Path, Req) ->
    rest_set_debug(SrvId),
    try do_rest_api(Verb, Path, Req) of
        {ok, Map} when is_map(Map) ->
            rest_api_reply(200, Map, Req);
        {error, Error} ->
            #{<<"code">>:=Code} = Status = nkdomain_api:status(SrvId, Error),
            rest_api_reply(Code, Status, Req);
        {ok, _, #{meta:=#{nkdomain_http_stream:=true, nkdomain_http_req:=HttpReq}}} ->
            {stop, HttpReq};
        {ok, Reply, #{meta:=#{nkdomain_http_req:=HttpReq}}} ->
            rest_api_reply(200, Reply, HttpReq);
        {created, Reply, #{meta:=#{nkdomain_http_req:=HttpReq}}} ->
            rest_api_reply(201, Reply, HttpReq);
        {error, _, #{meta:=#{nkdomain_http_stream:=true, nkdomain_http_req:=HttpReq}}} ->
            {stop, HttpReq};
        {error, #{<<"code">>:=Code}=Status, #{meta:=#{nkdomain_http_req:=HttpReq}}} ->
            rest_api_reply(Code, Status, HttpReq);
        {error, Msg, #{meta:=#{nkdomain_http_req:=HttpReq}}} ->
            #{<<"code">>:=Code} = Status = nkdomain_api:status(SrvId, Msg),
            rest_api_reply(Code, Status, HttpReq);
        {raw, {CT, Bin}, #{meta:=#{nkdomain_http_req:=HttpReq}}} ->
            Hds = #{
                <<"Content-Type">> => CT,
                <<"Server">> => <<"NetComposer">>
            },
            {http, 200, Hds, Bin, HttpReq}
    catch
        throw:{error, Error} ->
            #{<<"code">>:=Code} = Status = nkdomain_api:status(SrvId, Error),
            rest_api_reply(Code, Status, Req)
    end.


%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
event_stream_start(#{meta:=#{nkdomain_http_req:=Req}=Meta}=ApiReq) ->
    Hds = #{
        <<"Content-Type">> => <<"application/json">>,
        <<"Server">> => <<"NetComposer">>
    },
    Req2 = nkservice_rest_http:stream_start(200, Hds, Req),
    Meta2 = Meta#{
        nkdomain_http_stream => true,
        nkdomain_http_req := Req2
    },
    {ok, ApiReq#{meta:=Meta2}}.


%% @doc
event_stream_stop(_Reason, #{meta:=#{nkdomain_http_req:=Req}}=ApiReq) ->
    lager:error("NKLOG EVENT STREAM STOP"),
    nkservice_rest_http:stream_stop(Req),
    {ok, ApiReq}.


%% @doc
event_stream_heartbeat(#{meta:=#{nkdomain_http_req:=Req}}=ApiReq) ->
    lager:error("NKLOG EVENT STREAM HEARTBEAT"),
    ok = nkservice_rest_http:stream_body(<<"\r\n">>, Req),
    {ok, ApiReq}.


%% @doc
new_event(Event, #{meta:=#{nkdomain_http_req:=Req}}=ApiReq) ->
    Body = nklib_json:encode(Event),
    nkservice_rest_http:stream_body(Body, Req),
    {ok, ApiReq}.


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc
rest_set_debug(SrvId) ->
    Debug = nkservice_util:get_debug(SrvId, nkdomain, single, {api, http})==true,
    put(nkdomain_debug, Debug),
    ?API_DEBUG("HTTP debug started", []).


%% @private
rest_api_reply(Code, Body, Req) ->
    Hds = #{
        <<"Content-Type">> => <<"application/json">>,
        <<"Server">> => <<"NetComposer">>
    },
    Body2 = nklib_json:encode_sorted(Body),
    {http, Code, Hds, Body2, Req}.



%% @private
% /
do_rest_api(<<"GET">>, [], #{srv:=SrvId}) ->
    Paths1 = ?CALL_SRV(SrvId, nkdomain_get_paths, [SrvId, []]),
    Paths2 = lists:usort(lists:flatten(Paths1)),
    {ok, #{<<"paths">>=>Paths2}};

do_rest_api(<<"GET">>, [<<"favicon.ico">>], _Req) ->
    {error, {resource_invalid, <<"favicon.ico">>}};

% /apis
do_rest_api(<<"GET">>, [<<"apis">>], #{srv:=SrvId}) ->
    {ok, nkdomain_api_lib:api_group_list(SrvId)};

% /apis/Api
do_rest_api(<<"GET">>, [<<"apis">>, Group], #{srv:=SrvId}) ->
    Groups = nkdomain_api_lib:api_groups(SrvId),
    case [Info || #{<<"name">>:=N}=Info <-Groups, N==Group] of
        [Info] ->
            {ok, Info};
        _ ->
            {error, {api_group_unknown, Group}}
    end;

% /apis/Api/Vsn
do_rest_api(<<"GET">>, [<<"apis">>, Group, Vsn], #{srv:=SrvId}) ->
    ApiVsn = <<Group/binary, $/, Vsn/binary>>,
    case nkdomain_lib:nkdomain_get_api_resources(SrvId, Group, Vsn) of
        undefined ->
            {error, {api_unknown, ApiVsn}};
        Resources ->
            {ok, nkdomain_api_lib:api_resources_list(ApiVsn, Resources)}
    end;

% /apis/core/v1/domains
do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>], Req) ->
    do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, ?ROOT_DOMAIN, <<"domains">>], Req);

% /apis/core/v1/domains/Domain
do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, Name], Req) ->
    do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, ?ROOT_DOMAIN, <<"domains">>, Name], Req);

% /apis/core/v1/domains/Domain/ResType
do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, Domain, ResType], Req) ->
    launch_rest_api(Verb, Group, Vsn, Domain, ResType, <<>>, [], Req);

% /apis/core/v1/domains/Domain/ResType/_upload
do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, Domain, ResType, <<"_upload">>], Req) ->
    launch_rest_upload(Verb, Group, Vsn, Domain, ResType, <<>>, [], Req);

% /apis/core/v1/domains/Domain/ResType/Name/SubRes/_upload
do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, Domain, ResType, Name, RestType2, <<"_upload">>], Req) ->
    launch_rest_upload(Verb, Group, Vsn, Domain, ResType, Name, [RestType2], Req);

% /apis/core/v1/domains/Domain/ResType/Name...
do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, Domain, ResType, Name|SubRes], Req) ->
    case lists:reverse(SubRes) of
        [<<"_upload">>|SubRes2] ->
            launch_rest_upload(Verb, Group, Vsn, Domain, ResType, Name, lists:reverse(SubRes2), Req);
        _ ->
            launch_rest_api(Verb, Group, Vsn, Domain, ResType, Name, SubRes, Req)
    end;

% /apis/core/v1/ResType (implicit 'root' domain)
do_rest_api(Verb, [<<"apis">>, Group, Vsn, ResType], Req) ->
    do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, ?ROOT_DOMAIN, ResType], Req);

% /apis/core/v1/ResType/Name (implicit 'root' domain)
do_rest_api(Verb, [<<"apis">>, Group, Vsn, ResType, Name|SubRes], Req) ->
    do_rest_api(Verb, [<<"apis">>, Group, Vsn, <<"domains">>, ?ROOT_DOMAIN, ResType, Name|SubRes], Req);


% /search/v1
do_rest_api(Verb, [?GROUP_SEARCH, Vsn], Req) ->
    do_rest_api(Verb, [?GROUP_SEARCH, Vsn, <<"domains">>, ?ROOT_DOMAIN], Req);

% /search/v1/domains/Domain
do_rest_api(Verb, [?GROUP_SEARCH, Vsn, <<"domains">>, Domain], Req) ->
    launch_rest_search(Verb, Vsn, Domain, Req);


% /graphql
do_rest_api(<<"POST">>, [<<"graphql">>], _) ->
    {error, {resource_invalid, <<>>}};




do_rest_api(Verb, [<<"_test">>, <<"faxin">>|Rest], Req) ->
    BodyOpts = #{max_size=>?MAX_BODY_SIZE},
    {Body, _Req2} = case nkservice_rest_http:get_body(Req, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LLOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid})
    end,
    Qs = nkservice_rest_http:get_qs(Req),
    Hds = nkservice_rest_http:get_headers(Req),
    lager:error("NKLOG HTTP FAX IN (~s)\nPath: ~p\nQs: ~p\nHeaders: ~p\nBody: ~p\n",
        [Verb, Rest, Qs, Hds, Body]),
    Rep = <<"<Response><Receive action=\"/fax/received\"/></Response>">>,
    {binary, <<"application/xml">>, Rep};



% /_test
do_rest_api(Verb, [<<"_test">>|Rest], Req) ->
    BodyOpts = #{max_size=>?MAX_BODY_SIZE},
    {Body, _Req2} = case nkservice_rest_http:get_body(Req, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LLOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid})
    end,
    Qs = nkservice_rest_http:get_qs(Req),
    Hds = nkservice_rest_http:get_headers(Req),
    lager:error("NKLOG HTTP _TEST (~s)\nPath: ~p\nQs: ~p\nHeaders: ~p\nBody: ~p\n",
                [Verb, Rest, Qs, Hds, Body]),
    {ok, #{}};




do_rest_api(_Verb, Path, _Req) ->
    {error, {resource_invalid, nklib_util:bjoin(Path, $/)}}.


%% @doc
launch_rest_api(Verb, Group, Vsn, Domain, ResType, Name, SubRes, #{srv:=SrvId}=Req) ->
    ?API_DEBUG("HTTP incoming: ~s /apis/~s/~s/domains/~s/~s/~s/~s", [Verb, Group, Vsn, Domain, ResType,Name,SubRes]),
    Qs = maps:from_list(nkservice_rest_http:get_qs(Req)),
    BodyOpts = #{max_size=>?MAX_BODY_SIZE, parse=>true},
    {Body, Req2} = case nkservice_rest_http:get_body(Req, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LLOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid})
    end,
    ToWatch = maps:get(<<"watch">>, Qs, undefined) == <<"true">>,
    Verb2 = case Verb of
        <<"GET">> when ToWatch ->
            watch;
        <<"GET">> when Name == <<>> ->
            list;
        <<"POST">> when Name == <<>>, SubRes == <<"_query">> ->
            list;
        <<"GET">> ->
            get;
        <<"HEAD">> when ToWatch ->
            watch;
        <<"HEAD">> when Name == <<>> ->
            list;
        <<"HEAD">> ->
            get;
        <<"POST">> ->
            create;
        <<"PUT">> ->
            update;
        <<"PATCH">> when Name == <<>> ->
            throw({error, method_not_allowed});
        <<"PATCH">> ->
            patch;
        <<"DELETE">> when Name == <<>> ->
            deletecollection;
        <<"DELETE">> ->
            delete;
        _ ->
            throw({error, method_not_allowed})
    end,
    ApiReq1 = #{
        verb => Verb2,
        group => Group,
        vsn => Vsn,
        domain => Domain,
        resource => ResType,
        subresource => SubRes,
        params => Qs,
        body => Body,
        auth => #{},
        callback => ?MODULE,
        url => nkservice_rest_http:get_external_url(Req),
        meta => #{
            nkdomain_http_req => Req2
        }
    },
    ApiReq2 = case Name of
        <<>> ->
            ApiReq1;
        _ ->
            ApiReq1#{name => Name}
    end,
    nkdomain_api:request(SrvId, ApiReq2).


%% @doc
launch_rest_upload(Verb, Group, Vsn, Domain, ResType, Name, SubRes, #{srv:=SrvId}=Req) ->
    ?API_DEBUG("HTTP incoming upload: ~s /apis/~s/~s/domains/~s/~s/~s/~s", [Verb, Group, Vsn, Domain, ResType,Name,SubRes]),
    Qs = maps:from_list(nkservice_rest_http:get_qs(Req)),
    BodyOpts = #{max_size=>?MAX_UPLOAD_SIZE, parse=>false},
    {Body, Req2} = case nkservice_rest_http:get_body(Req, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LLOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid})
    end,
    Verb2 = case Verb of
        <<"POST">> ->
            upload;
        _ ->
            throw({error, method_not_allowed})
    end,
    #{content_type:=CT} = Req,
    ApiReq1 = #{
        verb => Verb2,
        group => Group,
        vsn => Vsn,
        domain => Domain,
        resource => ResType,
        subresource => SubRes,
        params => Qs,
        body => Body,
        auth => #{},
        callback => ?MODULE,
        url => nkservice_rest_http:get_external_url(Req),
        meta => #{
            nkdomain_http_content_type => CT,
            nkdomain_http_req => Req2
        }
    },
    ApiReq2 = case Name of
        <<>> ->
            ApiReq1;
        _ ->
            ApiReq1#{name => Name}
    end,
    nkdomain_api:request(SrvId, ApiReq2).


%% @private
launch_rest_search(Verb, Vsn, Domain, #{srv:=SrvId}=Req) ->
    Qs = maps:from_list(nkservice_rest_http:get_qs(Req)),
    BodyOpts = #{max_size=>?MAX_BODY_SIZE, parse=>true},
    {Body, Req2} = case nkservice_rest_http:get_body(Req, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LLOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid})
    end,
    Delete = maps:get(<<"delete">>, Qs, undefined) == <<"true">>,
    Verb2 = case Verb of
        <<"POST">> when Delete ->
            deletecollection;
        <<"POST">> ->
            list;
        _ ->
            throw({error, method_not_allowed})
    end,
    ApiReq = #{
        verb => Verb2,
        group => ?GROUP_SEARCH,
        vsn => Vsn,
        domain => Domain,
        body => Body,
        url => nkservice_rest_http:get_external_url(Req),
        meta => #{
            nkdomain_http_req => Req2
        }
    },
    nkdomain_api:request(SrvId, ApiReq).



%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).