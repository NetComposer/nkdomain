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

%% @doc Test Utilities
-module(nkdomain_test_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').


-compile(export_all).
-compile(nowarn_export_all).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkservice/include/nkservice_actor.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_test_data() ->
    #{d1:=D1, d2:=D2, d3:=D3, u1:=U1} = test_data(),
    delete_test_data(),
    % Create domains
    {created, _} = api(#{verb=>create, resource=>domains, name=>"a-nktest", body=>D1}),
    {created, _} = api(#{verb=>create, domain=>"a-nktest", resource=>"domains", name=>"b", body=>D2}),
    {created, _} = api(#{verb=>create, domain=>"b.a-nktest", resource=>"domains", name=>"c", body=>D3}),
    {created, _} = api(#{verb=>create, domain=>"b.a-nktest", resource=>users, name=>"ut1", body=>U1}),
    nkdomain_api_events:wait_for_save(),
    ok.


delete_test_data() ->

    % First delete the events (potentially a lot) without transactions:
    http_delete("/domains/root/events?deep=true&fieldSelector=path:prefix:a-nktest"),
    % Deletes everything linked to 'a' with a transaction cascade delete
    http_delete("/domains/root/domains/a-nktest?cascade=true"),
    % Wait for events to be saved
    nkdomain_api_events:wait_for_save(),
    % Delete events generated on deletion of objects
    http_delete("/domains/root/events?deep=true&fieldSelector=path:prefix:a-nktest"),
    ok.



test_data() ->
    D1 = yaml(<<"
        apiVersion: core/v1a1
        kind: Domain
        spec:
            httpPool:
                maxConnections: 25
                timeout: 60000
        metadata:
            name: a-nktest
            domain: root
            labels:
                is_a-nktest_domain: true
            fts:
                fts_domain: 'Domáin a-nktest'
    "/utf8>>),
    D2 = yaml(<<"
        apiVersion: core/v1a1
        kind: Domain
        metadata:
            name: b
            domain: a-nktest
            labels:
                is_a-nktest_domain: true
                is_b_domain: true
            fts:
                fts_domain: 'Domáin b'
    "/utf8>>),
    D3 = yaml(<<"
        apiVersion: core/v1a1
        kind: Domain
        metadata:
            name: c
            domain: b.a-nktest
            labels:
                is_a-nktest_domain: true
                is_b_domain: true
                is_c_domain: true
            fts:
                fts_domain: 'Domáin c'
    "/utf8>>),
    U1 = yaml(<<"
        apiVersion: core/v1a1
        kind: User
        spec:
            password: pass1
        metadata:
            name: ut1
            domain: b.a-nktest
            labels:
                is_a-nktest_domain: true
                is_b_domain: true
            fts:
                fts_name: 'Úser MY name'
    "/utf8>>),
    #{d1=>D1, d2=>D2, d3=>D3, u1=>U1}.




api(Api) ->
    case nkdomain_api:request(?ROOT_SRV, Api#{group=>?GROUP_CORE, vsn=>?GROUP_CORE_V1A1}) of
        {ok, Reply, _} ->
            {ok, Reply};
        {created, Reply, _} ->
            {created, Reply};
        {error, Error, _} ->
            {error, Error}
    end.


api_watch(Api) ->
    Ref = make_ref(),
    Api2 = Api#{verb=>watch, callback=>?MODULE, meta => #{nkdomain_api_pid=>self(), nkdomain_api_ref=>Ref}},
    Pid = spawn_link(fun() -> api(Api2) end),
    {Ref, Pid}.


api_watch_stop({_Ref, Pid}) ->
    Pid ! stop_watch.


%% @private
wait_api_event({Ref, _Pid}=Id, Reason) ->
    receive
        {api_event, Ref, Body} ->
            self() ! {api_event2, Ref, Body},
            % io:format("API EVENT ~p\n", [Body]),
            wait_api_event(Id, Reason);
        {api_event2, Ref, #{<<"type">>:=Type, <<"object">>:=Ev}} when Reason == <<>> ->
            {Type, Ev};
        {api_event2, Ref, #{<<"type">>:=Type, <<"object">>:=#{<<"reason">>:=Reason}=Ev}} ->
            {Type, Ev}
    after 1000 ->
        api_event_timeout
    end.


%% @private
clean_events() ->
    receive
        Msg -> {error, Msg}
    after 1500 ->
        no_message
    end.



%% @doc
new_event(Body, #{meta:=#{nkdomain_api_pid:=Pid, nkdomain_api_ref:=Ref}}=ApiReq) ->
    Pid ! {api_event, Ref, Body},
    {ok, ApiReq}.


http_get(Path) ->
    {ok, {{_, Code, _}, _Hds, Body}} = httpc:request(http_url(Path)),
    {Code, nklib_json:decode(Body)}.

http_post(Path, Body) ->
    Body2 = nklib_json:encode(Body),
    {ok, {{_, Code, _}, _Hds, Body3}} = httpc:request(post, {http_url(Path), [], "application/json", Body2}, [], []),
    {Code, nklib_json:decode(Body3)}.

http_put(Path, Body) ->
    Body2 = nklib_json:encode(Body),
    {ok, {{_, Code, _}, _Hds, Body3}} = httpc:request(put, {http_url(Path), [], "application/json", Body2}, [], []),
    {Code, nklib_json:decode(Body3)}.

http_delete(Path) ->
    {ok, {{_, Code, _}, _Hds, Body}} = httpc:request(delete, {http_url(Path), [], "", ""}, [], []),
    {Code, nklib_json:decode(Body)}.

http_list(Path) ->
    {200, List} =  http_get(Path),
    #{<<"metadata">> := #{<<"total">>:=Total, <<"size">>:=Size}, <<"items">> := Items} = List,
    {Total, Size, Items}.

http_search(Domain, Spec) ->
    Path = binary_to_list(list_to_binary([http_host(), "/search/v1a1/domains/" ++ Domain])),
    Body = nklib_json:encode(Spec),
    case httpc:request(post, {Path, [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _Hds, List}} ->
            #{<<"metadata">> := #{<<"total">>:=Total, <<"size">>:=Size}, <<"items">> := Items} = nklib_json:decode(List),
            {Total, Size, Items};
        {ok, {{_, Code, _}, _Hds, Body2}} ->
            {Code, nklib_json:decode(Body2)}
    end.


http_search_delete(Domain, Spec) ->
    Path = binary_to_list(list_to_binary([http_host(), "/search/v1a1/domains/" ++ Domain ++ "?delete=true"])),
    Body = nklib_json:encode(Spec),
    {ok, {{_, Code, _}, _Hds, Body2}} = httpc:request(post, {Path, [], "application/json", Body}, [], []),
    {Code, nklib_json:decode(Body2)}.


http_host() ->
    "http://127.0.0.1:9001".


http_url(Path) ->
    Bin = list_to_binary([http_host(), "/apis/core/v1a1", Path]),
    binary_to_list(Bin).



http_watch(Path) ->
    Url = "http://127.0.0.1:9001/apis/core/v1a1" ++ Path ++ "?watch=true",
    % lager:error("NKLOG URL ~p", [Url]),
    Opts = [{connect_timeout, 1000}, {recv_timeout, 20000}, async, with_body],
    {ok, Ref} = hackney:request(get, Url, [], [], Opts),
    receive
        {hackney_response, Ref, {status, 200, _}} ->
            Ref
    after 5000 ->
        error(http_timeout)
    end.


http_watch_stop(Ref) ->
    ok = hackney_manager:close_request(Ref).


%% @private
wait_http_event(Ref, Reason) ->
    wait_http_event(Ref, Reason, <<>>).


%% @private
wait_http_event(Ref, Reason, Buff) ->
    receive
        {hackney_response, Ref, {headers, Headers}} ->
            #{
                <<"Content-Type">> := <<"application/json">>,
                <<"transfer-encoding">> := <<"chunked">>
            } = maps:from_list(Headers),
            wait_http_event(Ref, Reason, Buff);
        {hackney_response, Ref, <<"\r\n">>} ->
            wait_http_event(Ref, Reason, <<>>);
        {hackney_response, Ref, Body} ->
            Body2 = <<Buff/binary, Body/binary>>,
            case catch nklib_json:decode(Body2) of
                {'EXIT', _} ->
                    % lager:error("NKLOG MORE DATA ~p", [Body2]),
                    wait_http_event(Ref, Reason, Body2);
                Event ->
                    self() ! {http_event, Ref, Event},
                    % io:format("HTTP EVENT ~s\n", [nklib_json:encode_pretty(Event)]),
                    wait_http_event(Ref, Reason, <<>>)
            end;
        {http, Ref, done} ->
            stream_end;
        {http, Ref, {error, Error}} ->
            lager:error("NKLOG Hackney error: ~p", [Error]),
            stream_end;
        stop ->
            stop;
        {http_event, Ref, #{<<"type">>:=Type, <<"object">>:=Ev}} when Reason == <<>> ->
            {Type, Ev};
        {http_event, Ref, #{<<"type">>:=Type, <<"object">>:=#{<<"reason">>:=Reason}=Ev}} ->
            {Type, Ev}
    after 1000 ->
        http_event_timeout
    end.


%%http_watch(Path) ->
%%    Url = "http://127.0.0.1:9001/apis/core/v1a1" ++ Path ++ "?watch=true",
%%    % lager:error("NKLOG URL ~p", [Url]),
%%    {ok, Ref} = httpc:request(get, {Url, []}, [{connect_timeout, 1000}],
%%                 [{sync, false}, {stream, self}]),
%%    receive
%%        {http, {Ref, stream_start, Hds}} ->
%%            #{
%%                "content-type" := "application/json",
%%                "transfer-encoding" := "chunked"
%%            } = maps:from_list(Hds),
%%            Ref
%%    after 5000 ->
%%        error(http_timeout)
%%    end.


%%http_watch_stop(Ref) ->
%%    catch httpc:cancel_request(Ref).
%%
%%
%%%% @private
%%wait_http_event(Ref, Reason) ->
%%    wait_http_event(Ref, Reason, <<>>).
%%
%%
%%%% @private
%%wait_http_event(Ref, Reason, Buff) ->
%%    receive
%%        {http, {Ref, stream, <<"\r\n">>}} ->
%%            wait_http_event(Ref, Reason, <<>>);
%%        {http, {Ref, stream, Body}} ->
%%            Body2 = <<Buff/binary, Body/binary>>,
%%            case catch nklib_json:decode(Body2) of
%%                {'EXIT', _} ->
%%                    % lager:error("NKLOG MORE DATA ~p", [Body2]),
%%                    wait_http_event(Ref, Reason, Body2);
%%                Event ->
%%                    self() ! {http_event, Ref, Event},
%%                    % io:format("HTTP EVENT ~s\n", [nklib_json:encode_pretty(Event)]),
%%                    wait_http_event(Ref, Reason, <<>>)
%%            end;
%%        {http, {Ref, stream_end, _Hds}} ->
%%            stream_end;
%%        stop ->
%%            stop;
%%        {http_event, Ref, #{<<"type">>:=Type, <<"object">>:=Ev}} when Reason == <<>> ->
%%            {Type, Ev};
%%        {http_event, Ref, #{<<"type">>:=Type, <<"object">>:=#{<<"reason">>:=Reason}=Ev}} ->
%%            {Type, Ev}
%%        after 1000 ->
%%            http_event_timeout
%%    end.


k8s_watch() ->
     Url = "http://127.0.0.1:8001/api/v1/events?watch=true",
    {ok, Ref} = httpc:request(get, {Url, []}, [], [{sync, false}, {stream, self}]),
    receive
        {http, {Ref, event_stream_start, Hds}} ->
            #{
                "content-type" := "application/json",
                "transfer-encoding" := "chunked"
            } = maps:from_list(Hds),
            Events = k8s_watch_events(Ref, 5000, []),
            catch httpc:cancel_request(Ref),
            {ok, Events}
    after 5000 ->
        {error, timeout}
    end.


k8s_watch_events(Ref, Chunks, Acc) when Chunks > 0 ->
    receive
        {http, {Ref, stream, Body}} ->
            Event = (catch nklib_json:decode(Body)),
            lager:error("NKLOG STREAMK: ~p", [Event]),
            k8s_watch_events(Ref, Chunks-1, [Event|Acc]);
        {http, {Ref, stream_end, _Hds}} ->
            lager:error("NKLOG END"),
            Acc;
        Other ->
            lager:error("NKLOG HTTPC OTHER ~p", [Other]),
            {error, {httpc_closed, Other}}
    after 600000 ->
        error(600000)
    end;

k8s_watch_events(_Ref, _Chunks, Acc) ->
    Acc.



yaml(Str) ->
    %lager:error("NKLOG STR ~s", [Str]),
    [Obj] = nklib_yaml:decode(Str),
    Obj.




wait(Path) ->
    spawn(
        fun() ->
            R = http_watch(Path++"?watch=true"),
            lager:error("Http Watch result: ~p", [R])
        end).


send_test_event() ->
    send_test_event("/nkdomain-root/core/domain/root").


send_test_event(Path) ->
    nkservice_actor_srv:async_op(Path, {send_event, test_api}).



stop() ->
    nkservice_actor:stop(?ROOT_SRV, "/nkdomain-root/core/user/admin").

w(Vsn) ->
    nkdomain_api_events:wait_for_save(),
    api_watch(#{resource=>domains, name=>root, params=>#{deep=>false, resourceVersion=>Vsn}}),
    timer:sleep(500),
    lager:error("NKLOG SENDING1"),
    send_test_event().


%%% Delete "TestAPI" events at root
%%delete_root_test_api() ->
%%    Opts = #{
%%        filter => #{
%%            'and' => [
%%                #{field => <<"type">>, value => <<"event">>},
%%                #{field => <<"reason">>, value=><<"TestAPI">>}
%%            ]
%%        }
%%    },
%%    nkservice_actor:delete_all(?DOMAIN_SRV_ROOT, Opts#{delete=>true}, #{}).


