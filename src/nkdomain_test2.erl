-module(nkdomain_test2).
-compile(export_all).

-include_lib("nkapi/include/nkapi.hrl").

-define(WS, "ws://127.0.0.1:9202/api/ws").
-define(ADMIN_PASS, "1234").


test() ->
    {ok, _SessId, Pid, _Reply} = login("admin", ?ADMIN_PASS),
    ok = test_basic_1(Pid),
    {ok, _UId} = test_create_user(Pid),
    ok = test_session(Pid),
    ok = test_session2(Pid),
    test_delete(Pid).


%% Probar ahora dominios y subdominios
%% Búsqueda de hijos y tipos
%% Enable



%% Check for root domain and admin user
test_basic_1(Pid) ->
    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := <<"admin">>,
            <<"parent_id">> := <<"root">>,
            <<"path">> := <<"/users/admin">>,
            <<"created_time">> := _,
            <<"user">> := #{
                <<"name">> := _,
                <<"password">> := Pass,
                <<"surname">> := _
            }
        } = U1} =
        cmd(Pid, user, get, #{}),
    {ok, Pass} = nkdomain_user_obj:user_pass(?ADMIN_PASS),
    {ok, U1} = cmd(Pid, user, get, #{id=><<"admin">>}),
    {ok, U1} = cmd(Pid, user, get, #{id=><<"/users/admin">>}),
    {error, {<<"object_not_found">>,<<"Object not found">>}} = cmd(Pid, user, get, #{id=><<"admin2">>}),

    {ok,
        #{
            <<"type">> := <<"domain">>,
            <<"obj_id">> := <<"root">>,
            <<"parent_id">> := <<>>,
            <<"path">> := <<"/">>,
            <<"created_time">> := _,
            <<"description">> := _,
            <<"domain">> := #{}
        } = D1} =
        cmd(Pid, domain, get, #{}),
    {ok, D1} = cmd(Pid, user, get, #{id=><<"root">>}),
    {ok, D1} = cmd(Pid, user, get, #{id=><<"/">>}),
    ok.


%% Create user /users/user1, load, unload, update
test_create_user(Pid) ->
    case nkdomain_obj_lib:delete(root, "/users/user1", normal) of
        {error, path_not_found} ->
            ok;
        ok ->
            timer:sleep(1000)
    end,
    {error, path_not_found} = nkdomain:find(root, "/users/user1"),
    {error, object_not_found} = nkdomain:load(root, "/users/user1"),

    U2_Create = #{
        obj_name => user1,
        user => #{
            name => user1,
            surname => surname1,
            email => "user1@root",
            password => pass1
        }
    },
    {ok, #{<<"obj_id">>:=U2Id, <<"path">>:=<<"/users/user1">>}} =
        cmd(Pid, user, create, U2_Create),
    {error, {<<"name_is_already_used">>, <<"Name is already used: 'users/user1'">>}} =
        cmd(Pid, user, create, U2_Create),

    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := U2Id,
            <<"parent_id">> := <<"root">>,
            <<"path">> := <<"/users/user1">>,
            <<"created_time">> := CT,
            <<"user">> := #{
                <<"name">> := <<"user1">>,
                <<"surname">> := <<"surname1">>,
                <<"email">> := <<"user1@root">>,
                <<"password">> := P2

            }
        } = U2} =
        cmd(Pid, user, get, #{id=>U2Id}),
    true = nklib_util:m_timestamp() - CT < 1000,
    {ok, P2} = nkdomain_user_obj:user_pass("pass1"),

    timer:sleep(1000),
    {ok, U2} = cmd(Pid, user, get, #{id=><<"/users/user1">>}),
    {ok, <<"user">>, U2Id, <<"/users/user1">>, Pid1} = F1 = nkdomain:find(root, "/users/user1"),
    F1 = nkdomain:find(root, U2Id),
    F1 = nkdomain:load(root, "/users/user1"),
    F1 = nkdomain:load(root, U2Id),

    ok = nkdomain_obj:unload(U2Id, normal),
    timer:sleep(2000),
    {ok, <<"user">>, U2Id, <<"/users/user1">>, undefined} = F2 = nkdomain:find(root, "/users/user1"),
    F2 = nkdomain:find(root, U2Id),
    {ok, <<"user">>, U2Id, <<"/users/user1">>, Pid2} = F3 = nkdomain:load(root, "/users/user1"),
    F3 = nkdomain:find(root, U2Id),
    false = Pid1 == Pid2,

    U2_Update = #{surname=><<"surname-1">>, password=><<"pass2">>},
    {ok, #{}} = cmd(Pid, user, update, #{id=><<"/users/user1">>, user=>U2_Update}),
    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := U2Id,
            <<"parent_id">> := <<"root">>,
            <<"path">> := <<"/users/user1">>,
            <<"created_time">> := CT,
            <<"user">> := #{
                <<"name">> := <<"user1">>,
                <<"surname">> := <<"surname-1">>,
                <<"email">> := <<"user1@root">>,
                <<"password">> := P3

            }
        }} =
        cmd(Pid, user, get, #{id=>U2Id}),
    {ok, P3} = nkdomain_user_obj:user_pass("pass2"),
    {ok, U2Id}.


%% A session is created along the user login.
%% On logout, session is removed
test_session(Pid) ->
    {ok, SessId, Pid2, _Reply} = login("/users/user1", pass2),
    {ok, #{<<"type">>:=<<"user">>, <<"path">>:=<<"/users/user1">>, <<"obj_id">>:=UId}} = cmd(Pid2, user, get, #{}),
    {ok, #{<<"type">>:=<<"user">>, <<"path">>:=<<"/users/admin">>}} = cmd(Pid, user, get, #{}),
    {ok, <<"session">>, SessId, <<"/users/user1/sessions/", SessId/binary>>, SPid} = nkdomain:find(root, SessId),
    true = is_pid(Pid),

    % Object has active childs
    {error,
        {<<"object_has_childs">>,<<"Object has childs">>}} =
        cmd(Pid, user, delete, #{id=>UId, reason=>wont_work}),

    {ok, #{
        <<"type">> := <<"session">>,
        <<"obj_id">> := SessId,
        <<"parent_id">> := UId,
        <<"active">> := true,
        <<"created_time">> := _,
        <<"path">> := <<"/users/user1/sessions/", SessId/binary>>,
        <<"referred_id">> := UId,
        <<"session">> := #{
            <<"local">> := <<"ws:0.0.0.0:9202">>,
            <<"remote">> := <<"ws:127.0.0.1:", _/binary>>
        }
    }} =
        cmd(Pid2, session, get, #{}),

    nkapi_client:stop(Pid2),
    timer:sleep(2000),
    false = is_process_alive(SPid),
    {error, object_not_found} = nkdomain:find(root, SessId),
    {ok, 1, [S2]} = nkdomain_obj_lib:find_archive(root, SessId),
    #{
        <<"obj_id">>:=SessId,
        <<"destroyed_time">>:= T1,
        <<"destroyed_code">> := <<"object_stopped">>
    } = S2,
    true = nklib_util:m_timestamp() - T1 < 5000,
    ok.


%% Find session and childs of admin user
test_session2(Pid) ->
    {ok, #{<<"obj_id">>:=SessId}} = cmd(Pid, session, get, #{}),
    {ok, <<"session">>, SessId, Path, SessPid} = nkdomain:find(root, SessId),
    {ok, Childs} = nkdomain_obj:get_childs(<<"admin">>),
    {<<"session">>, SessId, _Name, SessPid} = lists:keyfind(SessId, 2, Childs),
    exit(SessPid, kill),
    timer:sleep(1100),
    {ok, Childs2} = nkdomain_obj:get_childs(<<"admin">>),
    false = lists:keyfind(SessId, 2, Childs2),
    {ok, <<"session">>, SessId, Path, undefined} = nkdomain:find(root, SessId),

    % Object has not active childs, but the child is still found
    {error, object_has_childs} = nkdomain_obj:delete(<<"admin">>, wont_work),

    {ok, #{active:=1}} = nkdomain_store:clean(root),
    timer:sleep(1100),
    {error, object_not_found} = nkdomain:find(root, SessId),
    {ok, 1, [#{<<"destroyed_code">>:=<<"object_clean_process">>}]} =
        nkdomain_obj_lib:find_archive(root, SessId),
    ok.


test_delete(Pid) ->
    {ok, #{<<"obj_id">>:=ObjId}} = cmd(Pid, user, get, #{id=><<"/users/user1">>}),
    {ok, #{}} = cmd(Pid, user, delete, #{id=> <<"/users/user1">>}),
    {error,{<<"object_not_found">>,<<"Object not found">>}} =
        cmd(Pid, user, delete, #{id=> <<"/users/user1">>}),
    timer:sleep(1100),
    {ok, 1, [#{<<"path">>:=<<"/users/user1">>}]} =
        nkdomain_obj_lib:find_archive(root, ObjId),
    {ok, _N, [#{<<"path">>:=<<"/users/user1">>}|_]} =
        nkdomain_obj_lib:find_archive(root, <<"/users/user1">>),
    ok.










%% ===================================================================
%% Client fun
%% ===================================================================


login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, _SessId, _Pid, _Reply} = nkapi_client:start(root, ?WS, Login, Fun, #{}).


api_client_fun(#nkapi_req{class=event, data=Event}, UserData) ->
    lager:notice("CLIENT event ~p", [lager:pr(Event, nkservice_events)]),
    {ok, UserData};

api_client_fun(_Req, UserData) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.

get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.


cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).


