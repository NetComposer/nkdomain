-module(nkdomain_test).
-compile(export_all).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").

-define(WS, "ws://127.0.0.1:9202/api/ws").
-define(ADMIN_PASS, "1234").


test() ->
    ok = test1(),
    ok = test2(),
    lager:notice("Domain test ok!").


test1() ->
    remove_data(),
    {ok, Pid, _} = login("admin", ?ADMIN_PASS),
    ok = test_basic_1(Pid),
    ok = test_create_user(Pid),
    ok = test_session1(Pid),
    ok = test_session2(Pid),
    %% test_session2 destroys the admin connection
    {ok, Pid2, _} = login("admin", ?ADMIN_PASS),
    ok = test_session3(Pid2),
    ok = test_delete(Pid2),
    remove_data().

test2() ->
    remove_data(),
    {ok, Pid, _} = login("admin", ?ADMIN_PASS),
    ok = test_basic_2(Pid),
    remove_data().





%% Check for root domain and admin user
test_basic_1(Pid) ->

    % Get admin user
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

    % Get root domain
    {ok,
        #{
            <<"type">> := <<"domain">>,
            <<"obj_id">> := <<"root">>,
            <<"parent_id">> := <<>>,
            <<"path">> := <<"/">>,
            <<"created_time">> := _,
            <<"description">> := _
        } = D1} =
        cmd(Pid, domain, get, #{}),
    {ok, D1} = cmd(Pid, user, get, #{id=><<"root">>}),
    {ok, D1} = cmd(Pid, user, get, #{id=><<"/">>}),
    ok.


test_create_user(Pid) ->
    {error, object_not_found} = nkdomain:find(root, "/users/tuser1"),
    {error, object_not_found} = nkdomain:load(root, "/users/tuser1"),

    %% Create user /users/tuser1,
    U2_Create = #{
        obj_name => tuser1,
        user => #{
            name => user1,
            surname => surname1,
            email => "user1@root",
            password => pass1
        }
    },
    {ok, #{<<"obj_id">>:=U2Id, <<"path">>:=<<"/users/tuser1">>}} = cmd(Pid, user, create, U2_Create),
    {error, {<<"object_already_exists">>, _}} = cmd(Pid, user, create, U2_Create),

    % Check user, creation date and password
    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := U2Id,
            <<"parent_id">> := <<"root">>,
            <<"path">> := <<"/users/tuser1">>,
            <<"created_time">> := CT,
            <<"user">> := #{
                <<"name">> := <<"user1">>,
                <<"surname">> := <<"surname1">>,
                <<"email">> := <<"user1@root">>,
                <<"password">> := P2

            }
        } = U2} =
        cmd(Pid, user, get, #{id=>U2Id}),
    true = nklib_util:m_timestamp() - CT < 500,
    {ok, P2} = nkdomain_user_obj:user_pass("pass1"),

    % Find user by several ways
    {ok, U2} = cmd(Pid, user, get, #{id=><<"/users/tuser1">>}),
    {ok, <<"user">>, U2Id, <<"/users/tuser1">>, Pid1} = F1 = nkdomain:find(root, "/users/tuser1"),
    F1 = nkdomain:find(root, U2Id),
    F1 = nkdomain:load(root, "/users/tuser1"),
    F1 = nkdomain:load(root, U2Id),

    % Unload the user
    ok = nkdomain_obj:unload(U2Id, normal),
    timer:sleep(100),
    {ok, <<"user">>, U2Id, <<"/users/tuser1">>, undefined} = F2 = nkdomain:find(root, "/users/tuser1"),
    F2 = nkdomain:find(root, U2Id),
    {ok, <<"user">>, U2Id, <<"/users/tuser1">>, Pid2} = F3 = nkdomain:load(root, "/users/tuser1"),
    F3 = nkdomain:find(root, U2Id),
    false = Pid1 == Pid2,

    % Update the user
    U2_Update = #{surname=><<"surname-1">>, password=><<"pass2">>},
    {ok, #{}} = cmd(Pid, user, update, #{id=><<"/users/tuser1">>, user=>U2_Update}),
    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := U2Id,
            <<"parent_id">> := <<"root">>,
            <<"path">> := <<"/users/tuser1">>,
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
    ok.


test_session1(Pid) ->
    % Do login over tuser1, check the session is created and loaded
    {ok, Pid2, SessId} = login("/users/tuser1", pass2),
    {ok, #{<<"type">>:=<<"user">>, <<"path">>:=<<"/users/tuser1">>, <<"obj_id">>:=UId}} = cmd(Pid2, user, get, #{}),
    {ok, #{<<"type">>:=<<"user">>, <<"path">>:=<<"/users/admin">>}} = cmd(Pid, user, get, #{}),
    {ok, <<"session">>, SessId, <<"/users/tuser1/sessions/", SessId/binary>>, SPid} = nkdomain:find(root, SessId),
    true = is_pid(SPid),

    % Object has active childs, we cannot delete it
    {error,
        {<<"object_has_childs">>,<<"Object has childs">>}} =
        cmd(Pid, user, delete, #{id=>UId}),

    % Get info about the session
    {ok, #{
        <<"type">> := <<"session">>,
        <<"obj_id">> := SessId,
        <<"parent_id">> := UId,
        <<"active">> := true,
        <<"created_time">> := _,
        <<"path">> := <<"/users/tuser1/sessions/", SessId/binary>>,
        <<"referred_id">> := UId,
        <<"session">> := #{
            <<"local">> := <<"ws:0.0.0.0:9202">>,
            <<"remote">> := <<"ws:127.0.0.1:", _/binary>>
        }
    }} =
        cmd(Pid2, session, get, #{}),

    % If we stop the WS connection, the session is stopped and destroyed, but found on archive
    % We must wait for it to be deleted and archived for the process to disappear
    nkapi_client:stop(Pid2),
    timer:sleep(1200),
    false = is_process_alive(SPid),
    {error, object_not_found} = nkdomain:find(root, SessId),
    {1, [S2]} = find_archive(SessId),
    #{
        <<"obj_id">>:=SessId,
        <<"destroyed_time">>:= T1,
        <<"destroyed_code">> := _
    } = S2,
    true = nklib_util:m_timestamp() - T1 < 5000,
    ok.


%% Find session and childs of admin user
test_session2(Pid) ->
    % Get the admin user (without id) and its current session
    {ok, #{<<"obj_id">>:=SessId}} = cmd(Pid, session, get, #{}),
    {ok, <<"session">>, SessId, Path, SessPid} = nkdomain:find(root, SessId),
    {ok, Childs} = nkdomain_obj:get_childs(<<"admin">>),
    SessName = nkdomain_util:name(SessId),
    SessId = maps:get(SessName, maps:get(<<"session">>, Childs)),

    % If we kill the session, admin notices and the web socket is closed
    exit(SessPid, kill),
    timer:sleep(100),
    {ok, Childs2} = nkdomain_obj:get_childs(<<"admin">>),

    Sessions = maps:get(<<"session">>, Childs2, #{}),
    false = maps:is_key(SessName, Sessions),
    {ok, <<"session">>, SessId, Path, undefined} = nkdomain:find(root, SessId),

    % Object has not active childs, but the child is still found on disk
    {error, object_has_childs} = nkdomain_obj:delete(<<"admin">>),

    % If we force a clean of the database, the stale object is deleted and archived
    {ok, #{active:=N}} = nkdomain_store:clean(root),
    true = N >= 1,
    {error, object_not_found} = nkdomain:find(root, SessId),
    % Archive has a 1-second refresh time
    timer:sleep(1100),
    {1, [#{<<"destroyed_code">>:=<<"object_clean_process">>}]} = find_archive(SessId),
    ok.


%% Test disabling users and sessions
test_session3(Admin) ->
    % Do login over tuser1, check the session is created and loaded
    {ok, Pid, SessId} = login("/users/tuser1", pass2),
    {ok, <<"session">>, SessId, <<"/users/tuser1/sessions/", SessId/binary>>, SPid} = nkdomain:find(root, SessId),
    true = is_pid(SPid),

    {ok, #{<<"path">>:=<<"/users/tuser1">>}} = cmd(Pid, user, get, #{}),
    {ok, #{}} = cmd(Pid, user, enable, #{enable=>false}),
    timer:sleep(1500),
    {error, object_not_found} = nkdomain:find(root, SessId),
    {1, [#{<<"destroyed_code">>:=<<"user_is_disabled">>}]} = find_archive(SessId),
    false = is_process_alive(Pid),
    false = is_process_alive(SPid),
    {error, {<<"object_is_disabled">>, _}} = login("/users/tuser1", pass2),

    {ok, #{}} = cmd(Admin, user, enable, #{id=>"/users/tuser1", enable=>true}),
    {ok, Pid3, _} = login("/users/tuser1", pass2),
    nkapi_client:stop(Pid3),
    timer:sleep(1500),
    ok.




test_delete(Pid) ->
    %% Delete the tuser1 object and find it in archive
    {ok, #{<<"obj_id">>:=ObjId}} = cmd(Pid, user, get, #{id=><<"/users/tuser1">>}),
    {ok, #{}} = cmd(Pid, user, delete, #{id=> <<"/users/tuser1">>}),

    timer:sleep(1100),
    {error,{<<"object_not_found">>,<<"Object not found">>}} =
        cmd(Pid, user, delete, #{id=> <<"/users/tuser1">>}),
    {1, [#{<<"path">>:=<<"/users/tuser1">>}]} = find_archive(ObjId),
    {_N, [#{<<"path">>:=<<"/users/tuser1">>}|_]} = find_archive(<<"/users/tuser1">>),
    ok.


%% Create domains and users
test_basic_2(Pid) ->

    % Create /stest1 and check we cannot create it again
    {ok, #{<<"obj_id">>:=S1Id, <<"path">>:=<<"/stest1">>}} =
        cmd(Pid, domain, create, #{obj_name=>stest1, description=><<"Test Sub1">>}),
    {error,{<<"object_already_exists">>, _}} =
        cmd(Pid, domain, create, #{obj_name=>stest1, description=><<"Test Sub1">>}),

    % Create /stest1/stest2 and check we cannot create a child with missing father
    {ok, #{<<"obj_id">>:=S2Id, <<"path">>:=<<"/stest1/stest2">>}} =
        cmd(Pid, domain, create, #{obj_name=>stest2, domain=>"/stest1", description=><<"Test Sub2">>}),
    {error,{<<"could_not_load_parent">>, <<"Object could not load parent '/stest2'">>}} =
        cmd(Pid, domain, create, #{obj_name=>stest2, domain=>"/stest2", description=><<"Test Sub2B">>}),

    % Check the created objects
    {ok,
        #{
            <<"type">> := <<"domain">>,
            <<"obj_id">> := S1Id,
            <<"path">> := <<"/stest1">>,
            <<"created_time">> := _CT1,
            <<"description">> := <<"Test Sub1">>,
            <<"parent_id">> := <<"root">>,
            <<"_is_enabled">> := true
        }} =
        cmd(Pid, domain, get, #{id=>"/stest1"}),

    {ok,
        #{
            <<"type">> := <<"domain">>,
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"created_time">> := CT2,
            <<"description">> := <<"Test Sub2">>,
            <<"parent_id">> := S1Id,
            <<"_is_enabled">> := true
        }} =
        cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),


    % Update /stest1/stest2
    {ok, #{}} = cmd(Pid, domain, update, #{id=>S2Id, description=><<"Test-Sub2">>}),
    {ok,
        #{
            <<"type">> := <<"domain">>,
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"created_time">> := CT2,
            <<"description">> := <<"Test-Sub2">>,
            <<"parent_id">> := S1Id
        }} =
        cmd(Pid, domain, get, #{id=>S2Id}),

    % Create /stest1/users/u1
    U1 = #{name=>n1, surname=>s1, email=>"u1@sub1"},
    {ok, #{<<"obj_id">>:=U1Id, <<"path">>:=<<"/stest1/users/u1">>}} =
        cmd(Pid, user, create, #{domain=>S1Id, obj_name=>u1, user=>U1}),
    {error,{<<"object_already_exists">>, _}} =
        cmd(Pid, user, create, #{domain=>S1Id, obj_name=>u1, user=>U1}),

    % Create /stest1/stest2/users/u1
    U2 = #{name=>n2, surname=>s2, email=>"n2@sub1.sub2"},
    {ok, #{<<"obj_id">>:=U2Id, <<"path">>:=<<"/stest1/stest2/users/u1">>}} =
        cmd(Pid, user, create, #{domain=>S2Id, obj_name=>u1, user=>U2}),
    {ok, _} = cmd(Pid, user, wait_for_save, #{id=>U2Id}),


    % Find types and childs on /stest1
    {ok, #{<<"data">> := #{<<"domain">> := 1,<<"user">> := 1},<<"total">> := 2}} =
        cmd(Pid, domain, find_types, #{id=><<"/stest1">>}),
    {ok,#{<<"data">> := #{<<"domain">> := 1,<<"user">> := 2},<<"total">> := 3}} =
        cmd(Pid, domain, find_all_types, #{id=><<"/stest1">>}),
    {ok, #{<<"total">> := 2, <<"data">> := [
        #{
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"type">> := <<"domain">>
        },
        #{
            <<"obj_id">> := U1Id,
            <<"path">> := <<"/stest1/users/u1">>,
            <<"type">> := <<"user">>
        }
    ]}} =
        cmd(Pid, domain, find_childs, #{id=><<"/stest1">>, sort=>[<<"path">>]}),
    {ok, #{<<"total">> := 3, <<"data">> := [
        #{
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"type">> := <<"domain">>
        },
        #{
            <<"obj_id">> := U2Id,
            <<"path">> := <<"/stest1/stest2/users/u1">>,
            <<"type">> := <<"user">>
        },
        #{
            <<"obj_id">> := U1Id,
            <<"path">> := <<"/stest1/users/u1">>,
            <<"type">> := <<"user">>
        }]}} =
        cmd(Pid, domain, find_all_childs, #{id=><<"/stest1">>, sort=>[<<"path">>]}),

    % We disable /stest1, all childs will be disabled (but stored property does change at childs)
    {ok, #{}} = cmd(Pid, domain, enable, #{id=><<"/stest1">>, enable=>false}),
    {ok, #{<<"enabled">>:=false, <<"_is_enabled">>:=false}} = cmd(Pid, domain, get, #{id=>"/stest1"}),
    {ok, #{<<"_is_enabled">>:=false}=S2_E1} = cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),
    false = maps:is_key(enabled, S2_E1),
    {ok, #{<<"_is_enabled">>:=false}=S1_U1} = cmd(Pid, domain, get, #{id=>"/stest1/users/u1"}),
    false = maps:is_key(enabled, S1_U1),
    {ok, #{<<"_is_enabled">>:=false}=S2_U1} = cmd(Pid, domain, get, #{id=>"/stest1/stest2/users/u1"}),
    false = maps:is_key(enabled, S2_U1),

    % This will not change the status (the stored status was already 'enabled')
    {ok, #{}} = cmd(Pid, domain, enable, #{id=><<"/stest1/stest2">>, enable=>true}),
    {ok, #{<<"_is_enabled">>:=false}=S2_E2} = cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),
    false = maps:is_key(enabled, S2_E2),

    % We disable by hand /stest1/stest2
    {ok, #{}} = cmd(Pid, domain, enable, #{id=><<"/stest1/stest2">>, enable=>false}),
    {ok, #{<<"_is_enabled">>:=false, <<"enabled">>:=false}} = cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),

    % When we activate again /stest1, /stest1/stest2 and childs remain disabled
    {ok, #{}} = cmd(Pid, domain, enable, #{id=><<"/stest1">>, enable=>true}),
    {ok, #{<<"enabled">>:=true, <<"_is_enabled">>:=true}} = cmd(Pid, domain, get, #{id=>"/stest1"}),
    {ok, #{<<"_is_enabled">>:=true}} = cmd(Pid, domain, get, #{id=>"/stest1/users/u1"}),
    {ok, #{<<"_is_enabled">>:=false, <<"enabled">>:=false}} = cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),
    {ok, #{<<"_is_enabled">>:=false}} = cmd(Pid, domain, get, #{id=>"/stest1/stest2/users/u1"}),

    % We unload everything, we loading, every enabled status remains
    true = is_loaded("/stest1"),
    true = is_loaded("/stest1/users/u1"),
    true = is_loaded("/stest1/stest2"),
    true = is_loaded("/stest1/stest2/users/u1"),
    nkdomain_obj:unload("/stest1", normal),

    timer:sleep(100),
    false = is_loaded("/stest1"),
    false = is_loaded("/stest1/users/u1"),
    false = is_loaded("/stest1/stest2"),
    false = is_loaded("/stest1/stest2/users/u1"),

    % This user will load everything on its branch
    {ok, #{<<"_is_enabled">>:=false}} = cmd(Pid, domain, get, #{id=>"/stest1/stest2/users/u1"}),
    true = is_loaded("/stest1/stest2/users/u1"),
    true = is_loaded("/stest1/stest2"),
    true = is_loaded("/stest1"),
    false = is_loaded("/stest1/users/u1"),

    {ok, #{<<"_is_enabled">>:=false, <<"enabled">>:=false}} = cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),
    {ok, #{<<"enabled">>:=true, <<"_is_enabled">>:=true}} = cmd(Pid, domain, get, #{id=>"/stest1"}),
    {ok, #{<<"_is_enabled">>:=true}} = cmd(Pid, domain, get, #{id=>"/stest1/users/u1"}),

    % We enable again /stest1/stest2
    {ok, #{}} = cmd(Pid, domain, enable, #{id=><<"/stest1/stest2">>, enable=>true}),
    {ok, #{<<"_is_enabled">>:=true, <<"enabled">>:=true}} = cmd(Pid, domain, get, #{id=>"/stest1/stest2"}),
    {ok, #{<<"_is_enabled">>:=true}} = cmd(Pid, domain, get, #{id=>"/stest1/users/u1"}),

    ok.



%% ===================================================================
%% Client fun
%% ===================================================================

remove_data() ->
    case nkdomain:find(root, "/users/tuser1") of
        {ok, <<"user">>, UId, <<"/users/tuser1">>, _} ->
            ok = nkdomain_store:delete(root, UId);
        {error, object_not_found} ->
            ok
    end,
    case nkdomain_domain_obj:find_childs(root, "/stest1", #{}) of
        {ok, 0, []} ->
            ok;
        _ ->
            %% lager:notice("Deleting all childs for /stest1"),
            nkdomain_store_es:object_store_delete_all_childs(root, "/stest1", #{})
    end,
    case nkdomain:find(root, "/stest1") of
        {ok, <<"domain">>, S1Id_0, <<"/stest1">>, _} ->
            %% lager:warning("/stest1 was already present"),
            ok = nkdomain_store:delete(root, S1Id_0);
        {error, object_not_found} ->
            ok
    end.


find_archive(Id) ->
    {ok, N, Data, _} = nkdomain_store:find_archive(root, #{filters=>#{id=>Id}, fields=><<"_all">>}),
    {N, Data}.


login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    case nkapi_client:start(root, ?WS, Login, Fun, #{}) of
        {ok, SessId, Pid, _Reply} -> {ok, Pid, SessId};
        {error, Error} -> {error, Error}
    end.


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


is_loaded(Id) ->
    case nkdomain_obj_lib:find_loaded(Id) of
        #obj_id_ext{} -> true;
        _ -> false
    end.
