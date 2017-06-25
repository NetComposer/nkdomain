-module(nkdomain_test).
-compile(export_all).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(WS, "ws://127.0.0.1:9304/s/_api/ws").
-define(SRV, sipstorm_v01).
-define(ADMIN_PASS, "netcomposer").





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
            <<"domain_id">> := <<"root">>,
            <<"path">> := <<"/users/admin">>,
            <<"created_time">> := _,
            <<"user">> := #{
                <<"name">> := _,
                <<"password">> := Pass,
                <<"surname">> := _
            }
        } = U1} =
        cmd(Pid, <<"objects/user/get">>, #{}),
    Pass = nkdomain_user_obj:user_pass(?ADMIN_PASS),
    {ok, U1} = cmd(Pid, <<"objects/user/get">>, #{id=><<"admin">>}),
    {ok, U1} = cmd(Pid, <<"objects/user/get">>, #{id=><<"/users/admin">>}),
    {error, {<<"object_not_found">>,<<"Object not found">>}} = cmd(Pid, <<"objects/user/get">>, #{id=><<"admin2">>}),

    % Get root domain
    {ok,
        #{
            <<"type">> := ?DOMAIN_DOMAIN,
            <<"obj_id">> := <<"root">>,
            <<"domain_id">> := <<>>,
            <<"path">> := <<"/">>,
            <<"created_time">> := _,
            <<"description">> := _
        } = D1} =
        cmd(Pid, <<"objects/domain/get">>, #{}),
    {ok, D1} = cmd(Pid, <<"objects/domain/get">>, #{id=><<"root">>}),
    {ok, D1} = cmd(Pid, <<"objects/domain/get">>, #{id=><<"/">>}),
    ok.


test_create_user(Pid) ->
    {error, object_not_found} = nkdomain:find(?SRV, "/users/tuser1"),
    {error, object_not_found} = nkdomain:load(?SRV, "/users/tuser1"),

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
    {ok, #{<<"obj_id">>:=U2Id, <<"path">>:=<<"/users/tuser1">>}} = cmd(Pid, <<"objects/user/create">>, U2_Create),
    {error, {<<"email_duplicated">>, _}} = cmd(Pid, <<"objects/user/create">>, U2_Create),

    % Check user, creation date and password
    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := U2Id,
            <<"domain_id">> := <<"root">>,
            <<"path">> := <<"/users/tuser1">>,
            <<"created_time">> := CT,
            <<"user">> := #{
                <<"name">> := <<"user1">>,
                <<"surname">> := <<"surname1">>,
                <<"email">> := <<"user1@root">>,
                <<"password">> := P2

            }
        } = U2} =
        cmd(Pid, <<"objects/user/get">>, #{id=>U2Id}),
    true = nkdomain_util:timestamp() - CT < 500,
    P2 = nkdomain_user_obj:user_pass("pass1"),

    % Find user by several ways
    {ok, U2} = cmd(Pid, <<"objects/user/get">>, #{id=><<"/users/tuser1">>}),
    {ok, <<"user">>, U2Id, <<"/users/tuser1">>, Pid1} = F1 = nkdomain:find(?SRV, "/users/tuser1"),
    F1 = nkdomain:find(?SRV, U2Id),
    F1 = nkdomain:load(?SRV, "/users/tuser1"),
    F1 = nkdomain:load(?SRV, U2Id),

    % Unload the user
    ok = nkdomain:unload(?SRV, U2Id, normal),
    timer:sleep(100),
    {ok, <<"user">>, U2Id, <<"/users/tuser1">>, undefined} = F2 = nkdomain:find(?SRV, "/users/tuser1"),
    F2 = nkdomain:find(?SRV, U2Id),
    {ok, <<"user">>, U2Id, <<"/users/tuser1">>, Pid2} = F3 = nkdomain:load(?SRV, "/users/tuser1"),
    F3 = nkdomain:find(?SRV, U2Id),
    false = Pid1 == Pid2,

    % Update the user
    U2_Update = #{surname=><<"surname-1">>, password=><<"pass2">>},
    {ok, #{}} = cmd(Pid, <<"objects/user/update">>, #{id=><<"/users/tuser1">>, user=>U2_Update}),
    {ok,
        #{
            <<"type">> := <<"user">>,
            <<"obj_id">> := U2Id,
            <<"domain_id">> := <<"root">>,
            <<"path">> := <<"/users/tuser1">>,
            <<"created_time">> := CT,
            <<"user">> := #{
                <<"name">> := <<"user1">>,
                <<"surname">> := <<"surname-1">>,
                <<"email">> := <<"user1@root">>,
                <<"password">> := P3

            }
        }} =
        cmd(Pid, <<"objects/user/get">>, #{id=>U2Id}),
    P3 = nkdomain_user_obj:user_pass("pass2"),
    ok.


test_session1(Pid) ->
    % Do login over tuser1, check the session is created and loaded
    {ok, Pid2, SessId} = login("/users/tuser1", pass2),
    {ok, #{<<"type">>:=<<"user">>, <<"path">>:=<<"/users/tuser1">>, <<"obj_id">>:=UId}} = cmd(Pid2, <<"objects/user/get">>, #{}),
    {ok, #{<<"type">>:=<<"user">>, <<"path">>:=<<"/users/admin">>}} = cmd(Pid, <<"objects/user/get">>, #{}),
    {ok, <<"session">>, SessId, <<"/sessions/", _/binary>>, SPid} = nkdomain:find(?SRV, SessId),
    true = is_pid(SPid),

    % Object has active childs, we cannot delete it
    {error,
        {<<"object_has_childs">>,<<"Object has childs">>}} =
        cmd(Pid, <<"objects/user/delete">>, #{id=>UId}),

    timer:sleep(500),

    % Get info about the session
    {ok, #{
        <<"type">> := <<"session">>,
        <<"obj_id">> := SessId,
        <<"parent_id">> := UId,
        <<"active">> := true,
        <<"created_time">> := _,
        <<"path">> := <<"/sessions/", _/binary>>,
        <<"created_by">> := UId,
        <<"session">> := #{
            <<"local">> := <<"ws:0.0.0.0:", _/binary>>,
            <<"remote">> := <<"ws:127.0.0.1:", _/binary>>
        }
    }} =
        cmd(Pid2, <<"objects/session/get">>, #{}),

    % If we stop the WS connection, the session is stopped and destroyed, but found on archive
    % We must wait for it to be deleted and archived for the process to disappear
    nkapi_client:stop(Pid2),
    timer:sleep(1200),
    false = is_process_alive(SPid),
    {error, object_not_found} = nkdomain:find(?SRV, SessId),
%%    {1, [S2]} = find_archive(SessId),
%%    #{
%%        <<"obj_id">>:=SessId,
%%        <<"destroyed_time">>:= T1,
%%        <<"destroyed_code">> := _
%%    } = S2,
%%    true = nkdomain_util:timestamp() - T1 < 5000,
    ok.


%% Find session and childs of admin user
test_session2(Pid) ->
    % Get the admin user (without id) and its current session
    {ok, #{<<"obj_id">>:=SessId}} = cmd(Pid, <<"objects/session/get">>, #{}),
    {ok, <<"session">>, SessId, Path, SessPid} = nkdomain:find(?SRV, SessId),
    {ok, Childs} = nkdomain_obj:sync_op(?SRV, <<"admin">>, get_childs),
    true = maps:is_key(SessId, Childs),

    % If we kill the session, admin notices and the web socket is closed
    % We link with admin so that it is not unloaded after stopping its childs for 10 secs
    spawn_link(
        fun() ->
            ok = nkdomain_obj:sync_op(?SRV, <<"admin">>, {register, usage, {please_dont_stop, self()}}),
            timer:sleep(10000)
        end),
    timer:sleep(50),
    exit(SessPid, kill),
    timer:sleep(100),
    {ok, Childs2} = nkdomain_obj:sync_op(?SRV, <<"admin">>, get_childs),
    false = maps:is_key(SessId, Childs2),
    {ok, <<"session">>, SessId, Path, undefined} = nkdomain:find(?SRV, SessId),

    % If we force a clean of the database, the stale object is deleted and archived
    {ok, #{inactive:=N}} = nkdomain:clean(?SRV),
    true = N >= 1,
    {error, object_not_found} = nkdomain:find(?SRV, SessId),
    % Archive has a 1-second refresh time
%%    timer:sleep(1100),
%%    {1, [#{<<"destroyed_code">>:=<<"object_clean_process">>}]} = find_archive(SessId),
    ok.


%% Test disabling users and sessions
test_session3(Admin) ->
    % Do login over tuser1, check the session is created and loaded
    {ok, Pid, SessId} = login("/users/tuser1", pass2),
    {ok, <<"session">>, SessId, <<"/sessions/", _/binary>>, SPid} = nkdomain:find(?SRV, SessId),
    true = is_pid(SPid),

    {ok, #{<<"path">>:=<<"/users/tuser1">>}} = cmd(Pid, <<"objects/user/get">>, #{}),
    {ok, #{}} = cmd(Pid, <<"objects/user/enable">>, #{enable=>false}),
    timer:sleep(1500),
    {error, object_not_found} = nkdomain:find(?SRV, SessId),
%%    {1, [#{<<"destroyed_code">>:=<<"session_is_disabled">>}]} = find_archive(SessId),
    false = is_process_alive(Pid),
    false = is_process_alive(SPid),
    {error, {<<"object_is_disabled">>, _}} = login("/users/tuser1", pass2),

    {ok, #{}} = cmd(Admin, <<"objects/user/enable">>, #{id=>"/users/tuser1", enable=>true}),
    {ok, Pid3, _} = login("/users/tuser1", pass2),
    nkapi_client:stop(Pid3),
    timer:sleep(1500),
    ok.




test_delete(Pid) ->
    %% Delete the tuser1 object and find it in archive
    {ok, #{<<"obj_id">>:=_ObjId}} = cmd(Pid, <<"objects/user/get">>, #{id=><<"/users/tuser1">>}),
    {ok, #{}} = cmd(Pid, <<"objects/user/delete">>, #{id=> <<"/users/tuser1">>}),

    timer:sleep(1100),
    {error,{<<"object_not_found">>,<<"Object not found">>}} =
        cmd(Pid, <<"objects/user/delete">>, #{id=> <<"/users/tuser1">>}),
%%    {1, [#{<<"path">>:=<<"/users/tuser1">>}]} = find_archive(ObjId),
%%    {_N, [#{<<"path">>:=<<"/users/tuser1">>}|_]} = find_archive(<<"/users/tuser1">>),
    ok.


%% Create domains and users
test_basic_2(Pid) ->

    % Create /stest1 and check we cannot create it again
    {ok, #{<<"obj_id">>:=S1Id, <<"path">>:=<<"/stest1">>}} =
        cmd(Pid, <<"objects/domain/create">>, #{obj_name=>stest1, description=><<"Test Sub1">>, domain=>#{}}),
    {error,{<<"object_already_exists">>, _}} =
        cmd(Pid, <<"objects/domain/create">>, #{obj_name=>stest1, description=><<"Test Sub1">>, domain=>#{}}),

    % Create /stest1/stest2 and check we cannot create a child with missing father
    {ok, #{<<"obj_id">>:=S2Id, <<"path">>:=<<"/stest1/stest2">>}} =
        cmd(Pid, <<"objects/domain/create">>, #{obj_name=>stest2, domain_id=>"/stest1", description=><<"Test Sub2">>, domain=>#{}}),
    {error,{<<"could_not_load_domain">>, <<"Object could not load domain '/stest2'">>}} =
        cmd(Pid, <<"objects/domain/create">>, #{obj_name=>stest2, domain_id=>"/stest2", description=><<"Test Sub2B">>, domain=>#{}}),

    % Check the created objects
    {ok,
        #{
            <<"type">> := ?DOMAIN_DOMAIN,
            <<"obj_id">> := S1Id,
            <<"path">> := <<"/stest1">>,
            <<"created_time">> := _CT1,
            <<"description">> := <<"Test Sub1">>,
            <<"domain_id">> := <<"root">>,
            <<"_is_enabled">> := true
        }} =
        cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1"}),

    {ok,
        #{
            <<"type">> := ?DOMAIN_DOMAIN,
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"created_time">> := CT2,
            <<"description">> := <<"Test Sub2">>,
            <<"domain_id">> := S1Id,
            <<"_is_enabled">> := true
        }} =
        cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),


    % Update /stest1/stest2
    {ok, #{}} = cmd(Pid, <<"objects/domain/update">>, #{id=>S2Id, description=><<"Test-Sub2">>}),
    {ok,
        #{
            <<"type">> := ?DOMAIN_DOMAIN,
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"created_time">> := CT2,
            <<"description">> := <<"Test-Sub2">>,
            <<"domain_id">> := S1Id
        }} =
        cmd(Pid, <<"objects/domain/get">>, #{id=>S2Id}),

    % Create /stest1/users/u1
    U1 = #{name=>n1, surname=>s1, email=>"u1@sub1"},
    {ok, #{<<"obj_id">>:=U1Id, <<"path">>:=<<"/stest1/users/u1">>}} =
        cmd(Pid, <<"objects/user/create">>, #{domain_id=>S1Id, obj_name=>u1, user=>U1}),
    {error,{<<"email_duplicated">>, _}} =
        cmd(Pid, <<"objects/user/create">>, #{domain_id=>S1Id, obj_name=>u1, user=>U1}),
    {error,{<<"object_already_exists">>, _}} =
        cmd(Pid, <<"objects/user/create">>, #{domain_id=>S1Id, obj_name=>u1, user=>U1#{email:="kk"}}),

    % Create /stest1/stest2/users/u1
    U2 = #{name=>n2, surname=>s2, email=>"n2@sub1.sub2"},
    {ok, #{<<"obj_id">>:=U2Id, <<"path">>:=<<"/stest1/stest2/users/u1">>}} =
        cmd(Pid, <<"objects/user/create">>, #{domain_id=>S2Id, obj_name=>u1, user=>U2}),
    % {ok, _} = cmd(Pid, <<"objects/user/wait_for_save">>, #{id=>U2Id}),


    % Find types and childs on /stest1
    {ok, #{<<"data">> := #{?DOMAIN_DOMAIN := 1,<<"user">> := 1},<<"total">> := 2}} =
        cmd(Pid, <<"objects/domain/find_types">>, #{id=><<"/stest1">>}),
    {ok,#{<<"data">> := #{?DOMAIN_DOMAIN := 1,<<"user">> := 2},<<"total">> := 3}} =
        cmd(Pid, <<"objects/domain/find_all_types">>, #{id=><<"/stest1">>}),
    {ok, #{<<"total">> := 2, <<"data">> := [
        #{
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"type">> := ?DOMAIN_DOMAIN
        },
        #{
            <<"obj_id">> := U1Id,
            <<"path">> := <<"/stest1/users/u1">>,
            <<"type">> := <<"user">>
        }
    ]}} =
        cmd(Pid, <<"objects/domain/find_childs">>, #{id=><<"/stest1">>, sort=>[<<"path">>]}),
    {ok, #{<<"total">> := 3, <<"data">> := [
        #{
            <<"obj_id">> := S2Id,
            <<"path">> := <<"/stest1/stest2">>,
            <<"type">> := ?DOMAIN_DOMAIN
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
        cmd(Pid, <<"objects/domain/find_all_childs">>, #{id=><<"/stest1">>, sort=>[<<"path">>]}),

    % We disable /stest1, all childs will be disabled (but stored property does change at childs)
    {ok, #{}} = cmd(Pid, <<"objects/domain/enable">>, #{id=><<"/stest1">>, enable=>false}),
    {ok, #{<<"enabled">>:=false, <<"_is_enabled">>:=false}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1"}),
    {ok, #{<<"_is_enabled">>:=false}=S2_E1} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),
    false = maps:is_key(enabled, S2_E1),
    {ok, #{<<"_is_enabled">>:=false}=S1_U1} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/users/u1"}),
    false = maps:is_key(enabled, S1_U1),
    {ok, #{<<"_is_enabled">>:=false}=S2_U1} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/stest2/users/u1"}),
    false = maps:is_key(enabled, S2_U1),

    % This will not change the status (the stored status was already 'enabled')
    {ok, #{}} = cmd(Pid, <<"objects/domain/enable">>, #{id=><<"/stest1/stest2">>, enable=>true}),
    {ok, #{<<"_is_enabled">>:=false}=S2_E2} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),
    false = maps:is_key(enabled, S2_E2),

    % We disable by hand /stest1/stest2
    {ok, #{}} = cmd(Pid, <<"objects/domain/enable">>, #{id=><<"/stest1/stest2">>, enable=>false}),
    {ok, #{<<"_is_enabled">>:=false, <<"enabled">>:=false}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),

    % When we activate again /stest1, /stest1/stest2 and childs remain disabled
    {ok, #{}} = cmd(Pid, <<"objects/domain/enable">>, #{id=><<"/stest1">>, enable=>true}),
    {ok, #{<<"enabled">>:=true, <<"_is_enabled">>:=true}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1"}),
    {ok, #{<<"_is_enabled">>:=true}} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/users/u1"}),
    {ok, #{<<"_is_enabled">>:=false, <<"enabled">>:=false}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),
    {ok, #{<<"_is_enabled">>:=false}} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/stest2/users/u1"}),

    true = is_loaded("/stest1"),
    true = is_loaded("/stest1/users/u1"),
    true = is_loaded("/stest1/stest2"),
    true = is_loaded("/stest1/stest2/users/u1"),

    % We unload everything. We need to unload childs before or they will restart father
    nkdomain:unload(?SRV, "/stest1/stest2/users/u1", normal),
    nkdomain:unload(?SRV, "/stest1/stest2", normal),
    nkdomain:unload(?SRV, "/stest1/users/u1", normal),
    nkdomain:unload(?SRV, "/stest1", normal),
    timer:sleep(100),
    false = is_loaded("/stest1"),
    false = is_loaded("/stest1/users/u1"),
    false = is_loaded("/stest1/stest2"),
    false = is_loaded("/stest1/stest2/users/u1"),

    % This user will load everything on its branch
    {ok, #{<<"_is_enabled">>:=false}} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/stest2/users/u1"}),
    true = is_loaded("/stest1/stest2/users/u1"),
    true = is_loaded("/stest1/stest2"),
    true = is_loaded("/stest1"),
    false = is_loaded("/stest1/users/u1"),

    {ok, #{<<"_is_enabled">>:=false, <<"enabled">>:=false}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),
    {ok, #{<<"enabled">>:=true, <<"_is_enabled">>:=true}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1"}),
    {ok, #{<<"_is_enabled">>:=true}} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/users/u1"}),

    % We enable again /stest1/stest2
    {ok, #{}} = cmd(Pid, <<"objects/domain/enable">>, #{id=><<"/stest1/stest2">>, enable=>true}),
    {ok, #{<<"_is_enabled">>:=true, <<"enabled">>:=true}} = cmd(Pid, <<"objects/domain/get">>, #{id=>"/stest1/stest2"}),
    {ok, #{<<"_is_enabled">>:=true}} = cmd(Pid, <<"objects/user/get">>, #{id=>"/stest1/users/u1"}),

    ok.



%% ===================================================================
%% Client fun
%% ===================================================================

remove_data() ->
    case nkdomain:find(?SRV, "/users/tuser1") of
        {ok, <<"user">>, UId, <<"/users/tuser1">>, _} ->
            {ok, _} = ?SRV:object_db_delete(?SRV, UId);
        {error, object_not_found} ->
            ok
    end,
    case nkdomain_domain_obj:find_childs(?SRV, "/stest1", #{}) of
        {ok, 0, []} ->
            ok;
        _ ->
            %% lager:notice("Deleting all childs for /stest1"),
            nkdomain:delete_all_childs(?SRV, "/stest1")
    end,
    case nkdomain:find(?SRV, "/stest1") of
        {ok, ?DOMAIN_DOMAIN, S1Id_0, <<"/stest1">>, _} ->
            %% lager:warning("/stest1 was already present"),
            ok = nkdomain:delete(?SRV, S1Id_0);
        {error, object_not_found} ->
            ok
    end.


%%find_archive(Id) ->
%%    Filter = case nkdomain_util:is_path(Id) of
%%        {true, Path} -> #{path => Path};
%%        false -> #{obj_id => Id}
%%    end,
%%    {ok, N, Data, _} = nkdomain_store:find_archive(?SRV, #{filters=>Filter}),
%%    {N, Data}.


login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    case nkapi_client:start(?SRV, ?WS, Login, Fun, #{}, <<"objects/user/login">>) of
        {ok, #{<<"session_id">>:=SessId}, Pid} -> {ok, Pid, SessId};
        {error, Error} -> {error, Error}
    end.


api_client_fun(#nkreq{cmd = <<"event">>, data=Event}, UserData) ->
    lager:notice("CLIENT event ~p", [lager:pr(Event, nkevent)]),
    {ok, UserData};

api_client_fun(_Req, UserData) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.

get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.


cmd(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, Cmd, Data).


is_loaded(Id) ->
    case nkdomain_lib:find_loaded(?SRV, Id) of
        #obj_id_ext{} -> true;
        _ -> false
    end.



%% ===================================================================
%% Parse
%% ===================================================================

p1() ->
    Obj = #{
        type => user,
        obj_id => a,
        path => "/",
        domain_id => p1,
        created_time => 0,
        <<"user">> => #{name => n1}
    },
    nkdomain_callbacks:object_parse(?SRV, load, Obj).