-module(nkdomain_test).
-compile(export_all).

-include_lib("nkapi/include/nkapi.hrl").

-define(WS, "ws://127.0.0.1:9202/api/ws").


login() ->
    login(admin, "1234").

login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, _SessId, _Pid, _Reply} = nkapi_client:start(root, ?WS, Login, Fun, #{}).


user_create(Path, Name, Surname) ->
    Data = #{
        user => #{
            name => to_bin(Name),
            surname => to_bin(Surname)
        },
        path => to_bin(Path),
        aliases => [alias1, alias2]
    },
    case cmd(user, create, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.


user_delete(Id) ->
    cmd(user, delete, #{id=>to_bin(Id)}).


user_update(Id, Name, Password, Aliases) ->
    Data = #{
        id => to_bin(Id),
        user => #{
            name => to_bin(Name),
            password => Password
        },
        aliases => Aliases
    },
    cmd(user, update, Data).


domain_create(Path, Desc) ->
    Data = #{
        path => to_bin(Path),
        description => Desc,
        aliases => [dom1, dom2]
    },
    case cmd(domain, create, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.


domain_delete(Id) ->
    cmd(domain, delete, #{id=>to_bin(Id)}).


domain_update(Id, Desc, Aliases) ->
    Data = #{
        id => to_bin(Id),
        description => Desc,
        aliases => Aliases
    },
    cmd(domain, update, Data).


domain_get_types(Id) ->
    cmd(domain, get_types, #{id=>Id}).


domain_get_all_types(Id) ->
    cmd(domain, get_all_types, #{id=>Id}).




%% ===================================================================
%% Client fun
%% ===================================================================


api_client_fun(#nkapi_req{class=event, data=Event}, UserData) ->
    lager:notice("CLIENT event ~p", [lager:pr(Event, nkservice_events)]),
    {ok, UserData};

api_client_fun(_Req, UserData) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.

get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.


%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Class, Cmd, Data) ->
    Pid = get_client(),
    cmd(Pid, Class, Cmd, Data).

cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).




%% ===================================================================
%% OBJECTS
%% ===================================================================


root_create() ->
    Obj = #{
        path => <<"/">>,
        type => <<"domain">>,
        description => <<"NetComposer">>
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"root">>}).


sub1_create() ->
    Obj = #{
        path => <<"/sub1">>,
        type => <<"domain">>,
        description => <<"Sub1">>
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"sub1">>}).


sub2_create() ->
    Obj = #{
        path => <<"/sub1/sub2">>,
        type => <<"domain">>,
        description => <<"Sub2">>
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"sub2">>}).


admin_create() ->
    Obj = #{
        path => <<"/users/admin">>,
        type => <<"user">>,
        description => <<"Admin User">>,
        user => #{
            name => <<"Admin">>,
            surname => <<"User">>,
            password => "1234"
        }
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"admin">>}).


user2_create() ->
    Obj = #{
        path => <<"/users/u2">>,
        type => <<"user">>,
        description => <<"User 2">>,
        aliases => <<"user2@domain.com">>,
        user => #{
            name => <<"Name 2">>,
            surname => <<"Surname 2">>,
            password => "1234"
        }
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"user2">>}).


user3_create() ->
    Obj = #{
        path => <<"/sub1/users/u3">>,
        type => <<"user">>,
        description => <<"User 3">>,
        aliases => <<"user3@domain.com">>,
        user => #{
            name => <<"Name 3">>,
            surname => <<"Surname 3">>,
            password => "4321"
        }
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"user3">>}).


%% @private
to_bin(Term) -> nklib_util:to_binary(Term).