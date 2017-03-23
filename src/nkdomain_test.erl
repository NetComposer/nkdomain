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

sub1_create() ->
    {ok, Obj} = nkdomain_obj_lib:make_obj(
        root,
        domain,
        "/",
        #{description => <<"Sub1">>},
        #{obj_id=><<"sub1">>}),
    nkdomain_obj:create(root, Obj, #{}).


sub2_create() ->
    {ok, Obj} = nkdomain_obj_lib:make_obj(
        root,
        domain,
        "/sub1",
        #{description => <<"Sub2">>},
        #{obj_id=><<"sub2">>}),
    nkdomain_obj:create(root, Obj, #{}).





root_user_create(Name, SurName) ->
    {ok, Obj} = nkdomain_obj_lib:make_obj(
        root,
        user,
        <<"root">>,
        #{user => #{name=>Name, surname=>SurName}},
        #{name=>Name}),
    nkdomain_obj:create(root, Obj, #{}).


sub2_user_create(Name, SurName) ->
    {ok, Obj} = nkdomain_obj_lib:make_obj(
        root,
        user,
        <<"/sub1/sub2">>,
        #{user => #{name=>Name, surname=>SurName}},
        #{name=>Name}),
    nkdomain_obj:create(root, Obj, #{}).


to_bin(R) -> nklib_util:to_binary(R).