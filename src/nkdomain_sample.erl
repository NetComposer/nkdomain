-module(nkdomain_sample).
-compile(export_all).

-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkmail/include/nkmail.hrl").

-define(WS, "ws://127.0.0.1:9301/api/ws").
%%-define(WS, "wss://v1.netc.io/netcomp/v01/api/ws").


login() ->
    login(admin, "1234").

login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, _Reply, _Pid} = nkapi_client:start(root, ?WS, Login, Fun, #{}, <<"objects/user/login">>).


event_get_subs() ->
    cmd(<<"event/get_subscriptions">>, #{}).

event_subscribe(ObjType, EvType, ObjId) ->
    cmd(<<"event/subscribe">>, #{class=>domain, subclass=>ObjType, type=>EvType, obj_id=>ObjId}).

event_unsubscribe(ObjType, EvType, ObjId) ->
    cmd(<<"event/unsubscribe">>, #{class=>domain, subclass=>ObjType, type=>EvType, obj_id=>ObjId}).


user_get() ->
    cmd(<<"objects/user/get">>, #{unknown=>1}).

user_get(Id) ->
    cmd(<<"objects/user/get">>, #{id=>Id}).

user_create(Domain, Name, Surname) ->
    Data = #{
        obj_name => to_bin(Name),
        parent_id => to_bin(Domain),
        user => #{
            name => to_bin(Name),
            surname => to_bin(Surname),
            password => <<"1234">>
        }
    },
    case cmd(<<"objects/user/create">>, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.

user_create(Domain, Name, Surname, Password, Email) ->
    user_create(Domain, Name, Password, Name, Surname, Email).

user_create(Domain, ObjName, Password, Name, Surname, Email) ->
    Data = #{
        obj_name => to_bin(ObjName),
        parent_id => to_bin(Domain),
        user => #{
            name => to_bin(Name),
            password => to_bin(Password),
            surname => to_bin(Surname),
            email => to_bin(Email)
        }
    },
    case cmd(<<"objects/user/create">>, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.

user_create2(Domain, Name, Surname, Avatar, Phone, Address) ->
    Data = #{
        obj_name => to_bin(Name),
        parent_id => to_bin(Domain),
        user => #{
            name => to_bin(Name),
            surname => to_bin(Surname),
            avatar_t => to_bin(Avatar),
            phone_t => to_bin(Phone),
            address_t => to_bin(Address)
        }
    },
    case cmd(<<"objects/user/create">>, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.




user_delete(Id) ->
    cmd(<<"objects/user/delete">>, #{id=>to_bin(Id)}).


user_update(Id, Name, Password, Email) ->
    Data = #{
        id => to_bin(Id),
        user => #{
            name => to_bin(Name),
            password => Password,
            email => Email
        }
    },
    cmd(<<"objects/user/update">>, Data).

user_update(Id, Name) ->
    Data = #{
        id => to_bin(Id),
        user => #{
            name => to_bin(Name)
        }
    },
    cmd(<<"objects/user/update">>, Data).


user_make_token() ->
    cmd(<<"objects/user/make_token">>, #{ttl=>30}).





domain_get() ->
    cmd(<<"objects/domain/get">>, #{}).

domain_get(Id) ->
    cmd(<<"objects/domain/get">>, #{id=>Id}).

domain_create(Domain, Name, Desc) ->
    Data = #{
        obj_name => Name,
        parent_id => Domain,
        description => Desc,
        domain => #{}
    },
    case cmd(<<"objects/domain/create">>, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.


domain_delete(Id) ->
    cmd(<<"objects/domain/delete">>, #{id=>to_bin(Id)}).


domain_update(Id, Name, Desc) ->
    Data = #{
        id => to_bin(Id),
        name => Name,
        description => Desc
    },
    cmd(<<"objects/domain/update">>, Data).


domain_find() ->
    cmd(<<"objects/domain/find">>, #{}).

domain_find(Id) ->
    cmd(<<"objects/domain/find">>, #{id=>Id}).

domain_find(Id, Spec) ->
    cmd(<<"objects/domain/find">>, Spec#{id=>Id}).

domain_find_all() ->
    cmd(<<"objects/domain/find_all">>, #{}).

domain_find_all(Id) ->
    cmd(<<"objects/domain/find_all">>, #{id=>Id}).

domain_find_all(Id, Spec) ->
    cmd(<<"objects/domain/find_all">>, Spec#{id=>Id}).

domain_find_types() ->
    cmd(<<"objects/domain/find_types">>, #{}).

domain_find_types(Id) ->
    cmd(<<"objects/domain/find_types">>, #{id=>Id}).

domain_find_types(Id, Spec) ->
    cmd(<<"objects/domain/find_types">>, Spec#{id=>Id}).

domain_find_all_types() ->
    cmd(<<"objects/domain/find_all_types">>, #{}).

domain_find_all_types(Id) ->
    cmd(<<"objects/domain/find_all_types">>, #{id=>Id}).

domain_find_all_types(Id, Spec) ->
    cmd(<<"objects/domain/find_all_types">>, Spec#{id=>Id}).

domain_find_childs() ->
    cmd(<<"objects/domain/find_childs">>, #{}).

domain_find_childs(Id) ->
    cmd(<<"objects/domain/find_childs">>, #{id=>Id}).

domain_find_childs(Id, Spec) ->
    cmd(<<"objects/domain/find_childs">>, Spec#{id=>Id}).

domain_find_all_childs() ->
    cmd(<<"objects/domain/find_all_childs">>, #{}).

domain_find_all_childs(Id) ->
    cmd(<<"objects/domain/find_all_childs">>, #{id=>Id}).

domain_find_all_childs(Id, Spec) ->
    cmd(<<"objects/domain/find_all_childs">>, Spec#{id=>Id}).


domain_find_all_users() ->
    cmd(<<"objects/domain/find_all_childs">>, #{type=>user}).


session_get() ->
    cmd(<<"objects/session/get">>, #{}).

session_delete() ->
    cmd(<<"objects/session/delete">>, #{}).


config_create(Sub, Parent, Config) ->
    cmd(<<"objects/config/create">>, #{subtype=>Sub, parent=>Parent, config=>Config}).

config_get(Id) ->
    cmd(<<"objects/config/get">>, #{id=>Id}).

config_update(Id, Config) ->
    cmd(<<"objects/config/update">>, #{id=>Id, config=>Config}).

config_delete(Id, Reason) ->
    cmd(<<"objects/config/update">>, #{id=>Id, reason=>Reason}).

config_find(SubType, Parent) ->
    cmd(<<"objects/config/find">>, #{parent=>Parent, subtype=>SubType}).




%% ===================================================================
%% Client fun
%% ===================================================================


api_client_fun(#nkreq{cmd = <<"event">>, data=Event}, UserData) ->
    lager:warning("CLIENT event ~p", [lager:pr(Event, nkevent)]),
    {ok, UserData};

api_client_fun(_Req, UserData) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.

get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.


%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Cmd, Data) ->
    Pid = get_client(),
    cmd(Pid, Cmd, Data).

cmd(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, Cmd, Data).




%% ===================================================================
%% OBJECTS
%% ===================================================================



to_bin(R) -> nklib_util:to_binary(R).