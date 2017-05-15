-module(nkdomain_sample).
-compile(export_all).

-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkmail/include/nkmail.hrl").

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


event_get_subs() ->
    cmd(event, get_subscriptions, #{}).

event_subscribe(ObjType, EvType, ObjId) ->
    cmd(event, subscribe, #{class=>domain, subclass=>ObjType, type=>EvType, obj_id=>ObjId}).

event_unsubscribe(ObjType, EvType, ObjId) ->
    cmd(event, unsubscribe, #{class=>domain, subclass=>ObjType, type=>EvType, obj_id=>ObjId}).


user_get() ->
    cmd(user, get, #{}).

user_get(Id) ->
    cmd(user, get, #{id=>Id}).

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
    case cmd(user, create, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.

user_create(Domain, Name, Password, Surname, Email) ->
    Data = #{
        obj_name => to_bin(Name),
        parent_id => to_bin(Domain),
        user => #{
            name => to_bin(Name),
            password => to_bin(Password),
            surname => to_bin(Surname),
            email => to_bin(Email)
        }
    },
    case cmd(user, create, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.

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
    case cmd(user, create, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.

user_delete(Id) ->
    cmd(user, delete, #{id=>to_bin(Id)}).


user_update(Id, Name, Password, Email) ->
    Data = #{
        id => to_bin(Id),
        user => #{
            name => to_bin(Name),
            password => Password,
            email => Email
        }
    },
    cmd(user, update, Data).


user_make_token() ->
    cmd(user, make_token, #{ttl=>1}).





domain_get() ->
    cmd(domain, get, #{}).

domain_get(Id) ->
    cmd(domain, get, #{id=>Id}).

domain_create(Domain, Name, Desc) ->
    Data = #{
        obj_name => Name,
        parent_id => Domain,
        description => Desc,
        domain => #{}
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


domain_find() ->
    cmd(domain, find, #{}).

domain_find(Id) ->
    cmd(domain, find, #{id=>Id}).

domain_find(Id, Spec) ->
    cmd(domain, find, Spec#{id=>Id}).

domain_find_all() ->
    cmd(domain, find_all, #{}).

domain_find_all(Id) ->
    cmd(domain, find_all, #{id=>Id}).

domain_find_all(Id, Spec) ->
    cmd(domain, find_all, Spec#{id=>Id}).

domain_find_types() ->
    cmd(domain, find_types, #{}).

domain_find_types(Id) ->
    cmd(domain, find_types, #{id=>Id}).

domain_find_types(Id, Spec) ->
    cmd(domain, find_types, Spec#{id=>Id}).

domain_find_all_types() ->
    cmd(domain, find_all_types, #{}).

domain_find_all_types(Id) ->
    cmd(domain, find_all_types, #{id=>Id}).

domain_find_all_types(Id, Spec) ->
    cmd(domain, find_all_types, Spec#{id=>Id}).

domain_find_childs() ->
    cmd(domain, find_childs, #{}).

domain_find_childs(Id) ->
    cmd(domain, find_childs, #{id=>Id}).

domain_find_childs(Id, Spec) ->
    cmd(domain, find_childs, Spec#{id=>Id}).

domain_find_all_childs() ->
    cmd(domain, find_all_childs, #{}).

domain_find_all_childs(Id) ->
    cmd(domain, find_all_childs, #{id=>Id}).

domain_find_all_childs(Id, Spec) ->
    cmd(domain, find_all_childs, Spec#{id=>Id}).


domain_find_all_users() ->
    cmd(domain, find_all_childs, #{type=>user}).


session_get() ->
    cmd(session, get, #{}).


config_create(Sub, Parent, Config) ->
    cmd(config, create, #{subtype=>Sub, parent=>Parent, config=>Config}).

config_get(Id) ->
    cmd(config, get, #{id=>Id}).

config_update(Id, Config) ->
    cmd(config, update, #{id=>Id, config=>Config}).

config_delete(Id, Reason) ->
    cmd(config, update, #{id=>Id, reason=>Reason}).

config_find(SubType, Parent) ->
    cmd(config, find, #{parent=>Parent, subtype=>SubType}).




%% ===================================================================
%% Client fun
%% ===================================================================


api_client_fun(#nkapi_req{class=event, data=Event}, UserData) ->
    lager:warning("CLIENT event ~p", [lager:pr(Event, nkevent)]),
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
     nkdomain_domain_obj:create(root, "root", "sub1", "Sub 1").


sub2_create() ->
    nkdomain_domain_obj:create(root, "/sub1", "sub2", "Sub 2").


user_create_root(Name, Email) ->
    Data = #{name=>Name, surname=>"surname", email=>Email},
    nkdomain_user_obj:create(root, <<"root">>, Name, Data).

user_create_sub1(Name, Email) ->
    Data = #{name=>Name, surname=>"surname", email=>Email},
    nkdomain_user_obj:create(root, <<"/sub1">>, Name, Data).



to_bin(R) -> nklib_util:to_binary(R).