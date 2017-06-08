-module(nkdomain_sample).
-compile(export_all).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkmail/include/nkmail.hrl").

-define(HTTP, "http://127.0.0.1:9301").
-define(WS, "ws://127.0.0.1:9301/_api/ws").


login() ->
    login(admin, "1234").

login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, #{<<"session_id">>:=SessId}, Pid} = nkapi_client:start(root, ?WS, Login, Fun, #{}, <<"objects/user/login">>),
    {ok, SessId, Pid}.


login(User, Pass, Domain) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)},
        domain_id => Domain
    },
    {ok, #{<<"session_id">>:=SessId}, Pid} = nkapi_client:start(root, ?WS, Login, Fun, #{}, <<"objects/user/login">>),
    {ok, SessId, Pid}.



event_get_subs() ->
    cmd(<<"event/get_subscriptions">>, #{}).

event_subscribe(ObjType, EvType, ObjId) ->
    cmd(<<"event/subscribe">>, #{class=>domain, subclass=>ObjType, type=>EvType, obj_id=>ObjId}).

event_unsubscribe(ObjType, EvType, ObjId) ->
    cmd(<<"event/unsubscribe">>, #{class=>domain, subclass=>ObjType, type=>EvType, obj_id=>ObjId}).


%% @doc
log(Source, Msg, Data) ->
    cmd(<<"session/log">>, Data#{source=>Source, message=>Msg}).



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


file_post1(T) ->
    {ok, #{<<"obj_id">>:=FileId}} = upload(T, "/_file", "text/plain", "1234"),
    {ok, "1234"} = download(T, FileId),
    FileId.

file_post2(T) ->
    {error,#{<<"data">>:=#{<<"code">>:=<<"store_not_found">>}}} =
       upload(T, "/_file?store_id=none", "text/plain", "4321"),
     {ok, #{<<"obj_id">>:=FileId}} =
        upload(T, "/_file?store_id=/file.stores/carlos.s3_secure", "text/plain", "4321"),
    {ok, "4321"} = download(T, FileId),
    FileId.

file_post3(T) ->
    {error,#{<<"data">>:=#{<<"code">>:=<<"could_not_load_parent">>}}} =
        upload(T, "/_file?domain=a", "text/plain", "4321"),
    {ok, #{<<"obj_id">>:=FileId}} =
        upload(T, "/_file?domain=/ct", "text/plain", "4321"),
    {ok, "4321"} = download(T, FileId),
    FileId.



file_create_local() ->
    cmd(<<"objects/file/create">>, #{tags=>[a, b], file=>#{store_id=><<"/file.stores/local">>}}).

file_create_s3() ->
    cmd(<<"objects/file/create">>, #{tags=>[b, c], file=>#{store_id=><<"/file.stores/carlos.s3_secure">>}}).

file_update(Id) ->
    cmd(<<"objects/file/update">>, #{id=>Id, tags=>[b, c], file=>#{content_type=>pdf2}}).



mail_config_create(_Id) ->
    Config = #{
        class => smtp,
        from => <<"carlosj.gf@gmail.com">>
    },
    cmd(<<"objects/mail.config/create">>, #{tags=>[a, b], ?DOMAIN_MAIL_PROVIDER=>Config}).

mail_send1() ->
    Msg = #{
        provider => <<"/mail.providers/info.netcomposer">>,
        % from => "My test <info.netcomposer@gmail.com>",
        to => <<"My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>">>,
        subject => <<"sub2">>,
        body => <<"msg2">>
    },
    cmd(<<"objects/mail/send">>, Msg).


mail_send2() ->
    Dir = code:priv_dir(nkmail),
    {ok, F1} = file:read_file(filename:join(Dir, "sample.jpg")),
    {ok, F2} = file:read_file(filename:join(Dir, "sample.pdf")),

    Msg = #{
        provider => <<"/mail.providers/info.netcomposer">>,
        from => <<"My test <info.netcomposer@gmail.com>">>,
        to => <<"My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>">>,
        subject => <<"sub3">>,
        content_type => <<"text/html">>,
        %%  body => "This is <strong>msg3</strong> øÿ áñ"
        body => <<"This is <strong>msg3</strong> øÿ áéíóúñ">>,
        attachments => [
            #{
                name => <<"File1">>,
                content_type => <<"image/jpeg">>,
                body => base64:encode(F1)
            },
            #{
                name => <<"File2">>,
                content_type => <<"application/pdf">>,
                body => base64:encode(F2)
            }
        ]
    },
    cmd(<<"objects/mail/send">>, Msg).






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
%% HTTP
%% ===================================================================

http_login() ->
    Data = #{
        id => <<"admin">>,
        password=> <<"1234">>,
        ttl => 60,
        meta => #{b=>2}
    },
    {ok, #{<<"token_id">>:=TokenId}} = http([], <<"objects/user/login">>, Data),
    TokenId.

http_domain_find(Token, Id) ->
    http(Token, <<"objects/domain/find">>, #{id=>Id}).


http_domain_find(User, Pass, Id) ->
    http(User, Pass, <<"objects/domain/find">>, #{id=>Id}).


http(User, Pass, Cmd, Data) ->
    http(base64:encode(list_to_binary([User, ":", Pass])), Cmd, Data).


http(Token, Cmd, Data) ->
    Hds = [{"X-NetComposer-Auth", nklib_util:to_list(Token)}],
    Body = nklib_json:encode_pretty(#{cmd=>to_bin(Cmd), data=>Data}),
    Url = <<?HTTP, "/_api">>,
    {ok, {{_, 200, _}, _Hs, B}} = httpc:request(post, {to_list(Url), Hds, "application/json", Body}, [], []),
    case nklib_json:decode(B) of
        #{<<"result">>:=<<"ok">>}=Result ->
            {ok, maps:get(<<"data">>, Result)};
        #{<<"result">>:=<<"error">>, <<"data">>:=#{<<"code">>:=Code, <<"error">>:=Error}} ->
            {error, {Code, Error}}
    end.


upload(T, Url, CT, Body) ->
    Hds = [{"X-NetComposer-Auth", nklib_util:to_list(T)}],
    Url2 = list_to_binary([?HTTP, Url]),
    case httpc:request(post, {to_list(Url2), Hds, to_list(CT), Body}, [], []) of
        {ok, {{_, 200, _}, _Hs, B}} ->
            {ok, nklib_json:decode(B)};
        {ok, {{_, 400, _}, _Hs, B}} ->
            {error, nklib_json:decode(B)}
    end.



download(T, File) ->
    Hds = [{"X-NetComposer-Auth", nklib_util:to_list(T)}],
    Url = list_to_binary([?HTTP, "/_file/", File]),
    case httpc:request(get, {to_list(Url), Hds}, [], []) of
        {ok, {{_, 200, _}, _Hs, B}} ->
            {ok, B};
        {ok, {{_, 400, _}, _Hs, B}} ->
            {error, nklib_json:decode(B)}
    end.


%%upload_icon(Id) ->
%%    {ok, {_, _, B1}} = httpc:request("https://www.flatpyramid.com/uploads/3d-models/samples/other/popeye_3d-3d-model-sample-22266-87323.gif"),
%%    Url = binary_to_list(<< ?FILES, "/icon/", (to_bin(Id))/binary>>),
%%    httpc:request(post, {Url, [], "image/gif", B1}, [], []).
%%
%%download_icon(Id) ->
%%    Url = binary_to_list(<< ?FILES, "/icon/", (to_bin(Id))/binary>>),
%%    httpc:request(get, {Url, []}, [], []).
%%




%% ===================================================================
%% OBJECTS
%% ===================================================================


to_list(R) -> nklib_util:to_list(R).

to_bin(R) -> nklib_util:to_binary(R).