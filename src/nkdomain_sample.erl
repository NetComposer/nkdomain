-module(nkdomain_sample).
-compile(export_all).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkmail/include/nkmail.hrl").

-define(HTTP, "https://127.0.0.1:9306/s/v06").
-define(WS, "wss://127.0.0.1:9306/s/v06/_api/ws").

%%-define(HTTP, "https://v1.netc.io/d/v03").
%%-define(WS, "wss://v1.netc.io/d/v03/_api/ws").

-define(ADMIN_PASS, "netcomposer").


login() ->
    login(admin, ?ADMIN_PASS).

login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, #{<<"session_id">>:=SessId}, Pid} = nkapi_client:start(?NKSRV, ?WS, Login, Fun, #{}, <<"objects/session/start">>),
    {ok, SessId, Pid}.


login(User, Pass, Domain) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)},
        domain_id => Domain
    },
    {ok, #{<<"session_id">>:=SessId}, Pid} = nkapi_client:start(?NKSRV, ?WS, Login, Fun, #{}, <<"objects/session/start">>),
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
        domain_id => to_bin(Domain),
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
        domain_id => to_bin(Domain),
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
        domain_id => to_bin(Domain),
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

user_update_obj_name(Id, ObjName) ->
    Data = #{
        id => to_bin(Id),
        obj_name => to_bin(ObjName)
    },
    cmd(<<"objects/user/update_obj_name">>, Data).


user_make_token() ->
    cmd(<<"objects/user/make_token">>, #{ttl=>30}).

user_avatar(Id) ->
    Dir = filename:join(code:priv_dir(nkdomain), "avatar1.png"),
    {ok, Bin} = file:read_file(Dir),
    File = #{content_type=><<"image/png">>, body=>base64:encode(Bin)},
    {ok, #{<<"obj_id">>:=FileId}} = cmd(<<"objects/file/create">>, #{name=>avatar1, file=>File}),
    Data = #{
        id => to_bin(Id),
        icon_id => FileId
    },
    cmd(<<"objects/user/update">>, Data).

user_get_status(AppId) ->
    cmd(<<"objects/user/get_status">>, #{app_id=>AppId}).

user_set_status(AppId, Status) ->
    cmd(<<"objects/user/set_status">>, #{app_id=>AppId, status=>Status}).


domain_get() ->
    cmd(<<"objects/domain/get">>, #{}).

domain_get(Id) ->
    cmd(<<"objects/domain/get">>, #{id=>Id}).

domain_create(Domain, ObjName, Name, Desc) ->
    Data = #{
        obj_name => ObjName,
        domain_id => Domain,
        name => Name,
        description => Desc,
        domain => #{}
    },
    case cmd(<<"objects/domain/create">>, Data) of
        {ok, #{<<"obj_id">>:=ObjId}} -> {ok, ObjId};
        {error, Error} -> {error, Error}
    end.

domain_create2(ObjName) ->
    domain_create(root, ObjName, <<"ABCDÉ Ñ"/utf8>>, <<"abc déf"/utf8>>).



domain_delete(Id) ->
    cmd(<<"objects/domain/delete"/utf8>>, #{id=>to_bin(Id)}).


domain_update(Id, Name, Desc) ->
    Data = #{
        id => to_bin(Id),
        name => Name,
        description => Desc
    },
    cmd(<<"objects/domain/update"/utf8>>, Data).


domain_find() ->
    cmd(<<"objects/domain/find"/utf8>>, #{}).

domain_find(Id) ->
    cmd(<<"objects/domain/find"/utf8>>, #{id=>Id}).

domain_find(Id, Spec) ->
    cmd(<<"objects/domain/find"/utf8>>, Spec#{id=>Id}).

domain_find_all() ->
    cmd(<<"objects/domain/find_all"/utf8>>, #{}).

domain_find_all(Id) ->
    cmd(<<"objects/domain/find_all"/utf8>>, #{id=>Id}).

domain_find_all(Id, Spec) ->
    cmd(<<"objects/domain/find_all"/utf8>>, Spec#{id=>Id}).

domain_find_types() ->
    cmd(<<"objects/domain/find_types"/utf8>>, #{}).

domain_find_types(Id) ->
    cmd(<<"objects/domain/find_types"/utf8>>, #{id=>Id}).

domain_find_types(Id, Spec) ->
    cmd(<<"objects/domain/find_types"/utf8>>, Spec#{id=>Id}).

domain_find_all_types() ->
    cmd(<<"objects/domain/find_all_types"/utf8>>, #{}).

domain_find_all_types(Id) ->
    cmd(<<"objects/domain/find_all_types"/utf8>>, #{id=>Id}).

domain_find_all_types(Id, Spec) ->
    cmd(<<"objects/domain/find_all_types"/utf8>>, Spec#{id=>Id}).

domain_find_childs() ->
    cmd(<<"objects/domain/find_childs"/utf8>>, #{}).

domain_find_childs(Id) ->
    cmd(<<"objects/domain/find_childs"/utf8>>, #{id=>Id}).

domain_find_childs(Id, Spec) ->
    cmd(<<"objects/domain/find_childs"/utf8>>, Spec#{id=>Id}).

domain_find_all_childs() ->
    cmd(<<"objects/domain/find_all_childs"/utf8>>, #{}).

domain_find_all_childs(Id) ->
    cmd(<<"objects/domain/find_all_childs"/utf8>>, #{id=>Id}).

domain_find_all_childs(Id, Spec) ->
    cmd(<<"objects/domain/find_all_childs"/utf8>>, Spec#{id=>Id}).

domain_find_all_users() ->
    cmd(<<"objects/domain/find_all_childs"/utf8>>, #{type=>user}).

domain_unload_childs(Id) ->
    cmd(<<"objects/domain/unload_childs"/utf8>>, #{id=>Id}).


session_get() ->
    cmd(<<"objects/session/get"/utf8>>, #{}).

session_delete() ->
    cmd(<<"objects/session/delete"/utf8>>, #{}).


config_create(Sub, Domain, Parent, Config) ->
    cmd(<<"objects/config/create"/utf8>>, #{subtype=>Sub, domain_id=>Domain, parent=>Parent, config=>Config}).

config_get(Id) ->
    cmd(<<"objects/config/get"/utf8>>, #{id=>Id}).

config_update(Id, Config) ->
    cmd(<<"objects/config/update"/utf8>>, #{id=>Id, config=>Config}).

config_delete(Id, Reason) ->
    cmd(<<"objects/config/update"/utf8>>, #{id=>Id, reason=>Reason}).

config_find(SubType, Parent) ->
    cmd(<<"objects/config/find"/utf8>>, #{parent=>Parent, subtype=>SubType}).



file_store_find_all() ->
    cmd(<<"objects/file.store/find_all"/utf8>>, #{}).


file_get(FileId) ->
    cmd(<<"objects/file/get"/utf8>>, #{id=>FileId}).


file_create_inline(Name) ->
    File = #{content_type=><<"plain/text"/utf8>>, body=>base64:encode(<<"1234">>)},
    {ok, #{<<"obj_id">>:=Id}} = cmd(<<"objects/file/create"/utf8>>, #{tags=>[a, b], name=>Name, file=>File}),
    Id.

file_create_inline_secure(Name) ->
    File = #{content_type=><<"plain/text"/utf8>>, body=>base64:encode(<<"1234">>), store_id=>"/file.stores/local_secure"},
    {ok, #{<<"obj_id">>:=Id}} = cmd(<<"objects/file/create"/utf8>>, #{name=>Name, file=>File}),
    Id.

file_create_inline_s3_secure(Name) ->
    File = #{content_type=><<"plain/text"/utf8>>, body=>base64:encode(<<"1234">>), store_id=>"/file.stores/carlos.s3_secure"},
    {ok, #{<<"obj_id">>:=Id}} = cmd(<<"objects/file/create"/utf8>>, #{name=>Name, file=>File}),
    Id.


file_get_inline(Id) ->
    cmd(<<"objects/file/get_inline"/utf8>>, #{id=>Id}).


file_post(T) ->
    Url = list_to_binary([?HTTP, "/_file"]),
    {ok, #{<<"obj_id">>:=FileId}} = upload(T, Url, "text/plain", "1,2,3,4"),
    FileId.

file_post2(T) ->
    {ok, Bin} = file:read_file("/tmp/file1.png"),
    Url = list_to_binary([?HTTP, "/_file"]),
    {ok, #{<<"obj_id">>:=FileId}} = upload(T, Url, "image/png", Bin),
    FileId.

file_post3(T) ->
    Bin = crypto:strong_rand_bytes(10000000),
    Url = list_to_binary([?HTTP, "/_file"]),
    {ok, #{<<"obj_id">>:=FileId}} = upload(T, Url, "application/binary", Bin),
    FileId.




file_post_secure(T) ->
    Url = list_to_binary([?HTTP, "/_file?store_id=/file.stores/local_secure"]),
    {ok, #{<<"obj_id">>:=FileId}} = upload(T, Url, "text/plain", "1234"),
    FileId.

file_post_s3_secure(T) ->
    Url = list_to_binary([?HTTP, "/_file?store_id=/file.stores/carlos.s3_secure"]),
    {ok, #{<<"obj_id">>:=FileId}} = upload(T, Url, "text/plain", "1234"),
    FileId.


file_download(T, FileId) ->
    Url = list_to_binary([?HTTP, "/_file/", FileId]),
    download(T, Url).

file_download2(T, FileId) ->
    Url = list_to_binary([?HTTP, "/_file/", FileId]),
    {ok, _, Body} = download(T, Url),
    ok = file:write_file(filename:join("/tmp", FileId), Body).


mail_provider_get_all() ->
    cmd(<<"objects/mail.provider/find_all"/utf8>>, #{}).


mail_config_create(_Id) ->
    Config = #{
        class => smtp,
        from => <<"carlosj.gf@gmail.com">>
    },
    cmd(<<"objects/mail.config/create"/utf8>>, #{tags=>[a, b], ?DOMAIN_MAIL_PROVIDER=>Config}).

mail_send1() ->
    Msg = #{
        provider_id => <<"/mail.providers/info.netcomposer"/utf8>>,
        % from => "My test <info.netcomposer@gmail.com>",
        to => <<"My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>"/utf8>>,
        subject => <<"sub2"/utf8>>,
        body => <<"msg2">>
    },
    cmd(<<"objects/mail/send"/utf8>>, Msg).


mail_send2() ->
    Dir = code:priv_dir(nkmail),
    {ok, F1} = file:read_file(filename:join(Dir, "sample.jpg")),
    {ok, F2} = file:read_file(filename:join(Dir, "sample.pdf")),

    Msg = #{
        provider_id => <<"/mail.providers/info.netcomposer"/utf8>>,
        from => <<"My test <info.netcomposer@gmail.com>"/utf8>>,
        to => <<"My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>"/utf8>>,
        subject => <<"sub3"/utf8>>,
        content_type => <<"text/html"/utf8>>,
        %%  body => "This is <strong>msg3</strong> øÿ áñ"
        body => <<"This is <strong>msg3</strong> øÿ áéíóúñ"/utf8>>,
        attachments => [
            #{
                name => <<"File1"/utf8>>,
                content_type => <<"image/jpeg"/utf8>>,
                body => base64:encode(F1)
            },
            #{
                name => <<"File2"/utf8>>,
                content_type => <<"application/pdf"/utf8>>,
                body => base64:encode(F2)
            }
        ]
    },
    cmd(<<"objects/mail/send"/utf8>>, Msg).






%% ===================================================================
%% Client fun
%% ===================================================================


api_client_fun(#nkreq{cmd = <<"event"/utf8>>, data=Event}, UserData) ->
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
        id => <<"admin"/utf8>>,
        password=> ?ADMIN_PASS,
        ttl => 60,
        meta => #{b=>2}
    },
    {ok, #{<<"token_id">>:=TokenId}} = http([], <<"objects/user/get_token"/utf8>>, Data),
    TokenId.

http_domain_find(Token, Id) ->
    http(Token, <<"objects/domain/find"/utf8>>, #{id=>Id}).


http_domain_find(User, Pass, Id) ->
    http(User, Pass, <<"objects/domain/find"/utf8>>, #{id=>Id}).


http(User, Pass, Cmd, Data) ->
    http(base64:encode(list_to_binary([User, ":", Pass])), Cmd, Data).


http(Token, Cmd, Data) ->
    Hds = [{"X-NetComposer-Auth", nklib_util:to_list(Token)}],
    Body = nklib_json:encode_pretty(#{cmd=>to_bin(Cmd), data=>Data}),
    Url = <<?HTTP, "/_api"/utf8>>,
    {ok, {{_, 200, _}, _Hs, B}} = httpc:request(post, {to_list(Url), Hds, "application/json", Body}, [], []),
    case nklib_json:decode(B) of
        #{<<"result">>:=<<"ok">>}=Result ->
            {ok, maps:get(<<"data"/utf8>>, Result)};
        #{<<"result">>:=<<"error"/utf8>>, <<"data">>:=#{<<"code">>:=Code, <<"error">>:=Error}} ->
            {error, {Code, Error}}
    end.


upload(T, Url, CT, Body) ->
    Hds = [{"X-NetComposer-Auth", nklib_util:to_list(T)}],
    case httpc:request(post, {to_list(Url), Hds, to_list(CT), Body}, [], []) of
        {ok, {{_, 200, _}, _Hs, B}} ->
            {ok, nklib_json:decode(B)};
        {ok, {{_, 400, _}, _Hs, B}} ->
            {error, nklib_json:decode(B)}
    end.



download(T, Url) ->
    Hds = [{"X-NetComposer-Auth", nklib_util:to_list(T)}],
    case httpc:request(get, {to_list(Url), Hds}, [], []) of
        {ok, {{_, 200, _}, Hs, B}} ->
            {ok, Hs, B};
        {ok, {{_, 400, _}, _Hs, B}} ->
            {error, nklib_json:decode(B)}
    end.



create_users(N) ->
    nkdomain:delete_all_childs("/name_test"),
    {ok, _, _Pid1} = login(),
    _ = nkdomain_sample:domain_create("/", name_test, "NameTest", "Name Test"),
    lists:foreach(
        fun(_) ->
            {N1, N2, N3} = get_name(),
            S = list_to_binary([N2, <<" ">>, N3]),
            Id = nklib_util:uid(),
            Email = <<Id/binary, "@test">>,
            {ok, _} = user_create("/name_test", Id, "1234", N1, S, Email)
        end,
        lists:seq(1, N)).



%% ===================================================================
%% OBJECTS
%% ===================================================================


to_list(R) -> nklib_util:to_list(R).

to_bin(R) -> nklib_util:to_binary(R).



%% ===================================================================
%% OBJECTS
%% ===================================================================


get_name() ->
    Pos1 = crypto:rand_uniform(1,201),
    Name1 = lists:nth(Pos1, names()),
    Pos2 = crypto:rand_uniform(1,1001),
    Name2 = lists:nth(Pos2, surnames()),
    Pos3 = crypto:rand_uniform(1,1001),
    Name3 = lists:nth(Pos3, surnames()),
    {Name1, Name2, Name3}.



%% 200 names
names() -> [
    <<"JOSE"/utf8>>,<<"ANTONIO"/utf8>>,<<"JUAN"/utf8>>,<<"MANUEL"/utf8>>,<<"FRANCISCO"/utf8>>,<<"LUIS"/utf8>>,<<"JAVIER"/utf8>>,<<"MIGUEL"/utf8>>,<<"ANGEL"/utf8>>,<<"CARLOS"/utf8>>,<<"JESUS"/utf8>>,<<"DAVID"/utf8>>,<<"PEDRO"/utf8>>,<<"DANIEL"/utf8>>,<<"MARIA"/utf8>>,<<"ALEJANDRO"/utf8>>,<<"RAFAEL"/utf8>>,<<"FERNANDO"/utf8>>,<<"ALBERTO"/utf8>>,<<"RAMON"/utf8>>,<<"PABLO"/utf8>>,<<"JORGE"/utf8>>,<<"SERGIO"/utf8>>,<<"ENRIQUE"/utf8>>,<<"VICENTE"/utf8>>,<<"ANDRES"/utf8>>,<<"DIEGO"/utf8>>,<<"VICTOR"/utf8>>,<<"IGNACIO"/utf8>>,<<"ADRIAN"/utf8>>,<<"RAUL"/utf8>>,<<"JOAQUIN"/utf8>>,<<"EDUARDO"/utf8>>,<<"ALVARO"/utf8>>,<<"IVAN"/utf8>>,<<"OSCAR"/utf8>>,<<"SANTIAGO"/utf8>>,<<"RUBEN"/utf8>>,<<"ROBERTO"/utf8>>,<<"ALFONSO"/utf8>>,<<"JAIME"/utf8>>,<<"GABRIEL"/utf8>>,<<"RICARDO"/utf8>>,<<"EMILIO"/utf8>>,<<"MARIO"/utf8>>,<<"JULIO"/utf8>>,<<"SALVADOR"/utf8>>,<<"MARCOS"/utf8>>,<<"JULIAN"/utf8>>,<<"TOMAS"/utf8>>,<<"GUILLERMO"/utf8>>,<<"AGUSTIN"/utf8>>,<<"FELIX"/utf8>>,<<"JORDI"/utf8>>,<<"JOSEP"/utf8>>,<<"CRISTIAN"/utf8>>,<<"JOAN"/utf8>>,<<"CESAR"/utf8>>,<<"GONZALO"/utf8>>,<<"MOHAMED"/utf8>>,<<"NICOLAS"/utf8>>,<<"DOMINGO"/utf8>>,<<"MARTIN"/utf8>>,<<"FELIPE"/utf8>>,<<"SEBASTIAN"/utf8>>,<<"HUGO"/utf8>>,<<"ALFREDO"/utf8>>,<<"MARC"/utf8>>,<<"MARIANO"/utf8>>,<<"ISMAEL"/utf8>>,<<"SAMUEL"/utf8>>,<<"HECTOR"/utf8>>,<<"AITOR"/utf8>>,<<"GREGORIO"/utf8>>,<<"ESTEBAN"/utf8>>,<<"XAVIER"/utf8>>,<<"LORENZO"/utf8>>,<<"RODRIGO"/utf8>>,<<"ARTURO"/utf8>>,<<"EUGENIO"/utf8>>,<<"ALEX"/utf8>>,<<"CRISTOBAL"/utf8>>,<<"ALBERT"/utf8>>,<<"IKER"/utf8>>,<<"VALENTIN"/utf8>>,<<"LUCAS"/utf8>>,<<"BORJA"/utf8>>,<<"ALEXANDER"/utf8>>,<<"ADOLFO"/utf8>>,<<"GERMAN"/utf8>>,<<"MARCO"/utf8>>,<<"JOHN"/utf8>>,<<"JONATHAN"/utf8>>,<<"ERNESTO"/utf8>>,<<"CHRISTIAN"/utf8>>,<<"BENITO"/utf8>>,<<"ISAAC"/utf8>>,<<"GUSTAVO"/utf8>>,<<"CARMELO"/utf8>>,<<"GERARDO"/utf8>>,<<"MARIA"/utf8>>,<<"CARMEN"/utf8>>,<<"ANA"/utf8>>,<<"ISABEL"/utf8>>,<<"DOLORES"/utf8>>,<<"PILAR"/utf8>>,<<"JOSEFA"/utf8>>,<<"TERESA"/utf8>>,<<"ROSA"/utf8>>,<<"CRISTINA"/utf8>>,<<"ANGELES"/utf8>>,<<"ANTONIA"/utf8>>,<<"LAURA"/utf8>>,<<"FRANCISCA"/utf8>>,<<"ELENA"/utf8>>,<<"MERCEDES"/utf8>>,<<"LUISA"/utf8>>,<<"MARTA"/utf8>>,<<"CONCEPCION"/utf8>>,<<"ROSARIO"/utf8>>,<<"LUCIA"/utf8>>,<<"JOSE"/utf8>>,<<"JUANA"/utf8>>,<<"MANUELA"/utf8>>,<<"SARA"/utf8>>,<<"PAULA"/utf8>>,<<"RAQUEL"/utf8>>,<<"JESUS"/utf8>>,<<"BEATRIZ"/utf8>>,<<"EVA"/utf8>>,<<"PATRICIA"/utf8>>,<<"VICTORIA"/utf8>>,<<"ROCIO"/utf8>>,<<"ENCARNACION"/utf8>>,<<"JULIA"/utf8>>,<<"BELEN"/utf8>>,<<"SILVIA"/utf8>>,<<"ESTHER"/utf8>>,<<"ANDREA"/utf8>>,<<"MONTSERRAT"/utf8>>,<<"NURIA"/utf8>>,<<"ANGELA"/utf8>>,<<"IRENE"/utf8>>,<<"INMACULADA"/utf8>>,<<"MONICA"/utf8>>,<<"SANDRA"/utf8>>,<<"MARGARITA"/utf8>>,<<"YOLANDA"/utf8>>,<<"ALBA"/utf8>>,<<"SONIA"/utf8>>,<<"ALICIA"/utf8>>,<<"MAR"/utf8>>,<<"SUSANA"/utf8>>,<<"MARINA"/utf8>>,<<"AMPARO"/utf8>>,<<"NATALIA"/utf8>>,<<"NIEVES"/utf8>>,<<"GLORIA"/utf8>>,<<"CLAUDIA"/utf8>>,<<"SOLEDAD"/utf8>>,<<"INES"/utf8>>,<<"LOURDES"/utf8>>,<<"VERONICA"/utf8>>,<<"CAROLINA"/utf8>>,<<"LUZ"/utf8>>,<<"CONSUELO"/utf8>>,<<"BEGOÑA"/utf8>>,<<"NOELIA"/utf8>>,<<"ASUNCION"/utf8>>,<<"LORENA"/utf8>>,<<"SOFIA"/utf8>>,<<"MILAGROS"/utf8>>,<<"CATALINA"/utf8>>,<<"ESPERANZA"/utf8>>,<<"OLGA"/utf8>>,<<"ALEJANDRA"/utf8>>,<<"CARLA"/utf8>>,<<"EMILIA"/utf8>>,<<"FATIMA"/utf8>>,<<"AURORA"/utf8>>,<<"BLANCA"/utf8>>,<<"MAGDALENA"/utf8>>,<<"MIRIAM"/utf8>>,<<"LIDIA"/utf8>>,<<"NEREA"/utf8>>,<<"CLARA"/utf8>>,<<"DANIELA"/utf8>>,<<"ANNA"/utf8>>,<<"CELIA"/utf8>>,<<"ELISA"/utf8>>,<<"EUGENIA"/utf8>>,<<"JOSEFINA"/utf8>>,<<"VIRGINIA"/utf8>>,<<"PURIFICACION"/utf8>>,<<"VANESA"/utf8>>,<<"REMEDIOS"/utf8>>,<<"TRINIDAD"/utf8>>,<<"GEMA"/utf8>>,<<"ADRIANA"/utf8>>,<<"VICENTA"/utf8>>
].


%% 1000 surnames
surnames() -> [
    <<"Abad"/utf8>>,<<"Adadia"/utf8>>,<<"Abascal"/utf8>>,<<"Abella"/utf8>>,<<"Abellán"/utf8>>,<<"Abril"/utf8>>,<<"Acedo"/utf8>>,<<"Acevedo"/utf8>>,<<"Acero"/utf8>>,<<"Acosta"/utf8>>,<<"Acuña"/utf8>>,<<"Adán"/utf8>>,<<"Aguado"/utf8>>,<<"Agudo"/utf8>>,<<"Águila"/utf8>>,<<"Aguilar"/utf8>>,<<"Aguilera"/utf8>>,<<"Aguiló"/utf8>>,<<"Aguirre"/utf8>>,<<"Agullo"/utf8>>,<<"Agustí"/utf8>>,<<"Agustín"/utf8>>,<<"Álamo"/utf8>>,<<"Alarcón"/utf8>>,<<"Alba"/utf8>>,<<"Alberdi"/utf8>>,<<"Albero"/utf8>>,<<"Alberola"/utf8>>,<<"Alberto"/utf8>>,<<"Alcalá"/utf8>>,<<"Alcalde"/utf8>>,<<"Alcántara"/utf8>>,<<"Alcaraz"/utf8>>,<<"Alcázar"/utf8>>,<<"Alcolea"/utf8>>,<<"Alegre"/utf8>>,<<"Alegria"/utf8>>,<<"Alemán"/utf8>>,<<"Alemany"/utf8>>,<<"Alfaro"/utf8>>,<<"Alfonso"/utf8>>,<<"Aliaga"/utf8>>,<<"Almagro"/utf8>>,<<"Almansa"/utf8>>,<<"Almazán"/utf8>>,<<"Almeida"/utf8>>,<<"Alonso"/utf8>>,<<"Alsina"/utf8>>,<<"Alvarado"/utf8>>,<<"Álvarez"/utf8>>,<<"Álvaro"/utf8>>,<<"Aller"/utf8>>,<<"Amador"/utf8>>,<<"Amat"/utf8>>,<<"Amaya"/utf8>>,<<"Amigó"/utf8>>,<<"Amo"/utf8>>,<<"Amor"/utf8>>,<<"Amores"/utf8>>,<<"Amorós"/utf8>>,<<"Anaya"/utf8>>,<<"Andrade"/utf8>>,<<"Andrés"/utf8>>,<<"Andreu"/utf8>>,<<"Ángel"/utf8>>,<<"Anglada"/utf8>>,<<"Angulo"/utf8>>,<<"Anguita"/utf8>>,<<"Antón"/utf8>>,<<"Antúnez"/utf8>>,<<"Aparicio"/utf8>>,<<"Aragón"/utf8>>,<<"Aragonés"/utf8>>,<<"Aramburu"/utf8>>,<<"Arana"/utf8>>,<<"Aranda"/utf8>>,<<"Araujo"/utf8>>,<<"Arce"/utf8>>,<<"Arco"/utf8>>,<<"Arcos"/utf8>>,<<"Arellano"/utf8>>,<<"Arenas"/utf8>>,<<"Arévalo"/utf8>>,<<"Arias"/utf8>>,<<"Ariño"/utf8>>,<<"Ariza"/utf8>>,<<"Arjona"/utf8>>,<<"Armas"/utf8>>,<<"Armengol"/utf8>>,<<"Arnaiz"/utf8>>,<<"Arnal"/utf8>>,<<"Arnau"/utf8>>,<<"Aroca"/utf8>>,<<"Arranz"/utf8>>,<<"Arregui"/utf8>>,<<"Arribas"/utf8>>,<<"Arrieta"/utf8>>,<<"Arroyo"/utf8>>,<<"Arteaga"/utf8>>,<<"Artigas"/utf8>>,<<"Asenjo"/utf8>>,<<"Asensio"/utf8>>,<<"Atienza"/utf8>>,<<"Ávila"/utf8>>,<<"Avilés"/utf8>>,<<"Ayala"/utf8>>,<<"Ayllón"/utf8>>,<<"Ayuso"/utf8>>,<<"Azcona"/utf8>>,<<"Aznar"/utf8>>,<<"Azorin"/utf8>>,<<"Badía"/utf8>>,<<"Baena"/utf8>>,<<"Báez"/utf8>>,<<"Baeza"/utf8>>,<<"Balaguer"/utf8>>,<<"Ballester"/utf8>>,<<"Ballesteros"/utf8>>,<<"Baños"/utf8>>,<<"Baquero"/utf8>>,<<"Barba"/utf8>>,<<"Barberá"/utf8>>,<<"Barbero"/utf8>>,<<"Barcel"/utf8>>,<<"Bárcena"/utf8>>,<<"Barco"/utf8>>,<<"Baró"/utf8>>,<<"Barón"/utf8>>,<<"Barragán"/utf8>>,<<"Barral"/utf8>>,<<"Barranco"/utf8>>,<<"Barreda"/utf8>>,<<"Barrena"/utf8>>,<<"Barrera"/utf8>>,<<"Barriga"/utf8>>,<<"Barrio"/utf8>>,<<"Barrios"/utf8>>,<<"Barros"/utf8>>,<<"Barroso"/utf8>>,<<"Bartolomé"/utf8>>,<<"Bas"/utf8>>,<<"Bastida"/utf8>>,<<"Batalla"/utf8>>,<<"Batlle"/utf8>>,<<"Bautista"/utf8>>,<<"Bauzà"/utf8>>,<<"Bayo"/utf8>>,<<"Bayón"/utf8>>,<<"Bayona"/utf8>>,<<"Becerra"/utf8>>,<<"Bejarano"/utf8>>,<<"Belda"/utf8>>,<<"Belmonte"/utf8>>,<<"Beltrán"/utf8>>,<<"Bellido"/utf8>>,<<"Bello"/utf8>>,<<"Benavent"/utf8>>,<<"Benavente"/utf8>>,<<"Benavides"/utf8>>,<<"Benet"/utf8>>,<<"Benítez"/utf8>>,<<"Benito"/utf8>>,<<"Berenguer"/utf8>>,<<"Bermejo"/utf8>>,<<"Bermúdez"/utf8>>,<<"Bernad"/utf8>>,<<"Bernal"/utf8>>,<<"Bernat"/utf8>>,<<"Berrocal"/utf8>>,<<"Bertrán"/utf8>>,<<"Bilbao"/utf8>>,<<"Blanca"/utf8>>,<<"Blanco"/utf8>>,<<"Blanch"/utf8>>,<<"Blanes"/utf8>>,<<"Blasco"/utf8>>,<<"Blázquez"/utf8>>,<<"Boada"/utf8>>,<<"Boix"/utf8>>,<<"Bolaños"/utf8>>,<<"Bonet"/utf8>>,<<"Bonilla"/utf8>>,<<"Borja"/utf8>>,<<"Borrás"/utf8>>,<<"Borrego"/utf8>>,<<"Borrell"/utf8>>,<<"Bosch"/utf8>>,<<"Botella"/utf8>>,<<"Bou"/utf8>>,<<"Bravo"/utf8>>,<<"Briones"/utf8>>,<<"Bru"/utf8>>,<<"Buendía"/utf8>>,<<"Bueno"/utf8>>,<<"Burgos"/utf8>>,<<"Busquets"/utf8>>,<<"Bustamante"/utf8>>,<<"Bustos"/utf8>>,<<"Caballero"/utf8>>,<<"Cabanillas"/utf8>>,<<"Cabañas"/utf8>>,<<"Cabello"/utf8>>,<<"Cabeza"/utf8>>,<<"Cabezas"/utf8>>,<<"Cabo"/utf8>>,<<"Cabrera"/utf8>>,<<"Cabrero"/utf8>>,<<"Cáceres"/utf8>>,<<"Cadenas"/utf8>>,<<"Cal"/utf8>>,<<"Calatayud"/utf8>>,<<"Calderón"/utf8>>,<<"Calvet"/utf8>>,<<"Calvo"/utf8>>,<<"Calleja"/utf8>>,<<"Calzada"/utf8>>,<<"Camacho"/utf8>>,<<"Cámara"/utf8>>,<<"Camino"/utf8>>,<<"Campillo"/utf8>>,<<"Campo"/utf8>>,<<"Campos"/utf8>>,<<"Campoy"/utf8>>,<<"Camps"/utf8>>,<<"Canales"/utf8>>,<<"Canals"/utf8>>,<<"Canet"/utf8>>,<<"Cano"/utf8>>,<<"Cánovas"/utf8>>,<<"Cañete"/utf8>>,<<"Cañizares"/utf8>>,<<"Caparrós"/utf8>>,<<"Carbó"/utf8>>,<<"Carbonell"/utf8>>,<<"Cárdenas"/utf8>>,<<"Cardona"/utf8>>,<<"Carlos"/utf8>>,<<"Carmona"/utf8>>,<<"Carnero"/utf8>>,<<"Caro"/utf8>>,<<"Carpio"/utf8>>,<<"Carranza"/utf8>>,<<"Carrasco"/utf8>>,<<"Carreño"/utf8>>,<<"Carrera"/utf8>>,<<"Carreras"/utf8>>,<<"Carretero"/utf8>>,<<"Carrillo"/utf8>>,<<"Carrión"/utf8>>,<<"Carro"/utf8>>,<<"Carvajal"/utf8>>,<<"Casado"/utf8>>,<<"Casal"/utf8>>,<<"Casals"/utf8>>,<<"Casanova"/utf8>>,<<"Casanovas"/utf8>>,<<"Casares"/utf8>>,<<"Casas"/utf8>>,<<"Cases"/utf8>>,<<"Castañeda"/utf8>>,<<"Castejón"/utf8>>,<<"Castell"/utf8>>,<<"Castellanos"/utf8>>,<<"Castelló"/utf8>>,<<"Castells"/utf8>>,<<"Castilla"/utf8>>,<<"Castillo"/utf8>>,<<"Castrillo"/utf8>>,<<"Castro"/utf8>>,<<"Catalá"/utf8>>,<<"Catalán"/utf8>>,<<"Cazorla"/utf8>>,<<"Cepeda"/utf8>>,<<"Cerdá"/utf8>>,<<"Cerdán"/utf8>>,<<"Cerezo"/utf8>>,<<"Cerro"/utf8>>,<<"Cervantes"/utf8>>,<<"Cervera"/utf8>>,<<"Céspedes"/utf8>>,<<"Cid"/utf8>>,<<"Cifuentes"/utf8>>,<<"Cisneros"/utf8>>,<<"Clavero"/utf8>>,<<"Clemente"/utf8>>,<<"Cobo"/utf8>>,<<"Cobos"/utf8>>,<<"Coca"/utf8>>,<<"Codina"/utf8>>,<<"Coello"/utf8>>,<<"Colom"/utf8>>,<<"Coloma"/utf8>>,<<"Colomer"/utf8>>,<<"Coll"/utf8>>,<<"Collado"/utf8>>,<<"Comas"/utf8>>,<<"Company"/utf8>>,<<"Conde"/utf8>>,<<"Conesa"/utf8>>,<<"Contreras"/utf8>>,<<"Corbacho"/utf8>>,<<"Cordero"/utf8>>,<<"Córdoba"/utf8>>,<<"Cornejo"/utf8>>,<<"Corominas"/utf8>>,<<"Coronado"/utf8>>,<<"Corral"/utf8>>,<<"Correa"/utf8>>,<<"Cortés"/utf8>>,<<"Cortina"/utf8>>,<<"Costa"/utf8>>,<<"Cózar"/utf8>>,<<"Criado"/utf8>>,<<"Crespi"/utf8>>,<<"Crespo"/utf8>>,<<"Cruz"/utf8>>,<<"Cuadrado"/utf8>>,<<"Cuéllar"/utf8>>,<<"Cuenca"/utf8>>,<<"Cuervo"/utf8>>,<<"Cuesta"/utf8>>,<<"Cueto"/utf8>>,<<"Cuevas"/utf8>>,<<"Chacón"/utf8>>,<<"Chamorro"/utf8>>,<<"Chaparro"/utf8>>,<<"Chaves"/utf8>>,<<"Checa"/utf8>>,<<"Chico"/utf8>>,<<"Dalmau"/utf8>>,<<"Dávila"/utf8>>,<<"Daza"/utf8>>,<<"Delgado"/utf8>>,<<"Díaz"/utf8>>,<<"Diego"/utf8>>,<<"Diéguez"/utf8>>,<<"Díez"/utf8>>,<<"Doménech"/utf8>>,<<"Domingo"/utf8>>,<<"Domínguez"/utf8>>,<<"Donaire"/utf8>>,<<"Donoso"/utf8>>,<<"Duarte"/utf8>>,<<"Dueñas"/utf8>>,<<"Duque"/utf8>>,<<"Durán"/utf8>>,<<"Echevarría"/utf8>>,<<"Echeverría"/utf8>>,<<"Egea"/utf8>>,<<"Elías"/utf8>>,<<"Elorza"/utf8>>,<<"Enríquez"/utf8>>,<<"Escalona"/utf8>>,<<"Escamilla"/utf8>>,<<"Escobar"/utf8>>,<<"Escolano"/utf8>>,<<"Escribano"/utf8>>,<<"Escrivá"/utf8>>,<<"Escudero"/utf8>>,<<"Espada"/utf8>>,<<"España"/utf8>>,<<"Español"/utf8>>,<<"Esparza"/utf8>>,<<"Espejo"/utf8>>,<<"Espinosa"/utf8>>,<<"Esteban"/utf8>>,<<"Esteve"/utf8>>,<<"Estévez"/utf8>>,<<"Estrada"/utf8>>,<<"Expósito"/utf8>>,<<"Fabra"/utf8>>,<<"Fábregas"/utf8>>,<<"Fabregat"/utf8>>,<<"Fajardo"/utf8>>,<<"Fernández"/utf8>>,<<"Ferrán"/utf8>>,<<"Ferrández"/utf8>>,<<"Ferrándiz"/utf8>>,<<"Ferrando"/utf8>>,<<"Ferrer"/utf8>>,<<"Ferrera"/utf8>>,<<"Ferrero"/utf8>>,<<"Ferreras"/utf8>>,<<"Figueras"/utf8>>,<<"Figueroa"/utf8>>,<<"Figuerola"/utf8>>,<<"Fiol"/utf8>>,<<"Flor"/utf8>>,<<"Flores"/utf8>>,<<"Folch"/utf8>>,<<"Fonseca"/utf8>>,<<"Font"/utf8>>,<<"Fortuny"/utf8>>,<<"Franch"/utf8>>,<<"Francisco"/utf8>>,<<"Franco"/utf8>>,<<"Franch"/utf8>>,<<"Frías"/utf8>>,<<"Frutos"/utf8>>,<<"Fuente"/utf8>>,<<"Fuentes"/utf8>>,<<"Fuertes"/utf8>>,<<"Fuster"/utf8>>,<<"Gabaldón"/utf8>>,<<"Galán"/utf8>>,<<"Galiano"/utf8>>,<<"Galindo"/utf8>>,<<"Galván"/utf8>>,<<"Gálvez"/utf8>>,<<"Gallardo"/utf8>>,<<"Gallo"/utf8>>,<<"Gámez"/utf8>>,<<"Gárate"/utf8>>,<<"Garay"/utf8>>,<<"Garcés"/utf8>>,<<"García"/utf8>>,<<"Gargallo"/utf8>>,<<"Garmendia"/utf8>>,<<"Garrido"/utf8>>,<<"Garriga"/utf8>>,<<"Garzón"/utf8>>,<<"Gascón"/utf8>>,<<"Gaya"/utf8>>,<<"Gelabert"/utf8>>,<<"Gibert"/utf8>>,<<"Gil"/utf8>>,<<"Gilabert"/utf8>>,<<"Giménez"/utf8>>,<<"Gimeno"/utf8>>,<<"Giner"/utf8>>,<<"Giralt"/utf8>>,<<"Girón"/utf8>>,<<"Girona"/utf8>>,<<"Gisbert"/utf8>>,<<"Godoy"/utf8>>,<<"Goicoechea"/utf8>>,<<"Gómez"/utf8>>,<<"Gomila"/utf8>>,<<"Gomis"/utf8>>,<<"González"/utf8>>,<<"Gonzalo"/utf8>>,<<"Goñi"/utf8>>,<<"Gordillo"/utf8>>,<<"Gracia"/utf8>>,<<"Granados"/utf8>>,<<"Grande"/utf8>>,<<"Gras"/utf8>>,<<"Grau"/utf8>>,<<"Gual"/utf8>>,<<"Guardia"/utf8>>,<<"Guardiola"/utf8>>,<<"Guerra"/utf8>>,<<"Guerrero"/utf8>>,<<"Guijarro"/utf8>>,<<"Guillén"/utf8>>,<<"Guitart"/utf8>>,<<"Gutiérrez"/utf8>>,<<"Guzmán"/utf8>>,<<"Haro"/utf8>>,<<"Heras"/utf8>>,<<"Heredia"/utf8>>,<<"Hernández"/utf8>>,<<"Hernando"/utf8>>,<<"Herranz"/utf8>>,<<"Herrera"/utf8>>,<<"Herrero"/utf8>>,<<"Hervás"/utf8>>,<<"Hervia"/utf8>>,<<"Hidalgo"/utf8>>,<<"Hierro"/utf8>>,<<"Higueras"/utf8>>,<<"Hoyos"/utf8>>,<<"Hoz"/utf8>>,<<"Huerta"/utf8>>,<<"Huertas"/utf8>>,<<"Huguet"/utf8>>,<<"Hurtado"/utf8>>,<<"Ibáñez"/utf8>>,<<"Ibarra"/utf8>>,<<"Iborra"/utf8>>,<<"Iglesia"/utf8>>,<<"Iglesias"/utf8>>,<<"Infante"/utf8>>,<<"Iniesta"/utf8>>,<<"Íñigo"/utf8>>,<<"Iñiguez"/utf8>>,<<"Iriarte"/utf8>>,<<"Isern"/utf8>>,<<"Izaguirre"/utf8>>,<<"Izquierdo"/utf8>>,<<"Jaén"/utf8>>,<<"Jara"/utf8>>,<<"Jaume"/utf8>>,<<"Jáuregui"/utf8>>,<<"Jerez"/utf8>>,<<"Jiménez"/utf8>>,<<"Jódar"/utf8>>,<<"Jordá"/utf8>>,<<"Jordán"/utf8>>,<<"Jove"/utf8>>,<<"Jover"/utf8>>,<<"Juan"/utf8>>,<<"Juárez"/utf8>>,<<"Juliá"/utf8>>,<<"Julián"/utf8>>,<<"Jurado"/utf8>>,<<"Lago"/utf8>>,<<"Laguna"/utf8>>,<<"Lamas"/utf8>>,<<"Landa"/utf8>>,<<"Lara"/utf8>>,<<"Larrañaga"/utf8>>,<<"Leal"/utf8>>,<<"Ledesma"/utf8>>,<<"Leiva"/utf8>>,<<"León"/utf8>>,<<"Lerma"/utf8>>,<<"Lillo"/utf8>>,<<"Linares"/utf8>>,<<"Lobato"/utf8>>,<<"Lobo"/utf8>>,<<"López"/utf8>>,<<"Lorenzo"/utf8>>,<<"Losa"/utf8>>,<<"Losada"/utf8>>,<<"Lozano"/utf8>>,<<"Lucas"/utf8>>,<<"Lucena"/utf8>>,<<"Luís"/utf8>>,<<"Luján"/utf8>>,<<"Lumbreras"/utf8>>,<<"Luna"/utf8>>,<<"Luque"/utf8>>,<<"Luz"/utf8>>,<<"Llabrés"/utf8>>,<<"Lladó"/utf8>>,<<"Llamas"/utf8>>,<<"Llano"/utf8>>,<<"Llanos"/utf8>>,<<"Lledó"/utf8>>,<<"Llobet"/utf8>>,<<"Llopis"/utf8>>,<<"Llorens"/utf8>>,<<"Llorente"/utf8>>,<<"Lloret"/utf8>>,<<"Lluch"/utf8>>,<<"Macías"/utf8>>,<<"Machado"/utf8>>,<<"Madrid"/utf8>>,<<"Madrigal"/utf8>>,<<"Maestre"/utf8>>,<<"Maldonado"/utf8>>,<<"Malo"/utf8>>,<<"Mancebo"/utf8>>,<<"Manjón"/utf8>>,<<"Manrique"/utf8>>,<<"Manso"/utf8>>,<<"Manuel"/utf8>>,<<"Manzanares"/utf8>>,<<"Manzano"/utf8>>,<<"Marco"/utf8>>,<<"Marcos"/utf8>>,<<"Marí"/utf8>>,<<"Marín"/utf8>>,<<"Mariño"/utf8>>,<<"Mariscal"/utf8>>,<<"Mármol"/utf8>>,<<"Marqués"/utf8>>,<<"Márquez"/utf8>>,<<"Martí"/utf8>>,<<"Martín"/utf8>>,<<"Martínez"/utf8>>,<<"Martorell"/utf8>>,<<"Mas"/utf8>>,<<"Mascaró"/utf8>>,<<"Mata"/utf8>>,<<"Matas"/utf8>>,<<"Mate"/utf8>>,<<"Mateo"/utf8>>,<<"Mateos"/utf8>>,<<"Mateu"/utf8>>,<<"Mayo"/utf8>>,<<"Mayol"/utf8>>,<<"Mayoral"/utf8>>,<<"Maza"/utf8>>,<<"Medina"/utf8>>,<<"Meléndez"/utf8>>,<<"Melero"/utf8>>,<<"Mena"/utf8>>,<<"Méndez"/utf8>>,<<"Mendizábal"/utf8>>,<<"Mendoza"/utf8>>,<<"Menéndez"/utf8>>,<<"Mercader"/utf8>>,<<"Merino"/utf8>>,<<"Mesa"/utf8>>,<<"Miguel"/utf8>>,<<"Milla"/utf8>>,<<"Millán"/utf8>>,<<"Mínguez"/utf8>>,<<"Mir"/utf8>>,<<"Miralles"/utf8>>,<<"Miranda"/utf8>>,<<"Miró"/utf8>>,<<"Moles"/utf8>>,<<"Molina"/utf8>>,<<"Moliner"/utf8>>,<<"Molins"/utf8>>,<<"Moll"/utf8>>,<<"Monreal"/utf8>>,<<"Montalbán"/utf8>>,<<"Montaña"/utf8>>,<<"Montenegro"/utf8>>,<<"Montero"/utf8>>,<<"Montes"/utf8>>,<<"Montesinos"/utf8>>,<<"Montoya"/utf8>>,<<"Montserrat"/utf8>>,<<"Mora"/utf8>>,<<"Moraleda"/utf8>>,<<"Morales"/utf8>>,<<"Morán"/utf8>>,<<"Morante"/utf8>>,<<"Morata"/utf8>>,<<"Morcillo"/utf8>>,<<"Morell"/utf8>>,<<"Moreno"/utf8>>,<<"Morera"/utf8>>,<<"Morillo"/utf8>>,<<"Mosquera"/utf8>>,<<"Moya"/utf8>>,<<"Múgica"/utf8>>,<<"Mulet"/utf8>>,<<"Múñiz"/utf8>>,<<"Muñoz"/utf8>>,<<"Mur"/utf8>>,<<"Murcia"/utf8>>,<<"Murillo"/utf8>>,<<"Muro"/utf8>>,<<"Nadal"/utf8>>,<<"Naranjo"/utf8>>,<<"Narváez"/utf8>>,<<"Navarrete"/utf8>>,<<"Navarro"/utf8>>,<<"Navas"/utf8>>,<<"Nebot"/utf8>>,<<"Neira"/utf8>>,<<"Nevado"/utf8>>,<<"Nicolau"/utf8>>,<<"Nicolás"/utf8>>,<<"Nieto"/utf8>>,<<"Niño"/utf8>>,<<"Nogueira"/utf8>>,<<"Noguera"/utf8>>,<<"Nogués"/utf8>>,<<"Noriega"/utf8>>,<<"Novoa"/utf8>>,<<"Núñez"/utf8>>,<<"Ocaña"/utf8>>,<<"Ochoa"/utf8>>,<<"Ojeda"/utf8>>,<<"Oliva"/utf8>>,<<"Olivares"/utf8>>,<<"Olivé"/utf8>>,<<"Oliver"/utf8>>,<<"Olivera"/utf8>>,<<"Oliveras"/utf8>>,<<"Olmedo"/utf8>>,<<"Olmo"/utf8>>,<<"Oller"/utf8>>,<<"Ordóñez"/utf8>>,<<"Orozco"/utf8>>,<<"Ortega"/utf8>>,<<"Ortiz"/utf8>>,<<"Ortuño"/utf8>>,<<"Osorio"/utf8>>,<<"Osuna"/utf8>>,<<"Otero"/utf8>>,<<"Pablo"/utf8>>,<<"Pacheco"/utf8>>,<<"Padilla"/utf8>>,<<"Páez"/utf8>>,<<"Pagès"/utf8>>,<<"Palacio"/utf8>>,<<"Palacios"/utf8>>,<<"Palau"/utf8>>,<<"Palma"/utf8>>,<<"Palmer"/utf8>>,<<"Palomar"/utf8>>,<<"Palomares"/utf8>>,<<"Palomino"/utf8>>,<<"Palomo"/utf8>>,<<"Pallarès"/utf8>>,<<"Paniagua"/utf8>>,<<"Pardo"/utf8>>,<<"Paredes"/utf8>>,<<"Pareja"/utf8>>,<<"Parejo"/utf8>>,<<"Parra"/utf8>>,<<"Pascual"/utf8>>,<<"Pastor"/utf8>>,<<"Patiño"/utf8>>,<<"Pavón"/utf8>>,<<"Paz"/utf8>>,<<"Pazos"/utf8>>,<<"Pedraza"/utf8>>,<<"Pedrero"/utf8>>,<<"Pedro"/utf8>>,<<"Pedrosa"/utf8>>,<<"Peinado"/utf8>>,<<"Peiró"/utf8>>,<<"Peláez"/utf8>>,<<"Pelayo"/utf8>>,<<"Pellicer"/utf8>>,<<"Peña"/utf8>>,<<"Peñalver"/utf8>>,<<"Peñas"/utf8>>,<<"Pera"/utf8>>,<<"Peral"/utf8>>,<<"Perales"/utf8>>,<<"Peralta"/utf8>>,<<"Perea"/utf8>>,<<"Pereira"/utf8>>,<<"Perelló"/utf8>>,<<"Perera"/utf8>>,<<"Pérez"/utf8>>,<<"Pi"/utf8>>,<<"Pina"/utf8>>,<<"Pineda"/utf8>>,<<"Pinedo"/utf8>>,<<"Pinilla"/utf8>>,<<"Pino"/utf8>>,<<"Pinto"/utf8>>,<<"Pintor"/utf8>>,<<"Piña"/utf8>>,<<"Piñeiro"/utf8>>,<<"Piñol"/utf8>>,<<"Piquer"/utf8>>,<<"Pizarro"/utf8>>,<<"Pla"/utf8>>,<<"Plana"/utf8>>,<<"Planas"/utf8>>,<<"Plaza"/utf8>>,<<"Pol"/utf8>>,<<"Polo"/utf8>>,<<"Pomares"/utf8>>,<<"Pombo"/utf8>>,<<"Ponce"/utf8>>,<<"Pons"/utf8>>,<<"Pont"/utf8>>,<<"Porcel"/utf8>>,<<"Porras"/utf8>>,<<"Porta"/utf8>>,<<"Portero"/utf8>>,<<"Portillo"/utf8>>,<<"Posada"/utf8>>,<<"Pou"/utf8>>,<<"Poza"/utf8>>,<<"Pozo"/utf8>>,<<"Pozuelo"/utf8>>,<<"Prada"/utf8>>,<<"Prado"/utf8>>,<<"Prat"/utf8>>,<<"Prats"/utf8>>,<<"Priego"/utf8>>,<<"Prieto"/utf8>>,<<"Puente"/utf8>>,<<"Puerta"/utf8>>,<<"Puga"/utf8>>,<<"Pujol"/utf8>>,<<"Pulido"/utf8>>,<<"Quero"/utf8>>,<<"Querol"/utf8>>,<<"Quesada"/utf8>>,<<"Quevedo"/utf8>>,<<"Quintana"/utf8>>,<<"Quintanilla"/utf8>>,<<"Quintero"/utf8>>,<<"Quiroga"/utf8>>,<<"Quirós"/utf8>>,<<"Ramírez"/utf8>>,<<"Ramis"/utf8>>,<<"Ramón"/utf8>>,<<"Ramos"/utf8>>,<<"Raya"/utf8>>,<<"Real"/utf8>>,<<"Rebollo"/utf8>>,<<"Recio"/utf8>>,<<"Redondo"/utf8>>,<<"Reguera"/utf8>>,<<"Reig"/utf8>>,<<"Reina"/utf8>>,<<"Requena"/utf8>>,<<"Revilla"/utf8>>,<<"Rey"/utf8>>,<<"Reyes"/utf8>>,<<"Riba"/utf8>>,<<"Ribas"/utf8>>,<<"Ribera"/utf8>>,<<"Ribes"/utf8>>,<<"Ricart"/utf8>>,<<"Rico"/utf8>>,<<"Riera"/utf8>>,<<"Rincón"/utf8>>,<<"Río"/utf8>>,<<"Ríos"/utf8>>,<<"Ripoll"/utf8>>,<<"Riquelme"/utf8>>,<<"Rius"/utf8>>,<<"Rivero"/utf8>>,<<"Robledo"/utf8>>,<<"Robles"/utf8>>,<<"Roca"/utf8>>,<<"Rocamora"/utf8>>,<<"Rocha"/utf8>>,<<"Roda"/utf8>>,<<"Ródenas"/utf8>>,<<"Rodrigo"/utf8>>,<<"Rodríguez"/utf8>>,<<"Roig"/utf8>>,<<"Rojas"/utf8>>,<<"Roldán"/utf8>>,<<"Roma"/utf8>>,<<"Román"/utf8>>,<<"Romero"/utf8>>,<<"Romeu"/utf8>>,<<"Ropero"/utf8>>,<<"Ros"/utf8>>,<<"Rosa"/utf8>>,<<"Rosado"/utf8>>,<<"Rosales"/utf8>>,<<"Rosell"/utf8>>,<<"Roselló"/utf8>>,<<"Rosselló"/utf8>>,<<"Roura"/utf8>>,<<"Rovira"/utf8>>,<<"Royo"/utf8>>,<<"Rozas"/utf8>>,<<"Ruano"/utf8>>,<<"Rubio"/utf8>>,<<"Rueda"/utf8>>,<<"Ruiz"/utf8>>,<<"Saavedra"/utf8>>,<<"Sabater"/utf8>>,<<"Sacristán"/utf8>>,<<"Sáenz"/utf8>>,<<"Sáez"/utf8>>,<<"Sainz"/utf8>>,<<"Sala"/utf8>>,<<"Salamanca"/utf8>>,<<"Salas"/utf8>>,<<"Salazar"/utf8>>,<<"Salcedo"/utf8>>,<<"Saldaña"/utf8>>,<<"Sales"/utf8>>,<<"Salgado"/utf8>>,<<"Salinas"/utf8>>,<<"Salmerón"/utf8>>,<<"Salom"/utf8>>,<<"Salvà"/utf8>>,<<"Salvador"/utf8>>,<<"Samper"/utf8>>,<<"Sanabria"/utf8>>,<<"Sánchez"/utf8>>,<<"Sancho"/utf8>>,<<"Sandoval"/utf8>>,<<"Sanjuan"/utf8>>,<<"Sanmartín"/utf8>>,<<"Sanmiguel"/utf8>>,<<"Sans"/utf8>>,<<"Santamaría"/utf8>>,<<"Santos"/utf8>>,<<"Sanz"/utf8>>,<<"Sarabia"/utf8>>,<<"Sarmiento"/utf8>>,<<"Sastre"/utf8>>,<<"Saura"/utf8>>,<<"Sebastián"/utf8>>,<<"Seco"/utf8>>,<<"Sedano"/utf8>>,<<"Segarra"/utf8>>,<<"Segovia"/utf8>>,<<"Segura"/utf8>>,<<"Serna"/utf8>>,<<"Serra"/utf8>>,<<"Serrano"/utf8>>,<<"Sevilla"/utf8>>,<<"Sevillano"/utf8>>,<<"Sierra"/utf8>>,<<"Silva"/utf8>>,<<"Simó"/utf8>>,<<"Sobrino"/utf8>>,<<"Sola"/utf8>>,<<"Solana"/utf8>>,<<"Solano"/utf8>>,<<"Solé"/utf8>>,<<"Soler"/utf8>>,<<"Solera"/utf8>>,<<"Solís"/utf8>>,<<"Solsona"/utf8>>,<<"Somoza"/utf8>>,<<"Soria"/utf8>>,<<"Soriano"/utf8>>,<<"Sotelo"/utf8>>,<<"Soto"/utf8>>,<<"Suárez"/utf8>>,<<"Sureda"/utf8>>,<<"Taboada"/utf8>>,<<"Talavera"/utf8>>,<<"Tamarit"/utf8>>,<<"Tamayo"/utf8>>,<<"Tapia"/utf8>>,<<"Tejada"/utf8>>,<<"Tejedor"/utf8>>,<<"Tejera"/utf8>>,<<"Tejero"/utf8>>,<<"Téllez"/utf8>>,<<"Tello"/utf8>>,<<"Tena"/utf8>>,<<"Tenorio"/utf8>>,<<"Terrón"/utf8>>,<<"Teruel"/utf8>>,<<"Tirado"/utf8>>,<<"Toledo"/utf8>>,<<"Tolosa"/utf8>>,<<"Tomás"/utf8>>,<<"Tomé"/utf8>>,<<"Tormo"/utf8>>,<<"Toro"/utf8>>,<<"Torralba"/utf8>>,<<"Torre"/utf8>>,<<"Torrecilla"/utf8>>,<<"Torrens"/utf8>>,<<"Torrent"/utf8>>,<<"Torrents"/utf8>>,<<"Torres"/utf8>>,<<"Torrijos"/utf8>>,<<"Tovar"/utf8>>,<<"Trillo"/utf8>>,<<"Trujillo"/utf8>>,<<"Tudela"/utf8>>,<<"Tur"/utf8>>,<<"Ugarte"/utf8>>,<<"Ureña"/utf8>>,<<"Uría"/utf8>>,<<"Uriarte"/utf8>>,<<"Uribe"/utf8>>,<<"Urrutia"/utf8>>,<<"Valbuena"/utf8>>,<<"Valcárcel"/utf8>>,<<"Valderrama"/utf8>>,<<"Valdés"/utf8>>,<<"Valencia"/utf8>>,<<"Valenciano"/utf8>>,<<"Valentín"/utf8>>,<<"Valenzuela"/utf8>>,<<"Valera"/utf8>>,<<"Valero"/utf8>>,<<"Valverde"/utf8>>,<<"Vall"/utf8>>,<<"Valle"/utf8>>,<<"Vallejo"/utf8>>,<<"Vallés"/utf8>>,<<"Vargas"/utf8>>,<<"Vázquez"/utf8>>,<<"Vega"/utf8>>,<<"Velasco"/utf8>>,<<"Velázquez"/utf8>>,<<"Vélez"/utf8>>,<<"Vendrell"/utf8>>,<<"Vera"/utf8>>,<<"Verdejo"/utf8>>,<<"Verdú"/utf8>>,<<"Vicente"/utf8>>,<<"Vicens"/utf8>>,<<"Vidal"/utf8>>,<<"Vigil"/utf8>>,<<"Vila"/utf8>>,<<"Vilanova"/utf8>>,<<"Vilalta"/utf8>>,<<"Vilaplana"/utf8>>,<<"Vilar"/utf8>>,<<"Villa"/utf8>>,<<"Villalba"/utf8>>,<<"Villalobos"/utf8>>,<<"Villalonga"/utf8>>,<<"Villanueva"/utf8>>,<<"Villar"/utf8>>,<<"Villaverde"/utf8>>,<<"Villegas"/utf8>>,<<"Yáñez"/utf8>>,<<"Yuste"/utf8>>,<<"Zabala"/utf8>>,<<"Zabaleta"/utf8>>,<<"Zamora"/utf8>>,<<"Zamorano"/utf8>>,<<"Zapata"/utf8>>,<<"Zaragoza"/utf8>>,<<"Zorrilla"/utf8>>,<<"Zurita"/utf8>>
].