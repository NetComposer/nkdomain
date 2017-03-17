-module(nkdomain_test).
-compile(export_all).











root_create() ->
    Obj = #{
        path => <<"/">>,
        type => <<"domain">>,
        parent_id => <<>>,
        description => <<"NetComposer">>
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"root">>}).


sub1_create() ->
    Obj = #{
        path => <<"/sub1">>,
        type => <<"domain">>,
        parent_id => <<"root">>,
        description => <<"Sub1">>
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"sub1">>}).


sub2_create() ->
    Obj = #{
        path => <<"/sub1/sub2">>,
        type => <<"domain">>,
        parent_id => <<"sub1">>,
        description => <<"Sub2">>
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"sub2">>}).


user1_create() ->
    Obj = #{
        path => <<"/users/u1">>,
        type => <<"user">>,
        parent_id => <<"root">>,
        description => <<"User 1">>,
        aliases => <<"user1@domain.com">>,
        user => #{
            name => <<"Name 1">>,
            surname => <<"Surname 1">>,
            password => "1234"
        }
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"user1">>}).


user2_create() ->
    Obj = #{
        path => <<"/users/u2">>,
        type => <<"user">>,
        parent_id => <<"root">>,
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
        parent_id => <<"sub1">> ,
        description => <<"User 3">>,
        aliases => <<"user3@domain.com">>,
        user => #{
            name => <<"Name 3">>,
            surname => <<"Surname 3">>,
            password => "4321"
        }
    },
    nkdomain_obj:create(root, Obj, #{obj_id=><<"user3">>}).

