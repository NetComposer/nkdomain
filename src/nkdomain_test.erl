-module(nkdomain_test).
-compile(export_all).

root_create() ->
    Obj = #{
        obj_id => <<"root">>,
        domain => <<"/">>,
        type => nkdomain_domain,
        description => <<"NetComposer">>
    },
    nkdomain_obj:create(root, Obj, #{}).


root_load() ->
    nkdomain_obj:load(root, nkdomain_domain, <<"root">>, #{}).



sub1_create() ->
    Obj = #{
        obj_id => <<"sub1">>,
        domain => <<"/sub1">>,
        type => nkdomain_domain,
        description => <<"Sub1">>
    },
    nkdomain_obj:create(root, Obj, #{}).


sub2_create() ->
    Obj = #{
        obj_id => <<"sub2">>,
        domain => <<"/sub1/sub2">>,
        type => nkdomain_domain,
        description => <<"Sub2">>
    },
    nkdomain_obj:create(root, Obj, #{}).
