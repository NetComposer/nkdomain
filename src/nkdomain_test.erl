-module(nkdomain_test).
-compile(export_all).

root_create() ->
    Obj = #{
        obj_id => <<"root">>,
        path => <<"/">>,
        type => domain,
        module => nkdomain_domain,
        parent_id => <<>>,
        description => <<"NetComposer">>
    },
    nkdomain_obj:create(root, Obj, #{}).


root_load() ->
    nkdomain_obj:load(root, <<"root">>, #{}).



sub1_create() ->
    Obj = #{
        obj_id => <<"sub1">>,
        path => <<"/sub1">>,
        type => domain,
        module => nkdomain_domain,
        parent_id => <<"root">>,
        description => <<"Sub1">>
    },
    nkdomain_obj:create(root, Obj, #{}).

sub1_load() ->
    nkdomain_obj:load(root, <<"sub1">>, #{}).





sub2_create() ->
    Obj = #{
        obj_id => <<"sub2">>,
        path => <<"/sub1/sub2">>,
        type => domain,
        module => nkdomain_domain,
        parent_id => <<"sub1">>,
        description => <<"Sub2">>
    },
    nkdomain_obj:create(root, Obj, #{}).

sub2_load() ->
    nkdomain_obj:load(root, <<"sub2">>, #{}).
