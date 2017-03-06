-module(nkdomain_test).
-compile(export_all).

cd() ->
    Obj = #{
        obj_id => <<"root">>,
        domain => <<"/">>,
        type => domain,
        description => <<"NetComposer">>
    },
    nkdomain_obj:create(root, Obj, #{}).