%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Domain Object

-module(nkdomain_domain_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/4, find_types/2, find_all_types/2, find_childs/2, find_childs/3, find_childs_type/4,
         find_all_childs/2, find_all_childs/3, find_all_childs_type/3, find_all_childs_type/4]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_start/1, object_all_links_down/1]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Obj Domain "++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:id(), nkdomain:name(), binary()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, Parent, Name, Desc) ->
    Opts = #{
        name => Name,
        description => Desc
    },
    nkdomain_obj_lib:make_and_create(Srv, Parent, ?DOMAIN_DOMAIN, Opts).


%% @doc
find_types(Srv, Id) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_store_find_types(SrvId, ObjId, #{});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_types(Srv, Id) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_store_find_all_types(SrvId, Path, #{});
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_childs(Srv, Id) ->
    find_childs(Srv, Id, #{}).


%% @doc
find_childs(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_store_find_childs(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_childs_type(Srv, Id, Type, Spec) ->
    Filters1 = maps:get(filters, Spec, #{}),
    Filters2 = Filters1#{type=>Type},
    Spec2 = Spec#{filters=>Filters2},
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_store_find_childs(SrvId, ObjId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_childs(Srv, Id) ->
    find_all_childs(Srv, Id, #{}).


%% @doc
find_all_childs(Srv, Id, Spec) ->
    Filters1 = maps:get(filters, Spec, #{}),
    Filters2 = case Spec of
        #{type:=Type} ->
            Filters1#{type=>Type};
        _ ->
            Filters1
    end,
    Spec2 = Spec#{filters=>Filters2},
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_store_find_all_childs(SrvId, Path, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_childs_type(Srv, Id, Type) ->
    find_all_childs_type(Srv, Id, Type, #{}).


%% @doc
find_all_childs_type(Srv, Id, Type, Spec) ->
    find_all_childs(Srv, Id, Spec#{type=>Type}).







%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_DOMAIN,
        min_first_time => -1             % Do not unload
    }.


%% @private
object_mapping() ->
    #{}.


%% @private
object_syntax(_Mode) ->
    #{}.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_domain_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Data, State) ->
    nkdomain_domain_obj_api:cmd(Sub, Cmd, Data, State).


%% @private
object_start(#obj_session{srv_id=SrvId, obj_id=ObjId, path=Path}=Session) ->
    spawn(fun() -> start_dom_childs(SrvId, ObjId, Path) end),
    {ok, Session}.


%% @private
object_all_links_down(Session) ->
    {keepalive, Session}.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
start_dom_childs(SrvId, ObjId, Path) ->
    case find_childs_type(SrvId, ObjId, ?DOMAIN_DOMAIN, #{}) of
        {ok, 0, []} ->
            ok;
        {ok, _N, List} ->
            lists:foreach(
                fun({?DOMAIN_DOMAIN, ChildId, ChildPath}) ->
                    lager:info("Domain ~s starting child domain ~s", [Path, ChildPath]),
                    nkdomain:load(SrvId, ChildId, #{}) end,
                List);
        {error, Error} ->
            ?LLOG(warning, "could not find childs: ~p", [Error])
    end.