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

-export([create/3, get_config/2]).
-export([find/3, find_all/3, find_types/3, find_all_types/3, find_childs/3, find_all_childs/3]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3]).
-export([object_start/1]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Obj Domain "++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
-spec create(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, Name, Obj) ->
    nkdomain_obj_lib:make_and_create(Srv, Name, Obj, #{}).


%% @doc
get_config(Srv, Id) ->
    case nkdomain_obj_lib:load(Srv, Id, #{}) of
        #obj_id_ext{pid=Pid} ->
            nkdomain_obj:get_obj_type(Pid);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{parent_id=>ObjId},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            SrvId:object_store_find(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            SrvId:object_store_find(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_types(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_store_find_types(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_types(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_store_find_all_types(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_childs(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_store_find_childs(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_childs(Srv, Id, Spec) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_store_find_all_childs(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_DOMAIN,
        permanent => true
    }.


%% @private
object_mapping() ->
    #{}.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_domain_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(Cmd, Req, State) ->
    nkdomain_domain_obj_api:cmd(Cmd, Req, State).


%% @private
object_start(#?NKOBJ{srv_id=SrvId, obj_id=ObjId, path=Path}=Session) ->
    spawn(fun() -> start_dom_childs(SrvId, ObjId, Path) end),
    {ok, Session}.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
start_dom_childs(SrvId, ObjId, Path) ->
    case find_childs(SrvId, ObjId, #{filters=>#{type=>?DOMAIN_DOMAIN}}) of
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