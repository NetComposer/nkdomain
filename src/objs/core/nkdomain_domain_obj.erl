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

-export([find/3, find_all/3, find_types/3, find_all_types/3, find_childs/3, find_all_childs/3]).
-export([object_info/0, object_parse/3, object_es_mapping/0,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_start/1]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Obj Domain "++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
find(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{domain_id=>ObjId},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            nkdomain:search(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            Filters1 = maps:get(filters, Spec, #{}),
            Filters2 = Filters1#{path=><<"childs_of:", Path/binary>>},
            Spec2 = maps:remove(id, Spec#{filters=>Filters2}),
            nkdomain:search(SrvId, Spec2);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_types(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_db_search_types(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_types(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_db_search_all_types(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_childs(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, obj_id=ObjId} ->
            SrvId:object_db_search_childs(SrvId, ObjId, Spec);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
find_all_childs(Srv, Id, Spec) ->
    case nkdomain_lib:find(Srv, Id) of
        #obj_id_ext{srv_id=SrvId, path=Path} ->
            SrvId:object_db_search_all_childs(SrvId, Path, Spec);
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_info() ->
    #{
        type => ?DOMAIN_DOMAIN,
        permanent => true
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        defaults => #{enabled => false}
    }.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{
        vsn => binary,
        defaults => map,
        '__defaults' => #{vsn => <<"1">>}
    }.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_domain_obj_syntax:api(Cmd, Syntax).




%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_domain_obj_api:cmd(Cmd, Req).


%% @private
object_start(#?STATE{id=#obj_id_ext{srv_id=SrvId, obj_id=ObjId, path=Path}}=State) ->
    spawn(fun() -> start_dom_childs(SrvId, ObjId, Path) end),
    {ok, State}.


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