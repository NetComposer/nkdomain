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

%% @doc Config Object

-module(nkdomain_file_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/3]).
-export([upload/4, download/2]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3]).
-export([object_admin_info/0]).

-include("nkdomain.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN File "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

%% ===================================================================
%% API
%% ===================================================================



%% @doc
-spec create(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, Name, Obj) ->
    nkdomain_obj_lib:make_and_create(Srv, Name, Obj, #{}).


upload(Srv, File, CT, Body) ->
    case nkdomain_obj_lib:load(Srv, File, #{}) of
        #obj_id_ext{obj_id=ObjId, type = ?DOMAIN_FILE, pid=Pid} ->
            case nkdomain_util:store_file(ObjId, Body) of
                ok ->
                    Update = #{
                        content_type => CT,
                        size => byte_size(Body)
                    },
                    case nkdomain_obj:update(Pid, Update) of
                        {ok, _} ->
                            ok;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    ?LLOG(warning, "file ~s write error: ~p", [Error]),
                    {error, file_write_error}
            end;
        {error, Error} ->
            {error, Error}
    end.


download(Srv, File) ->
    case nkdomain_obj_lib:load(Srv, File, #{}) of
        #obj_id_ext{type = ?DOMAIN_FILE, obj_id=ObjId, pid=Pid} ->
            case nkdomain_obj:get_session(Pid) of
                {ok, #?NKOBJ{obj=Obj}} ->
                    Name = maps:get(name, Obj, <<>>),
                    case Obj of
                        #{?DOMAIN_FILE:=#{content_type:=CT}} ->
                            case nkdomain_util:get_file(ObjId) of
                                {ok, Body} ->
                                    {ok, Name, CT, Body};
                                {error, Error} ->
                                    {error, Error}
                            end
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.






%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_FILE
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 1100,
        tree_id => <<"domain_tree_resources_files">>
    }.


%% @private
object_mapping() ->
    #{
        content_type => #{type => keyword},
        size => #{type => long},
        store_id => #{type => keyword},
        password => #{type => keyword}
    }.


%% @private
object_parse(_SrvId, load, _Obj) ->
    #{
        content_type => binary,
        size => integer,
        store_id => binary,
        password => binary
    };

object_parse(_SrvId, update, _Obj) ->
    #{}.

%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_file_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(Cmd, Req, State) ->
    nkdomain_file_obj_api:cmd(Cmd, Req, State).




%% ===================================================================
%% Internal
%% ===================================================================




