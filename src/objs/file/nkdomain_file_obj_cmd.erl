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

%% @doc File Object API
-module(nkdomain_file_obj_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/2]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").

%% ===================================================================
%% API
%% ===================================================================

cmd(<<"create">>, #nkreq{srv_id=SrvId, data=Obj, user_id=UserId} = Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Obj, Req) of
        {ok, DomainId} ->
            Obj2 = Obj#{
                srv_id => SrvId,
                type => ?DOMAIN_FILE,
                created_by => UserId,
                domain_id => DomainId
            },
            #{?DOMAIN_FILE:=#{body:=Body} = File} = Obj2,
            File2 = maps:remove(body, File#{size=>byte_size(Body)}),
            case nkdomain_file_obj:get_store(File2) of
                {ok, StoreId, Store} ->
                    FileId = nkdomain_file_obj:make_file_id(),
                    case nkdomain_file_obj:upload(StoreId, Store, FileId, Body) of
                        {ok, FileMeta} ->
                            File3 = File2#{store_id=>StoreId},
                            Obj3 = Obj2#{
                                obj_id => FileId,
                                ?DOMAIN_FILE := maps:merge(File3, FileMeta)
                            },
                            case nkdomain_obj_make:create(Obj3) of
                                {ok, ObjIdExt, Unknown} ->
                                    #obj_id_ext{obj_id=ObjId, path=Path} = ObjIdExt,
                                    Req2 = nkservice_api:add_unknown(Unknown, Req),
                                    {ok, #{obj_id=>ObjId, path=>Path}, Req2};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_inline">>, #nkreq{data=#{id:=Id}}) ->
    case nkdomain:get_obj(Id) of
        {ok, #{obj_id:=FileId, ?DOMAIN_FILE:=File}=Obj} ->
            case nkdomain_file_obj:get_store(File) of
                {ok, _StoreId, Store} ->
                    case nkdomain_file_obj:download(Store, FileId, File) of
                        {ok, Body} ->
                            File2 = File#{body=>base64:encode(Body)},
                            {ok, Obj#{?DOMAIN_FILE:=File2}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_cmd:api(Cmd, ?DOMAIN_FILE, Req).
