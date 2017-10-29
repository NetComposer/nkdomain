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

-export([execute/4, http_post/1, http_post/4, http_get/2]).
-export([find/0, delete_all/0]).
-export([object_info/0, object_es_mapping/0, object_parse/2, object_api_syntax/2, object_api_cmd/2]).
-export([object_admin_info/0]).
-export([make_file_id/0, upload/4, download/3]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(MAX_FILE_SIZE, 10000000).


%% ===================================================================
%% Types
%% ===================================================================

%% ===================================================================
%% API
%% ===================================================================



%% @%% @private GraphQL execute
execute(_Ctx, #{?DOMAIN_FILE:=File}, Field, _Args) ->
    case Field of
        <<"contentType">> -> {ok, maps:get(content_type, File, null)};
        <<"size">> -> {ok, maps:get(size, File, null)}
    end.


%% @doc Creates a file from a nkservice_rest request
http_post(Req) ->
    Qs = nkservice_rest_http:get_qs(Req),
    Name = nklib_util:get_value(<<"name">>, Qs, <<>>),
    Domain = nklib_util:get_value(<<"domain">>, Qs, <<"/">>),
    StoreId = nklib_util:get_value(<<"store_id">>, Qs, <<>>),
    case http_post(Domain, StoreId, Name, Req) of
        {ok, #obj_id_ext{obj_id=ObjId, path=Path}, _Unknown} ->
            {ok, ObjId, Path};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates a file over HTTP
http_post(Domain, StoreId, Name, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_api_util:check_raw_token(Token) of
        {ok, UserId, _UserDomainId, _LoginMeta, _SessId} ->
            CT = nkservice_rest_http:get_ct(Req),
            File1 = #{
                content_type => CT,
                store_id => StoreId
            },
            case get_store(File1) of
                {ok, StoreObjId, Store} ->
                    FileId = make_file_id(),
                    MaxSize = maps:get(max_file_size, Store, ?MAX_FILE_SIZE),
                    case nkservice_rest_http:get_body(Req, #{max_size=>MaxSize}) of
                        {ok, Body} ->
                            SrvId = nkservice_rest_http:get_srv_id(Req),
                            case upload(StoreObjId, Store, FileId, Body) of
                                {ok, FileMeta} ->
                                    File2 = File1#{
                                        store_id => StoreObjId,
                                        size => byte_size(Body)
                                    },
                                    Obj = #{
                                        srv_id => SrvId,
                                        obj_id => FileId,
                                        type => ?DOMAIN_FILE,
                                        domain_id => Domain,
                                        created_by => UserId,
                                        name => Name,
                                        ?DOMAIN_FILE => maps:merge(File2, FileMeta)
                                    },
                                    nkdomain_obj_make:create(Obj);
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
    end.


%% @doc
http_get(FileId, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Qs = nkservice_rest_http:get_qs(Req),
    Token = case nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>) of
        <<>> ->
            nklib_util:get_value(<<"auth">>, Qs, <<>>);
        Auth0 ->
            Auth0
    end,
    case nkdomain_api_util:check_raw_token(Token) of
        {ok, _UserId, _UserDomainId, _LoginMeta, _SessId} ->
            case nkdomain:get_obj(FileId) of
                {ok, #{obj_id:=FileObjId, ?DOMAIN_FILE:=File}} ->
                    CT = maps:get(content_type, File),
                    case get_store(File) of
                        {ok, _StoreObjId, Store} ->
                            case nkfile:download(?NKROOT, Store, File#{name=>FileObjId}) of
                                {ok, _, Body} ->
                                    {ok, CT, Body};
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
    end.



%% @private
find() ->
    nkdomain_domain_obj:search(<<"root">>, #{filters=>#{type=>?DOMAIN_FILE}}).


%% @private
delete_all() ->
    nkdomain:delete_all_childs_type(<<"root">>, ?DOMAIN_FILE).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?DOMAIN_FILE
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 1100,
        type_view_mod => nkdomain_file_obj_type_view
    }.


%% @private
object_es_mapping() ->
    #{
        content_type => #{type => keyword},
        store_id => #{type => keyword},
        size => #{type => long},
        password => #{type => keyword}
    }.


%% @private
object_parse(update, _Obj) ->
    #{};

object_parse(_Mode, _Obj) ->
     #{
        vsn => binary,
        content_type => binary,
        store_id => binary,
        size => integer,
        password => binary,
        '__mandatory' => [content_type, store_id, size],
        '__defaults' => #{vsn => <<"1">>}
    }.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_file_obj_syntax:syntax(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_file_obj_cmd:cmd(Cmd, Req).


%% ===================================================================
%% Internal
%% ===================================================================

make_file_id() ->
    <<"file-", (nklib_util:luid())/binary>>.


%% @private
upload(StoreId, Store, FileId, Body) ->
    case nkfile:upload(?NKROOT, Store, #{store_id=>StoreId, name=>FileId}, Body) of
        {ok, Meta} ->
            {ok, maps:with([password], Meta)};
        {error, Error} ->
            {error, Error}
    end.


%% @private
download(Store, FileId, File) ->
    case nkfile:download(?NKROOT, Store, File#{name=>FileId}) of
        {ok, _, Body} ->
            {ok, Body};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_store(#{store_id:=StoreId}) when StoreId /= <<>> ->
    do_get_store(StoreId);

get_store(_Obj) ->
    case ?CALL_NKROOT(config_nkdomain_nkroot, []) of
        #nkdomain_config_cache{file_store=StoreId} ->
            do_get_store(StoreId);
        _ ->
            {error, store_id_missing}
    end.


%% @private
do_get_store(StoreId) ->
    case nkdomain_lib:load(StoreId) of
        #obj_id_ext{obj_id=StoreObjId, type = ?DOMAIN_FILE_STORE} ->
            case nkdomain:get_obj(StoreObjId) of
                {ok, #{?DOMAIN_FILE_STORE:=Data}} ->
                    {ok, StoreObjId, Data};
                _ ->
                    {error, store_id_invalid}
            end;
        _ ->
            {error, store_id_invalid}
    end.


