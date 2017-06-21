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

-export([http_post/3, http_get/3]).
-export([find/1, delete_all/1]).
-export([object_info/0, object_es_mapping/0, object_create/2, object_parse/3,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_admin_info/0]).

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


%% @doc Creates a file over HTTP
http_post(SrvId, Domain, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_user_obj:check_token(SrvId, Token) of
        {ok, UserId, _Meta} ->
            CT = nkservice_rest_http:get_ct(Req),
            Qs = nkservice_rest_http:get_qs(Req),
            Name = nklib_util:get_value(<<"name">>, Qs, <<>>),
            DomainId = case nklib_util:get_value(<<"domain">>, Qs, Domain) of
                <<>> -> <<"/">>;
                OtherDomain -> OtherDomain
            end,
            File1 = #{content_type => CT},
            File2 = case nklib_util:get_value(<<"store_id">>, Qs, <<>>) of
                <<>> ->
                    File1;
                FileStoreId ->
                    File1#{store_id=>FileStoreId}
            end,
            case get_store(SrvId, File2) of
                {ok, StoreId, Store} ->
                    FileId = make_file_id(),
                    MaxSize = maps:get(max_file_size, Store, ?MAX_FILE_SIZE),
                    case nkservice_rest_http:get_body(Req, #{max_size=>MaxSize}) of
                        {ok, Body} ->
                            case upload(SrvId, StoreId, Store, FileId, Body) of
                                {ok, FileMeta} ->
                                    File3 = File2#{
                                        store_id => StoreId,
                                        size => byte_size(Body)
                                    },
                                    Obj = #{
                                        obj_id => FileId,
                                        type => ?DOMAIN_FILE,
                                        parent_id => DomainId,
                                        created_by => UserId,
                                        name => Name,
                                        ?DOMAIN_FILE => maps:merge(File3, FileMeta)
                                    },
                                    nkdomain_obj_make:create(SrvId, Obj);
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
http_get(SrvId, FileId, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Qs = nkservice_rest_http:get_qs(Req),
    Token = case nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>) of
        <<>> ->
            nklib_util:get_value(<<"auth">>, Qs, <<>>);
        Auth0 ->
            Auth0
    end,
    case nkdomain_user_obj:check_token(SrvId, Token) of
        {ok, _UserId, _Meta} ->
            case nkdomain:get_obj(SrvId, FileId) of
                {ok, #{obj_id:=FileObjId, ?DOMAIN_FILE:=File}} ->
                    CT = maps:get(content_type, File),
                    case get_store(SrvId, File) of
                        {ok, _StoreId, Store} ->
                            case nkfile:download(SrvId, Store, File#{name=>FileObjId}) of
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
find(SrvId) ->
    nkdomain_domain_obj:find(SrvId, <<"root">>, #{filters=>#{type=>?DOMAIN_FILE}}).


%% @private
delete_all(SrvId) ->
    nkdomain:delete_all_childs_type(SrvId, <<"root">>, ?DOMAIN_FILE).



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
        weight => 1100
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
object_parse(_SrvId, load, _Obj) ->
     #{
        content_type => binary,
        store_id => binary,
        size => integer,
        password => binary,
        '__mandatory' => [content_type, store_id, size]
    };

object_parse(_SrvId, update, _Obj) ->
    #{}.



%% @doc Inside-WS file creation
-spec object_create(nkservice:id(), nkdomain:obj()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

object_create(SrvId, Obj) ->
    case nklib_syntax:parse(Obj, inline_syntax()) of
        {ok, #{?DOMAIN_FILE:=#{body:=Body}=File}, _} ->
            File2 = maps:remove(body, File#{size=>byte_size(Body)}),
            case get_store(SrvId, File2) of
                {ok, StoreId, Store} ->
                    FileId = make_file_id(),
                    case upload(SrvId, StoreId, Store, FileId, Body) of
                        {ok, FileMeta} ->
                            File3 = File2#{store_id=>StoreId},
                            Obj2 = Obj#{
                                obj_id => FileId,
                                ?DOMAIN_FILE := maps:merge(File3, FileMeta)
                            },
                            nkdomain_obj_make:create(SrvId, Obj2);
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
object_api_syntax(<<"get_inline">>, Syntax) ->
    Syntax#{
        id => binary,
        '__mandatory' => [id]
    };

object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_FILE, Syntax).


%% @private
object_api_cmd(<<"get_inline">>, #nkreq{srv_id=SrvId, data=#{id:=Id}}) ->
    case nkdomain:get_obj(SrvId, Id) of
        {ok, #{obj_id:=FileId, ?DOMAIN_FILE:=File}=Obj} ->
            case get_store(SrvId, File) of
                {ok, _StoreId, Store} ->
                    case download(SrvId, Store, FileId, File) of
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

object_api_cmd(Cmd, Req) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_FILE, Req).


%% ===================================================================
%% Internal
%% ===================================================================

make_file_id() ->
    <<"file-", (nklib_util:luid())/binary>>.

inline_syntax() ->
    #{
        name => binary,
        ?DOMAIN_FILE => #{
            content_type => binary,
            store_id => binary,
            body => base64,
            '__mandatory' => [content_type, body]
        },
        '__mandatory' => [name]
    }.


%% @private
upload(SrvId, StoreId, Store, FileId, Body) ->
    case nkfile:upload(SrvId, Store, #{store_id=>StoreId, name=>FileId}, Body) of
        {ok, Meta} ->
            {ok, maps:with([password], Meta)};
        {error, Error} ->
            {error, Error}
    end.


%% @private
download(SrvId, Store, FileId, File) ->
    case nkfile:download(SrvId, Store, File#{name=>FileId}) of
        {ok, _, Body} ->
            {ok, Body};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_store(SrvId, #{store_id:=StoreId}) ->
    do_get_store(SrvId, StoreId);

get_store(SrvId, _Obj) ->
    case SrvId:config_nkdomain() of
        #nkdomain_cache{file_store=StoreId} ->
            do_get_store(SrvId, StoreId);
        _ ->
            {error, store_id_missing}
    end.


%% @private
do_get_store(SrvId, StoreId) ->
    case nkdomain_lib:load(SrvId, StoreId) of
        #obj_id_ext{obj_id=StoreObjId, type = ?DOMAIN_FILE_STORE} ->
            case nkdomain:get_obj_type(SrvId, StoreObjId) of
                {ok, Data} ->
                    {ok, StoreObjId, Data};
                _ ->
                    {error, store_id_invalid}
            end;
        _ ->
            {error, store_id_invalid}
    end.


