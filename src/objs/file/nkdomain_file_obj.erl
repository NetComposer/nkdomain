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

-export([create/3, http_post/3, http_get/3]).
-export([find/2, delete_all/2]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/2,
         object_sync_op/3]).
-export([object_admin_info/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").

%% ===================================================================
%% Types
%% ===================================================================

%% ===================================================================
%% API
%% ===================================================================



%% @doc
-spec create(nkservice:id(), nkdomain:name(), nkdomain:obj()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(SrvId, Name, Obj) ->
    case nklib_syntax:parse(Obj, inline_syntax()) of
        {ok, #{?DOMAIN_FILE:=#{body:=Body}=File}, _} ->
            File2 = maps:remove(body,  File#{size=>byte_size(Body)}),
            case get_store_id(SrvId, File2) of
                {ok, #{store_id:=StoreId}=File3} ->
                    FileId = make_file_id(),
                    case upload(SrvId, StoreId, FileId, Body) of
                        {ok, FileMeta} ->
                            Obj2 = Obj#{?DOMAIN_FILE:=maps:merge(File3, FileMeta)},
                            Obj3 = Obj2#{obj_id=>FileId},
                            nkdomain_obj_lib:make_and_create(SrvId, Name, Obj3, #{});
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            nkdomain_obj_lib:make_and_create(SrvId, Name, Obj, #{})
    end.


http_post(SrvId, Domain, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_util:get_http_auth(SrvId, Token) of
        {ok, UserId, _Meta} ->
            case nkservice_rest_http:get_body(Req, #{max_size=>10000000}) of
                {ok, Body} ->
                    CT = nkservice_rest_http:get_ct(Req),
                    Qs = nkservice_rest_http:get_qs(Req),
                    Name = nklib_util:get_value(<<"name">>, Qs, <<>>),
                    DomainId = case nklib_util:get_value(<<"domain">>, Qs, Domain) of
                        <<>> -> <<"/">>;
                        OtherDomain -> OtherDomain
                    end,
                    File1 = #{
                        content_type => CT,
                        size => byte_size(Body)
                    },
                    File2 = case nklib_util:get_value(<<"store_id">>, Qs, <<>>) of
                        <<>> ->
                            File1;
                        FileStoreId ->
                            File1#{store_id=>FileStoreId}
                    end,
                    case get_store_id(SrvId, File2) of
                        {ok, #{store_id:=StoreId}=File3} ->
                            FileId = make_file_id(),
                            case upload(SrvId, StoreId, FileId, Body) of
                                {ok, FileMeta} ->
                                    Obj = #{
                                        obj_id => FileId,
                                        type => ?DOMAIN_FILE,
                                        parent_id => DomainId,
                                        created_by => UserId,
                                        name => Name,
                                        ?DOMAIN_FILE => maps:merge(File3, FileMeta)
                                    },
                                    nkdomain_obj_lib:make_and_create(SrvId, <<>>, Obj, #{});
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



http_get(SrvId, FileId, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Qs = nkservice_rest_http:get_qs(Req),
    Token = case nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>) of
        <<>> ->
            nklib_util:get_value(<<"auth">>, Qs, <<>>);
        Auth0 ->
            Auth0
    end,
    case nkdomain_util:get_http_auth(SrvId, Token) of
        {ok, _UserId, _Meta} ->
            case nkdomain_obj_lib:load(SrvId, FileId, #{}) of
                #obj_id_ext{type = ?DOMAIN_FILE, obj_id=FileObjId, pid=Pid} ->
                    case nkdomain_obj:get_obj(Pid) of
                        {ok, #{?DOMAIN_FILE:=File}} ->
                            CT = maps:get(content_type, File, <<>>),
                            case nkfile:download(SrvId, File#{name=>FileObjId}) of
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
find(SrvId, Root) ->
    nkdomain_domain_obj:find(SrvId, Root, #{filters=>#{type=>?DOMAIN_FILE}}).


%% @private
delete_all(SrvId, Root) ->
    nkdomain_store:delete_all_childs_type(SrvId, Root, ?DOMAIN_FILE).



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
        weight => 1100
    }.


%% @private
object_mapping() ->
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


%% @private
object_api_syntax(<<"get_inline">>, Syntax) ->
    Syntax#{
        id => binary,
        '__mandatory' => [id]
    };

object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_FILE, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(<<"get_inline">>, #nkreq{srv_id=SrvId, data=#{id:=Id}}) ->
    case nkdomain_obj_lib:load(SrvId, Id, #{}) of
        #obj_id_ext{obj_id=FileId, pid=Pid} ->
            case nkdomain_obj:get_obj(Pid) of
                {ok, #{?DOMAIN_FILE:=File}=Obj} ->
                    case download(SrvId, FileId, File) of
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


%% @private
object_sync_op({?MODULE, update, File}, _From, #?NKOBJ{obj=Obj}=State) ->
    Obj2 = Obj#{?DOMAIN_FILE:=File},
    State2 = State#?NKOBJ{obj=Obj2, is_dirty=true},
    {reply_and_save, ok, State2};

object_sync_op(_Op, _From, _Session) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

make_file_id() ->
    <<"file-", (nklib_util:luid())/binary>>.

inline_syntax() ->
    #{
        ?DOMAIN_FILE => #{
            content_type => binary,
            store_id => binary,
            body => base64,
            '__mandatory' => [content_type, body]
        }
    }.


%% @private
upload(SrvId, StoreId, FileId, Body) ->
    case nkfile:upload(SrvId, #{store_id=>StoreId, name=>FileId}, Body) of
        {ok, Meta} ->
            {ok, maps:with([password], Meta)};
        {error, Error} ->
            {error, Error}
    end.


%% @private
download(SrvId, FileId, File) ->
    case nkfile:download(SrvId, File#{name=>FileId}) of
        {ok, _, Body} ->
            {ok, Body};
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_store_id(SrvId, #{store_id:=StoreId}=Obj) ->
    check_store_id(SrvId, StoreId, Obj);

get_store_id(SrvId, Obj) ->
    case SrvId:config() of
        #{domain_default_store_id:=StoreId} ->
            check_store_id(SrvId, StoreId, Obj);
        _ ->
            {error, missing_store_id}
    end.


%% @private
check_store_id(SrvId, StoreId, Obj) ->
    case nkdomain_obj_lib:load(SrvId, StoreId, #{}) of
        #obj_id_ext{type = ?DOMAIN_FILE_STORE} ->
            {ok, Obj#{store_id=>StoreId}};
        _ ->
            {error, missing_store_id}
    end.


