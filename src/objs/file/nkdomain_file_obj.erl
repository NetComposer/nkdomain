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
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3,
         object_init/1, object_sync_op/3]).
-export([object_admin_info/0]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").


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


http_post(SrvId, Domain, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_util:get_http_auth(SrvId, Token) of
        {ok, UserId, _Meta, _State} ->
            case nkservice_rest_http:get_body(Req, #{max_size=>10000000}) of
                {ok, Body} ->
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
                        StoreId ->
                            File1#{store_id=>StoreId}
                    end,
                    Obj = #{
                        type => ?DOMAIN_FILE,
                        parent_id => DomainId,
                        created_by => UserId,
                        name => Name,
                        ?DOMAIN_FILE => File2
                    },
                    Meta = #{meta => #{nkdomain_file_bin=>Body}},
                    nkdomain_obj_lib:make_and_create(SrvId, <<>>, Obj, #{meta=>Meta});
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


upload(SrvId, StoreId, FileId, Body) ->
    case nkfile:upload(SrvId, #{store_id=>StoreId, name=>FileId}, Body) of
        {ok, Meta} ->
            {ok, maps:with([password], Meta)};
        {error, Error} ->
            {error, Error}
    end.


http_get(SrvId, FileId, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_util:get_http_auth(SrvId, Token) of
        {ok, _UserId, _Meta, _State} ->
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
        password => binary,
        body => base64,
        '__mandatory' => [content_type]
    };

object_parse(_SrvId, update, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_FILE, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(Cmd, Req, State) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_FILE, Req, State).


%% @private
object_init(#?NKOBJ{srv_id=SrvId, obj_id=FileId, obj=Obj, meta=Meta}=State) ->
    case get_store_id(State) of
        {ok, StoreId} ->
            #{?DOMAIN_FILE:=File} = Obj,
            case Meta of
                #{nkdomain_file_bin:=Bin} ->
                    #{content_type:=_, store_id:=StoreId} =File,
                    case upload(SrvId, StoreId, FileId, Bin) of
                        {ok, StoreMeta} ->
                            File2 = maps:merge(File, StoreMeta),
                            File3 = File2#{size => byte_size(Bin)},
                            Obj2 = Obj#{?DOMAIN_FILE:=File3},
                            Meta2 = maps:remove(nkdomain_file_bin, Meta),
                            {ok, State#?NKOBJ{obj=Obj2, meta=Meta2}};
                        {error, Error} ->
                            ?LLOG(warning, "error writing file: ~p", [Error], State),
                            {error, file_write_error}
                    end;
                _ ->
                    case File of
                        #{body:=Body} ->
                            case catch base64:decode(Body) of
                                Bin when is_binary(Bin) ->
                                    Meta2 = Meta#{nkdomain_file_bin=>Bin},
                                    File2 = maps:remove(body, File),
                                    Obj2 = Obj#{?DOMAIN_FILE:=File2},
                                    object_init(State#?NKOBJ{obj=Obj2, meta=Meta2});
                                _ ->
                                    {error, invalid_base64}
                            end;
                        _ ->
                            {error, missing_body}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.


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


%% @private
get_store_id(#?NKOBJ{obj=#{?DOMAIN_FILE:=#{store_id:=StoreId}}}=State) ->
    check_store_id(StoreId, State);

get_store_id(#?NKOBJ{srv_id=SrvId, type=Type, path=Path}=State) ->
    {ok, Domain, _} = nkdomain_util:get_parts(Type, Path),
    case nkdomain_domain_obj:get_config(SrvId, Domain) of
        {ok, #{default_store_id:=StoreId}} ->
            check_store_id(StoreId, State);
        _ ->
            case SrvId:config() of
                #{domain_default_store_id:=StoreId} ->
                    check_store_id(StoreId, State);
                _ ->
                    {error, missing_store_id}
            end
    end.


%% @private
check_store_id(StoreId, #?NKOBJ{srv_id=SrvId}) ->
    case nkdomain_obj_lib:load(SrvId, StoreId, #{}) of
        #obj_id_ext{type = ?DOMAIN_FILE_STORE} ->
            {ok, StoreId};
        _ ->
            {error, missing_store_id}
    end.


