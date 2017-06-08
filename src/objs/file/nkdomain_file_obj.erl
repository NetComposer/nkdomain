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
         object_sync_op/3]).
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


http_post(SrvId, Domain, Req) ->
    Headers = nkservice_rest_http:get_headers(Req),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_util:get_http_auth(SrvId, Token) of
        {ok, UserId, _Meta, _State} ->
            case nkservice_rest_http:get_body(Req, #{max_size=>10000000}) of
                {ok, Body} ->
                    CT = nkservice_rest_http:get_ct(Req),
                    Qs = nkservice_rest_http:get_qs(Req),
                    lager:error("NKLOG Qs ~p", [Qs]),
                    Name = nklib_util:get_value(<<"name">>, Qs, <<>>),
                    FileId = <<"file-", (nklib_util:luid())/binary>>,
                    DomainId = case nklib_util:get_value(<<"domain">>, Qs, Domain) of
                        <<>> -> <<"/">>;
                        OtherDomain -> OtherDomain
                    end,
                    case get_store_id(SrvId, Domain, Qs) of
                        {ok, StoreId} ->
                            case upload(SrvId, StoreId, FileId, Body) of
                                {ok, Meta} ->
                                    Obj = #{
                                        obj_id => FileId,
                                        type => ?DOMAIN_FILE,
                                        parent_id => DomainId,
                                        created_by => UserId,
                                        name => Name,
                                        ?DOMAIN_FILE => Meta#{
                                            content_type => CT,
                                            size => byte_size(Body)
                                        }
                                    },
                                    create(SrvId, <<>>, Obj);
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


upload(SrvId, StoreId, FileId, Body) ->
    case nkfile:upload(SrvId, #{store_id=>StoreId, name=>FileId}, Body) of
        {ok, Meta} ->
            Meta2 = maps:with([store_id, password], Meta),
            {ok, Meta2#{size => byte_size(Body)}};
        {error, Error} ->
            {error, Error}
    end.


get_store_id(SrvId, Domain, Qs) ->
    case nklib_util:get_value(<<"store_id">>, Qs, <<>>) of
        <<>> ->
            case nkdomain_domain_obj:get_config(SrvId, Domain) of
                {ok, #{default_store_id:=StoreId}} ->
                    {ok, StoreId};
                _ ->
                    case SrvId:config() of
                        #{domain_default_store_id:=StoreId} ->
                            {ok, StoreId};
                        _ ->
                            {error, missing_store_id}
                    end
            end;
        StoreId ->
            {ok, StoreId}
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
object_parse(SrvId, load, Obj) ->
    #{?DOMAIN_FILE:=File} = Obj,
    Syntax = #{
        content_type => binary,
        size => integer,
        store_id => binary,
        password => binary,
        '__mandatory' => [store_id]
    },
    case nklib_syntax:parse(File, Syntax, #{path=>?DOMAIN_FILE}) of
        {ok, #{store_id:=StoreId}=Store, UnknownFields} ->
            case nkdomain_obj_lib:load(SrvId, StoreId, #{}) of
                #obj_id_ext{type = ?DOMAIN_FILE_STORE} ->
                    {type_obj, Store, UnknownFields};
                {error, Error} ->
                    ?LLOG(warning, "error getting store ~s: ~p", [StoreId, Error]),
                    {error, {store_not_found, StoreId}}
            end;
        {error, Error} ->
            {error, Error}
    end;

object_parse(_SrvId, update, _Obj) ->
    #{}.

%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_FILE, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_api_cmd(<<"create">>, Req, State) ->
    nkdomain_obj_api:api(<<"create">>, ?DOMAIN_FILE, Req, State);

object_api_cmd(_Cmd, _Req, State) ->
    {error, not_implemented, State}.


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




