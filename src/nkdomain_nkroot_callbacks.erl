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

%% @doc NkDomain service callback module
-module(nkdomain_nkroot_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([nkservice_rest_http/4]).
-export([object_db_init/1, object_db_read/1, object_db_save/1, object_db_delete/1]).
-export([object_db_find_obj/1, object_db_search/1, object_db_search_alias/1,
         object_db_search_types/2, object_db_search_all_types/2,
         object_db_search_childs/2, object_db_search_all_childs/2, object_db_search_agg_field/4,
         object_db_delete_all_childs/2, object_db_clean/0]).
-export([service_api_syntax/3, service_api_allow/2, service_api_cmd/2]).
-export([api_server_http_auth/3, api_server_reg_down/4]).
-export([service_init/2, service_handle_cast/2, service_handle_info/2]).


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN NKROOT Callbacks: "++Txt, Args)).

-include("nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type obj_id() :: nkdomain:obj_id().
-type type() :: nkdomain:type().
-type path() :: nkdomain:path().



%% ===================================================================
%% REST
%% ===================================================================


%% @doc
nkservice_rest_http(<<"nkroot_graphql">>, Method, Path, Req) ->
    nkdomain_graphiql:http(Method, Path, Req);

nkservice_rest_http(_Id, get, [<<"_file">>, FileId], Req) ->
    case nkdomain_file_obj:http_get(FileId, Req) of
        {ok, CT, Bin} ->
            {http, 200, [{<<"Content-Type">>, CT}], Bin};
        {error, Error} ->
            nkservice_rest_http:reply_json({error, Error}, Req)
    end;

nkservice_rest_http(_Id, post, [<<"_file">>], Req) ->
    case nkdomain_file_obj:http_post(Req) of
        {ok, ObjId, Path, _Obj} ->
            Reply = #{obj_id=>ObjId, path=>Path},
            nkservice_rest_http:reply_json({ok, Reply}, Req);
        {error, Error} ->
            nkservice_rest_http:reply_json({error, Error}, Req)
    end;

nkservice_rest_http(_Id, _Method, _Path, _Req) ->
    % lager:warning("NkLOG HTTP Path: ~s", [_Path]),
    continue.




%% ===================================================================
%% API Server
%% ===================================================================


%% @doc
service_api_syntax(_Id, Syntax, #nkreq{cmd = <<"objects/", Rest/binary>>}=Req) ->
    case binary:split(Rest, <<"/">>) of
        [] ->
            continue;
        [Type, Cmd] ->
            case nkdomain_reg:get_type_module(Type) of
                undefined ->
                    continue;
                Module ->
                    Syntax2 = case erlang:function_exported(Module, object_api_syntax, 2) of
                        true ->
                            apply(Module, object_api_syntax, [Cmd, Syntax]);
                        false ->
                            nkdomain_obj_syntax:syntax(Cmd, Type, Syntax)
                    end,
                    {continue, [_Id, Syntax2, Req#nkreq{req_state={Type, Module, Cmd}}]}
            end
    end;

service_api_syntax(_Id, _Syntax, _Req) ->
    continue.


%% @doc
%% TODO to remove (after admin)
service_api_allow(_Id, #nkreq{cmd = <<"objects/user/login">>, user_id = <<>>}) ->
    true;

service_api_allow(_Id, #nkreq{cmd = <<"objects/session/start">>, user_id = <<>>}) ->
    true;

service_api_allow(_Id, #nkreq{cmd = <<"objects/user/get_token">>, user_id = <<>>}) ->
    true;

service_api_allow(_Id, #nkreq{cmd = <<"objects/", _/binary>>, user_id = <<>>}) ->
    false;

service_api_allow(_Id, #nkreq{cmd = <<"objects/", _/binary>>, req_state={_Type, Module, Cmd}}=Req) ->
    case nklib_util:apply(Module, object_api_allow, [Cmd, Req]) of
        not_exported ->
            true;
        Other ->
            Other
    end;

%%service_api_allow(_Id, #nkreq{cmd = <<"session", _/binary>>}) ->
%%    true;
%%
service_api_allow(_Id, #nkreq{cmd = <<"event", _/binary>>}) ->
    true;

%%service_api_allow(_Id, #nkreq{cmd = <<"nkadmin", _/binary>>}) ->
%%    true;

service_api_allow(_Id, _Req) ->
    continue.


%% @doc
service_api_cmd(_Id, #nkreq{cmd = <<"objects/", _/binary>>, req_state={Type, Module, Cmd}}=Req) ->
    #nkreq{timeout_pending=Pending} = Req,
    case Pending of
        true ->
            Pid = spawn_link(
                fun() ->
                    Req2 = Req#nkreq{timeout_pending=false},
                    Reply = case erlang:function_exported(Module, object_api_cmd, 2) of
                        true ->
                            apply(Module, object_api_cmd, [Cmd, Req2]);
                        false ->
                            nkdomain_obj_cmd:cmd(Cmd, Type, Req2)
                    end,
                    Reply2 = case Reply of
                        ok ->
                            {ok, #{}, Req2};
                        {ok, UserReply} ->
                            {ok, UserReply, Req2};
                        {error, Error} ->
                            {error, Error, Req2};
                        Other ->
                            Other
                    end,
                    nkservice_api:reply(Reply2)
                end),
            {ack, Pid, Req};
        false ->
            case erlang:function_exported(Module, object_api_cmd, 2) of
                true ->
                    apply(Module, object_api_cmd, [Cmd, Req]);
                false ->
                    nkdomain_obj_cmd:cmd(Cmd, Type, Req)
            end
    end;

service_api_cmd(_Id, _Req) ->
    continue.


%% @private
api_server_reg_down(_Id, {nkdomain_stop, Module, _Pid}, _Reason, State) ->
    {stop, {module_failed, Module}, State};

api_server_reg_down(_Id, _Link, _Reason, _State) ->
    continue.

%% @doc
api_server_http_auth(_Id, _HttpReq, #nkreq{cmd = <<"objects/user/get_token">>}=NkReq) ->
    {true, <<>>, NkReq};

api_server_http_auth(_Id, HttpReq, #nkreq{}=Req) ->
    Headers = nkapi_server_http:get_headers(HttpReq),
    Token = nklib_util:get_value(<<"x-netcomposer-auth">>, Headers, <<>>),
    case nkdomain_api_util:check_token(Token, Req) of
        {ok, UserId, Req2} ->
            {true, UserId, Req2};
        {error, _Error} ->
            false
    end.

%%%% @doc
%%api_server_handle_info({nkdist, {sent_link_down, Link}}, State) ->
%%    nkapi_server:stop(self(), {sent_link_down, Link}),
%%    {ok, State};
%%
%%api_server_handle_info(_Info, _State) ->
%%    continue.


%% ===================================================================
%% Plugin callbacks
%% ===================================================================


%% @private
service_init(_Service, State) ->
    nkdomain_nkroot_plugin:init(State).


%% @private
service_handle_cast(nkdomain_load_domain, State) ->
    #{id:=SrvId} = State,
    #{domain:=Domain} = SrvId:config(),
    case nkdomain_lib:load(Domain) of
        #obj_id_ext{type = ?DOMAIN_DOMAIN, obj_id=ObjId, path=Path, pid=Pid} ->
            lager:info("Service loaded domain ~s (~s)", [Path, ObjId]),
            monitor(process, Pid),
            DomainData = #{
                domain_obj_id => ObjId,
                domain_path => Path,
                domain_pid => Pid
            },
            nkservice_srv:put(SrvId, nkdomain_data, DomainData),
            State2 = State#{nkdomain => DomainData},
            {noreply, State2};
        {error, Error} ->
            ?LLOG(warning, "could not load domain ~s: ~p", [Domain, Error]),
            {noreply, State}
    end;

service_handle_cast(_Msg, _State) ->
    continue.


%% @private
service_handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case State of
        #{nkdomain:=#{domain_pid:=Pid, domain_path:=Path}} ->
            lager:info("Service received domain '~s' down", [Path]),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
service_handle_info(_Msg, _State) ->
    continue.



%% ===================================================================
%% DB Management
%%
%% This callbacks will be implemented by a plugin like nkdomain_store_es
%% ===================================================================


%% @doc Called to initialize the database
-spec object_db_init(nkservice:service()) ->
    {ok, nkservice:service()}| {error, term()}.

object_db_init(_State) ->
    {error, db_not_defined}.



%% @doc Reads an object from main database
-spec object_db_read(obj_id()) ->
    {ok, nkdomain:obj(), Meta::map()} | {error, term()}.

object_db_read(_ObjId) ->
    {error, db_not_defined}.


%% @doc Saves an object to database
-spec object_db_save(nkdomain:obj()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_save(_Obj) ->
    {error, db_not_defined}.


%% @doc Deletes an object from database
-spec object_db_delete(nkdomain:obj_id()) ->
    {ok, Meta::map()} | {error, term()}.

object_db_delete(_ObjId) ->
    {error, db_not_defined}.


%% @doc Finds an object from its ID or Path
-spec object_db_find_obj(nkdomain:id()) ->
    {ok, nkdomain:type(), nkdomain:obj_id(), nkdomain:path()} | {error, object_not_found|term()}.

object_db_find_obj(_ObjId) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_types(obj_id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), integer()}]} | {error, term()}.

object_db_search_types(_ObjId, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_all_types(path(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{type(), integer()}]} | {error, term()}.

object_db_search_all_types(_ObjId, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_childs(obj_id(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_childs(_ObjId, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_all_childs(path(), nkdomain:search_spec()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_all_childs(_Path, _Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_alias(nkdomain:alias()) ->
    {ok, Total::integer(), [{Srv::binary(), type(), obj_id(), path()}]} |
    {error, term()}.

object_db_search_alias(_Alias) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search(nkdomain:search_spec()) ->
    {ok, Total::integer(), Objs::[map()], map(), Meta::map()} |
    {error, term()}.

object_db_search(_Spec) ->
    {error, db_not_defined}.


%% @doc
-spec object_db_search_agg_field(nkdomain:id(), binary(),
                                 nkdomain:search_spec(), SubChilds::boolean()) ->
                                    {ok, Total::integer(), [{nkdomain:type(), integer()}], Map::map()} | {error, term()}.

object_db_search_agg_field(_Id, _Field, _Spec, _SubChilds) ->
    {error, db_not_defined}.


%% @doc Must stop loaded objects
-spec object_db_delete_all_childs(path(), nkdomain:search_spec()) ->
    {ok, Total::integer()} | {error, term()}.

object_db_delete_all_childs(_Path, _Spec) ->
    {error, db_not_defined}.


%% @doc Called to perform a cleanup of the store (expired objects, etc.)
%% Should call object_do_active/3 for each 'active' object found
-spec object_db_clean() ->
    ok | {error, term()}.

object_db_clean() ->
    {error, db_not_defined}.


