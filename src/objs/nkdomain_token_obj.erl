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

%% @doc User Object

-module(nkdomain_token_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/4]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/3, object_send_event/2,
         object_sync_op/3, object_async_op/2]).
-export([object_admin_tree/3]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkDOMAIN Token "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec create(nkservice:id(), nkdomain:id(), integer(), map()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, Parent, SecsTTL, Data) when is_integer(SecsTTL), SecsTTL >= 1 ->
    case nkdomain_obj_lib:load(Srv, Parent, #{}) of
        #obj_id_ext{obj_id=ReferredId, type=SubType} ->
            Obj = #{
                parent_id => ReferredId,
                type => ?DOMAIN_TOKEN,
                referred_id => ReferredId,
                subtype => SubType,
                expires_time => nkdomain_util:timestamp() + 1000*SecsTTL,
                ?DOMAIN_TOKEN => Data
            },
            nkdomain_obj_lib:make_and_create(Srv, <<>>, Obj, #{});
        {error, object_not_found} ->
            {error, referred_not_found};
        {error, Error} ->
            {error, Error}
    end.






%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_TOKEN,
        remove_after_stop => true
    }.


%% @private
object_mapping() ->
    disabled.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    any.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_TOKEN, Syntax).


%% @private
object_api_allow(_Cmd, _Req, State) ->
    {true, State}.


%% @private
object_send_event(_Event, Session) ->
    {ok, Session}.


%% @private
object_api_cmd(Cmd, Req, State) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_TOKEN, Req, State).


%% @private
object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op(_Op, _Session) ->
    continue.


%% @doc
object_admin_tree(Category, List, State) ->
    nkdomain_admin:add_tree_session(Category, ?DOMAIN_TOKEN, ?MODULE,
                                    domain_tree_sessions_tokens, 800, List, State).



%% ===================================================================
%% Internal
%% ===================================================================
