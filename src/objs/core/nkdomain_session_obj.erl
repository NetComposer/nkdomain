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

%% @doc User Session Object

-module(nkdomain_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/4]).
-export([object_info/0, object_schema_types/0, object_es_mapping/0, object_parse/2,
         object_api_syntax/2, object_api_cmd/2,
         object_init/1, object_stop/2, object_event/2]).
-export([object_admin_info/0]).
-export([object_do_active/1]).

-include("nkdomain.hrl").
-include("nkdomain_debug.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Type
%% ===================================================================

-type create_opts() ::
    #{
        session_link => {module(), pid()},
        session_id => binary(),
        data => #{
            login_meta => map(),
            local => binary(),
            remote => binary()
        }
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Creates a new session
-spec start(nkservice:id(), nkdomain:id(), nkdomain:id(), create_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(SrvId, DomainId, UserId, Opts) ->
        Obj1 = #{
            type => ?DOMAIN_SESSION,
            domain_id => DomainId,
            parent_id => UserId,
            srv_id => SrvId,
            created_by => UserId,
            active => true,
            ?DOMAIN_SESSION => maps:get(data, Opts, #{})
        },
        Obj2 = case Opts of
            #{session_id:=SessId} ->
                Obj1#{obj_id => SessId};
            _ ->
                Obj1
        end,
        CreateOpts = maps:with([session_link], Opts),
        case nkdomain_obj_make:create(Obj2, CreateOpts) of
            {ok, #obj_id_ext{obj_id=SessId2, pid=Pid}, _} ->
                {ok, SessId2, Pid};
            {error, Error} ->
                {error, Error}
        end.


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================

%% @private
object_info() ->
    #{
        type => ?DOMAIN_SESSION,
        schema_type => 'Session',
        stop_after_disabled => true,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 1000,
        type_view_mod => nkdomain_session_obj_type_view
%%        tree_id => <<"domain_tree_sessions_sessions">>
    }.


%% @doc
object_schema_types() ->
    #{
        'Session' => #{
            fields => #{
            },
            is_object => true,
            comment => "An User Session"
        }
    }.


%% @private
object_es_mapping() ->
    #{
        vsn => #{type => keyword},
        local => #{type => keyword},
        remote => #{type => keyword},
        login_meta => #{enabled => false}
    }.


%% @private
object_parse(_Mode, _Obj) ->
    #{
        local => binary,
        remote => binary,
        login_meta => any
    }.


%% @private
object_init(#obj_state{domain_id=DomainId, id=Id, obj=Obj}=State) ->
    %% TODO Link again if moved process
    #obj_id_ext{obj_id=SessId} = Id,
    #{created_by:=UserId} = Obj,
    ok = nkdomain_user:register_session(UserId, DomainId, ?DOMAIN_SESSION, SessId, #{}),
    State2 = nkdomain_obj_util:link_to_session_server(?MODULE, State),
    {ok, State2}.


%% @private
object_stop(_Reason, #obj_state{session_link={Mod, Pid}}=State) ->
    % When the session stops, we stop the WS
    Mod:stop_session(Pid, nkdomain_session_stop),
    {ok, State};

object_stop(_Reason, State) ->
    {ok, State}.


%% @private

%% @doc
object_api_syntax(Cmd, Syntax) ->
    nkdomain_session_obj_syntax:syntax(Cmd, Syntax).


%%%% @private
%%object_send_event(Event, State) ->
%%    nkdomain_user_obj_events:event(Event, State).


%% @private
object_api_cmd(Cmd, Req) ->
    nkdomain_session_obj_cmd:cmd(Cmd, Req).


%% @private
object_event({enabled, false}, State) ->
    nkdomain:unload(self(), session_is_disabled),
    {ok, State};

object_event(_Event, State) ->
    {ok, State}.





%% ===================================================================
%% nkdomain callbacks
%% ===================================================================

%% @private
object_do_active(_Id) ->
    delete_if_not_loaded.



%% ===================================================================
%% Internal
%% ===================================================================


