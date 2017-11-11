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

%% @doc Mutation callback
-module(nkdomain_graphql_mutation).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([execute/4]).

-include("nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% @doc Called at the beginning of the mutation process
execute(Ctx, _, MutationName, #{<<"input">>:=Params}) ->
    #{nkmeta:=#{start:=Start, srv_id:=SrvId}} = Ctx,
    % Find who is in charge of this query
    case nklib_types:get_module(nkdomain_mutation, MutationName) of
        undefined ->
            {error, unknown_mutation};
        Module ->
            Params2 = nkdomain_util:remove_nulls(Params),
            try
                Res = ?CALL_SRV(SrvId, object_graphql_mutation, [MutationName, Module, Params2, Ctx]),
                lager:info("Mutation time: ~p", [nkdomain_util:timestamp()-Start]),
                Res
            catch
                throw:Throw ->
                    {error, Throw}
            end
    end.

