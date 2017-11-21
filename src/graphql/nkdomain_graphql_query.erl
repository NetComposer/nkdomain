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

%% @doc Query processor.
-module(nkdomain_graphql_query).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([execute/4]).

-include_lib("nkservice/include/nkservice.hrl").

%% @doc Called at the beginning of the query processing
execute(Ctx, _DummyObj, QueryName, Params) ->
    #{nkmeta:=#{start:=Start, srv_id:=SrvId}} = Ctx,
    % Find who is in charge of this query
    case nklib_types:get_module(nkdomain_query, QueryName) of
        undefined ->
            {error, unknown_query};
        Module ->
            try
                Params2 = nkdomain_util:remove_nulls(Params),
                Res = ?CALL_SRV(SrvId, object_graphql_query, [QueryName, Module, Params2, Ctx]),
                lager:info("Query time: ~p", [nkdomain_util:timestamp()-Start]),
                Res
            catch
                throw:Error ->
                    {error, Error}
            end
    end.

