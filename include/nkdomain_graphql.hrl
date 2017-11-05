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

-ifndef(NKDOMAIN_GRAPHQL_HRL_).
-define(NKDOMAIN_GRAPHQL_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================

-record(page_info, {
    has_next_page :: boolean(),
    has_previous_page :: boolean()
}).


-record(search_results, {
    objects = [] :: [{nkdomain:obj_id_ex(), nkdomain:obj()}],
    total_count = 0 :: integer(),
    page_info :: #page_info{},
    cursor :: binary
}).




-endif.

