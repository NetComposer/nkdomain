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

%% @doc Image Job Object Type View

-module(nkdomain_image_job_obj_type_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/2, fields/0, sort_field/1, filter_field/3, entry/2, element_updated/3]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkimage/include/nkimage.hrl").


%% @doc
view(Path, Session) ->
    #{
        columns => [
            #{
                id => checkbox,
                type => checkbox
            },
            #{
                id => domain,
                type => text,
                name => domain_column_domain,
                sort => true,
                is_html => true,
                options => get_agg_name(<<"domain_id">>, Path, Session)
            },
            #{
                id => obj_name,
                type => text,
                name => domain_column_id,
                sort => true,
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => status,
                type => text,
                name => domain_column_status,
                options => get_agg_term(<<?IMAGE_JOB/binary, ".status">>, Path, Session),
                sort => false % true
            },
            #{
                id => created_by,
                type => text,
                name => domain_column_created_by,
                sort => true,
                options => get_agg_name(<<"created_by">>, Path, Session),
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => created_time,
                type => date,
                name => domain_column_created_time,
                sort => true
            }
        ],
        left_split => 1,
        on_click => []
    }.


%% @doc
fields() ->
    [
        <<"path">>,
        <<"obj_name">>,
        <<"created_time">>,
        <<"created_by">>,
        <<"enabled">>,
        <<?IMAGE_JOB/binary, ".status">>
    ].


%% @doc
sort_field(<<"status">>) -> <<?IMAGE_JOB/binary, ".status">>;
sort_field(_) -> <<>>.


%% @doc
filter_field(<<"status">>, Data, Acc) ->
    nkdomain_admin_util:add_filter(<<?IMAGE_JOB/binary, ".status">>, Data, Acc);
filter_field(_Field, _Data, Acc) ->
    Acc.


%% @doc
entry(Entry, Base) ->
    #{
        ?IMAGE_JOB := #{
            <<"status">> := Status
        }
    } = Entry,
    Base#{
        status => Status
    }.


%% @private
element_updated(_ObjId, _Value, _Session) ->
    {ok, #{}}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, ?IMAGE_JOB, Path, Session).


%% @private
get_agg_term(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_term(Field, ?IMAGE_JOB, Path, Session).