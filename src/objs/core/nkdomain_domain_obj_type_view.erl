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

-module(nkdomain_domain_obj_type_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/2, fields/0, sort_field/1, filter_field/3, entry/2, element_updated/3]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

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
                fillspace => <<"1.5">>,
                type => text,
                name => domain_column_domain,
                is_html => true,
                sort => true,
                is_html => true,
                options => get_agg_name(<<"domain_id">>, Path, Session)
            },
            #{
                id => name,
                type => text,
                name => domain_column_name,
                sort => true,
                editor => text
            },
            #{
                id => created_by,
                fillspace => <<"0.5">>,
                type => text,
                name => domain_column_created_by,
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
%        right_split => 2,
        on_click => []
    }.



%% @doc
fields() ->
    [
         <<"path">>,
         <<"srv_id">>,
         <<"created_time">>,
         <<"created_by">>,
         <<"enabled">>,
         <<"name">>
    ].


%% @doc
sort_field(_) -> <<>>.


%% @doc
filter_field(_Field, _Data, Acc) ->
    Acc.


%% @doc
entry(Entry, Base) ->
    #{
        <<"path">> := Path
    } = Entry,
    Base#{
        domain => nkdomain_admin_util:obj_path_url(Path, Path),
        name => maps:get(<<"name">>, Entry, <<>>)
    }.



%% @private
element_updated(_ObjId, Value, _Session) ->
    #{
        <<"name">> := Name
    } = Value,
    Update = #{name => Name},
    {ok, Update}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, <<>>, Path, Session).
