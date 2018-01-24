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

-module(nkdomain_user_obj_type_view).
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
                type => text,
                fillspace => <<"1.5">>,
                name => domain_column_domain,
                sort => true,
                is_html => true,
                options => get_agg_name(<<"domain_id">>, Path, Session)
            },
            #{
                id => obj_name,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_id,
                sort => true,
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => user_name,
                type => text,
                header_colspan => 1,
                filter_colspan => 2,
                fillspace => <<"0.5">>,
                name => domain_column_name,
                sort => true,
                editor => text
            },
            #{
                id => user_surname,
                type => text,
                name => domain_column_lastname,
                sort => true,
                editor => text
            },
            #{
                id => email,
                type => text,
                name => domain_column_email,
                sort => true,
                editor => text
            },
            #{
                id => created_by,
                type => text,
                fillspace => <<"0.5">>,
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
%        right_split => 2,
        on_click => []
    }.


fields() ->
    [<<"path">>,
     <<"obj_name">>,
     <<"created_time">>,
     <<"created_by">>,
     <<"enabled">>,
     <<"user.name">>, <<"user.surname">>, <<"user.email">>].


%% @doc
sort_field(<<"user_name">>) -> <<"user.name_sort">>;
sort_field(<<"user_surname">>) -> <<"user.surname_sort">>;
sort_field(<<"email">>) -> <<"user.email">>;
sort_field(_) -> <<>>.


%% @doc
filter_field(<<"user_name">>, Data, Acc) ->
    nkdomain_admin_util:add_multiword_filter(<<"user.fullname_norm">>, Data, Acc);
filter_field(<<"email">>, Data, Acc) ->
    nkdomain_admin_util:add_search_filter(<<"user.email">>, Data, Acc);
filter_field(_Field, _Data, Acc) ->
    Acc.


%% @doc
entry(Entry, Base) ->
    #{
        <<"user">> := #{
              <<"name">> := Name,
              <<"surname">> := Surname
          } = User
    } = Entry,
    Email = maps:get(<<"email">>, User, <<>>),
    Base#{
        user_name => Name,              %% Column names
        user_surname => Surname,
        email => Email
    }.


%% @private
element_updated(_ObjId, Value, _Session) ->
    #{
        <<"user_name">> := Name,
        <<"user_surname">> := Surname,
        <<"email">> := Email
    } = Value,
    Update = #{
        ?DOMAIN_USER => #{
            name => Name,
            surname => Surname,
            email => Email
        }
    },
    {ok, Update}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, ?DOMAIN_USER, Path, Session).



