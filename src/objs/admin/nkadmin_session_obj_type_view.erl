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

-module(nkadmin_session_obj_type_view).
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
                fillspace => <<"0.5">>,
                name => domain_column_domain,
                is_html => true,
                sort => true,
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
                id => created_by,
                type => text,
                name => domain_column_created_by,
                options => get_agg_name(<<"created_by">>, Path, Session),
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => created_time,
                fillspace => <<"0.5">>,
                type => date,
                name => domain_column_created_time,
                sort => true
            }
        ],
        left_split => 1,
        on_click => []
    }.


fields() ->
    [
        <<"path">>,
        <<"obj_name">>,
        <<"created_time">>,
        <<"created_by">>,
        <<"enabled">>
    ].


%% @doc
sort_field(_) -> <<>>.


%% @doc
filter_field(_Field, _Data, Acc) ->
    Acc.


%% @doc
entry(_Entry, Base) ->
    Base.



%%
%%%% @doc
%%table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, _Opts, #admin_session{domain_id=DomainId}) ->
%%    SortSpec = case Sort of
%%        {<<"obj_name">>, Order} ->
%%            <<Order/binary, ":obj_name">>;
%%        {<<"domain">>, Order} ->
%%            <<Order/binary, ":path">>;
%%        {<<"service">>, Order} ->
%%            <<Order/binary, ":srv_id">>;
%%        {Field, Order} when Field==<<"created_time">> ->
%%            <<Order/binary, $:, Field/binary>>;
%%        _ ->
%%            <<"desc:path">>
%%    end,
%%    %% Get the timezone_offset from the filter list and pass it to table_filter
%%    Offset = maps:get(<<"timezone_offset">>, Filter, 0),
%%    case table_filter(maps:to_list(Filter), #{timezone_offset => Offset}, #{type=>?DOMAIN_ADMIN_SESSION}) of
%%        {ok, Filters} ->
%%            % lager:warning("NKLOG Filters ~s", [nklib_json:encode_pretty(Filters)]),
%%            FindSpec = #{
%%                filters => Filters,
%%                fields => [
%%                    <<"path">>,
%%                    <<"obj_name">>,
%%                    <<"srv_id">>,
%%                    <<"created_time">>,
%%                    <<"created_by">>,
%%                    <<"enabled">>
%%                    ],
%%                sort => SortSpec,
%%                from => Start,
%%                size => Size
%%            },
%%            SubDomainsFilterId = nkdomain_admin_util:make_type_view_subfilter_id(?DOMAIN_ADMIN_SESSION),
%%            Fun = case maps:get(SubDomainsFilterId, Filter, 1) of
%%                0 -> search;
%%                1 -> search_all
%%            end,
%%            case nkdomain_domain:Fun(DomainId, FindSpec) of
%%                {ok, Total, List, _Meta} ->
%%                    Data = table_iter(List, Start+1, []),
%%                    {ok, Total, Data};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%%% @private
%%table_filter([], _Info, Acc) ->
%%    {ok, Acc};
%%
%%table_filter([Term|Rest], Info, Acc) ->
%%    case nkdomain_admin_util:table_filter(Term, Info, Acc) of
%%        {ok, Acc2} ->
%%            table_filter(Rest, Info, Acc2);
%%        {error, Error} ->
%%            {error, Error};
%%        unknown ->
%%            table_filter(Rest, Info, Acc)
%%    end.
%%
%%
%%
%%%% @private
%%table_iter([], _Pos, Acc) ->
%%    lists:reverse(Acc);
%%
%%table_iter([Entry|Rest], Pos, Acc) ->
%%    Base = nkdomain_admin_util:table_entry(?DOMAIN_ADMIN_SESSION, Entry, Pos),
%%    Data = Base#{
%%    },
%%    table_iter(Rest, Pos+1, [Data|Acc]).


%% @private
element_updated(_ObjId, _Value, _Session) ->
    #{}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, ?DOMAIN_ADMIN_SESSION, Path, Session).



