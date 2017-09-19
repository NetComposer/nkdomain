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

-export([view/2, table_data/3, element_updated/3]).

-include("nkdomain.hrl").
-include("nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

%% @doc
view(Path, Session) ->
    TableId = nkdomain_admin_util:make_type_view_id(?DOMAIN_USER),
    SubDomainsFilterId = nkdomain_admin_util:make_type_view_subfilter_id(?DOMAIN_USER),
    DeletedFilterId = nkdomain_admin_util:make_type_view_delfilter_id(?DOMAIN_USER),
    Spec = #{
        table_id => TableId,
        subdomains_id => SubDomainsFilterId,
        deleted_id => DeletedFilterId,
        filters => [SubDomainsFilterId, DeletedFilterId],
        base_domain => Path,
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
                options => get_agg_name(<<"domain_id">>, Path)
            },
            #{
                id => service,
                type => text,
                name => domain_column_service,
                sort => true,
                options => get_agg_srv_id(Path)
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
                id => name,
                type => text,
                header_colspan => 1,
                filter_colspan => 2,
                fillspace => <<"0.5">>,
                name => domain_column_name,
                sort => true,
                editor => text
            },
            #{
                id => surname,
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
                name => domain_column_created_by,
                sort => true,
                options => get_agg_name(<<"created_by">>, Path),
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
    },
    Table = #{
        id => TableId,
        class => webix_ui,
        value => nkadmin_webix_datatable:datatable(Spec, Session)
    },
    {Table, Session}.



%% @doc
table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, _Opts, #admin_session{domain_id=DomainId}) ->
    lager:error("NKLOG FF ~p", [Filter]),
    SortSpec = case Sort of
        {<<"obj_name">>, Order} ->
            <<Order/binary, ":obj_name">>;
        {<<"domain">>, Order} ->
            <<Order/binary, ":path">>;
        {<<"service">>, Order} ->
            <<Order/binary, ":srv_id">>;
        {<<"name">>, Order} ->
            <<Order/binary, ":user.name_sort">>;
        {<<"surname">>, Order} ->
            <<Order/binary, ":user.surname_sort">>;
        {<<"email">>, Order} ->
            <<Order/binary, ":user.email">>;
        {Field, Order} when Field==<<"created_time">> ->
            <<Order/binary, $:, Field/binary>>;
        _ ->
            <<"desc:path">>
    end,
    %% Get the timezone_offset from the filter list and pass it to table_filter
    Offset = maps:get(<<"timezone_offset">>, Filter, 0),
    case table_filter(maps:to_list(Filter), #{timezone_offset => Offset}, #{type=>?DOMAIN_USER}) of
        {ok, Filters} -> 
            % lager:warning("NKLOG Filters ~s", [nklib_json:encode_pretty(Filters)]),
            FindSpec = #{
                filters => Filters,
                fields => [<<"path">>,
                           <<"obj_name">>,
                           <<"srv_id">>,
                           <<"created_time">>,
                           <<"created_by">>,
                           <<"enabled">>,
                           <<"user.name">>, <<"user.surname">>, <<"user.email">>],
                sort => SortSpec,
                from => Start,
                size => Size
            },
            TableId = nkdomain_admin_util:make_type_view_id(?DOMAIN_USER),
            SubDomainsFilterId = nkdomain_admin_util:make_type_view_subfilter_id(?DOMAIN_USER),
            Fun = case maps:get(SubDomainsFilterId, Filter, 1) of
                0 -> search;
                1 -> search_all
            end,
            io:format("~nSEARCH ~p~n", [Fun]),
            case nkdomain_domain_obj:Fun(DomainId, FindSpec) of
                {ok, Total, List, _Meta} ->
                    Data = table_iter(List, Start+1, []),
                    {ok, Total, Data};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
table_filter([], _Info, Acc) ->
    {ok, Acc};

table_filter([Term|Rest], Info, Acc) ->
    case nkdomain_admin_util:table_filter(Term, Info, Acc) of
        {ok, Acc2} ->
            table_filter(Rest, Info, Acc2);
        {error, Error} ->
            {error, Error};
        unknown ->
            case Term of
                {<<"email">>, Data} ->
                    Acc2 = Acc#{<<"user.email">> => nkdomain_admin_util:search_spec(Data)},
                    table_filter(Rest, Info, Acc2);
                _ ->
                    table_filter(Rest, Info, Acc)
            end
    end.



%% @private
table_iter([], _Pos, Acc) ->
    lists:reverse(Acc);

table_iter([Entry|Rest], Pos, Acc) ->
    Base = nkdomain_admin_util:table_entry(?DOMAIN_USER, Entry, Pos),
    #{
        <<"user">> := #{
            <<"name">> := Name,
            <<"surname">> := Surname
        } = User
    } = Entry,
    Email = maps:get(<<"email">>, User, <<>>),
    Data = Base#{
        name => Name,
        surname => Surname,
        email => Email
    },
    table_iter(Rest, Pos+1, [Data|Acc]).


%% @private
element_updated(_ObjId, Value, _Session) ->
    #{
        <<"name">> := Name,
        <<"surname">> := Surname,
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
get_agg_name(Field, Path) ->
    nkdomain_admin_util:get_agg_name(Field, ?DOMAIN_USER, Path).


%% @private
get_agg_srv_id(Path) ->
    nkdomain_admin_util:get_agg_srv_id(?DOMAIN_USER, Path).


