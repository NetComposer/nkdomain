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

-export([view/1, table_data/2]).

-include("nkdomain.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

-define(ID, <<"domain_detail_user_table">>).
-define(ID_SUBDOMAINS, <<"domain_detail_user_table_subdomains">>).

%% @doc
view(Session) ->
    DomainOptions = [
        #{ id => <<"">>, value => <<"">> },
        #{ id => <<"/">>, value => <<"/">> },
        #{ id => <<"/c4/">>, value => <<"/c4">> }
    ],
    Spec = #{
        table_id => ?ID,
        subdomains_id => ?ID_SUBDOMAINS,
        filters => [?ID_SUBDOMAINS],
        columns => [
            #{
                id => pos,
                type => pos,
                name => domain_column_pos
            },
            #{
                id => domain,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_domain,
                sort => true,
                options => DomainOptions
            },
            #{
                id => html_id,
                type => text,
                name => domain_column_id,
                sort => false,
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => name,
                type => text,
                header_colspan => 2,
                filter_colspan => 2,
                fillspace => <<"0.5">>,
                name => domain_column_name,
                sort => true,
                editor => text
            },
            #{
                id => surname,
                type => text,
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
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => created_time,
                type => date,
                name => domain_column_created_time,
                sort => true
            },
            #{
                id => enabled_icon,
                type => {icon, <<"enabled_icon">>}
            },
            #{
                id => delete,
                type => {fixed_icon, <<"fa-trash">>}
            }
        ],
        left_split => 1,
        right_split => 2,
        on_click => [
            #{
                id => <<"fa-times">>,
                type => enable
            },
            #{
                id => <<"fa-check">>,
                type => disable
            },
            #{
                id => <<"fa-trash">>,
                type => delete
            }
        ]},
    Table = #{
        id => ?ID,
        class => webix_ui,
        value => nkadmin_webix_datatable:datatable(Spec, Session)
    },
    KeyData = #{data_fun => fun ?MODULE:table_data/2},
    Session2 = nkadmin_util:set_key_data(?ID, KeyData, Session),
    {Table, Session2}.


%% @doc
table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, #admin_session{srv_id=SrvId, domain_id=DomainId}) ->
    SortSpec = case Sort of
        {<<"domain">>, Order} ->
            <<Order/binary, ":path">>;
        {<<"name">>, Order} ->
            <<Order/binary, ":user.name.keyword">>;
        {<<"surname">>, Order} ->
            <<Order/binary, ":user.surname.keyword">>;
        {<<"email">>, Order} ->
            <<Order/binary, ":user.email">>;
        {Field, Order} when Field==<<"created_by">>; Field==<<"created_time">> ->
            <<Order/binary, $:, Field/binary>>;
        _ ->
            <<"desc:path">>
    end,
    %% Get the timezone_offset from the filter list and pass it to table_filter
    ClientTimeOffset = maps:get(<<"timezone_offset">>, Filter, <<"0">>),
    case table_filter(maps:to_list(Filter), #{type=>user}, #{timezone_offset => ClientTimeOffset}) of
        {ok, Filters} -> 
            FindSpec = #{
                filters => Filters,
                fields => [<<"path">>, <<"created_by">>, <<"created_time">>,
                           <<"user.name">>, <<"user.surname">>, <<"user.email">>],
                sort => SortSpec,
                from => Start,
                size => Size
            },
            Fun = case Filter of
                #{?ID_SUBDOMAINS := 0} -> find;
                _ -> find_all
            end,
            case nkdomain_domain_obj:Fun(SrvId, DomainId, FindSpec) of
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
table_filter([], Acc, _) ->
    {ok, Acc};

table_filter([{_, <<>>}|Rest], Acc, Info) ->
    table_filter(Rest, Acc, Info);

table_filter([{<<"domain">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"path">> => nkdomain_admin_detail:search_spec(<<Data/binary, "user">>)},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"email">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"user.email">> => nkdomain_admin_detail:search_spec(Data)},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"name">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{
        <<"or">> => [
            #{ <<"user.name">> => nkdomain_admin_detail:search_spec(Data) },
            #{ <<"user.surname">> => nkdomain_admin_detail:search_spec(Data) }
        ]
    },
    table_filter(Rest, Acc2, Info);

table_filter([{<<"created_by">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"created_by">> => nkdomain_admin_detail:search_spec(Data)},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"created_time">>, <<"custom">>}|_Rest], _Acc, _Info) ->
    {error, date_needs_more_data};

table_filter([{<<"created_time">>, Data}|Rest], Acc, #{timezone_offset:=Offset}=Info) ->
    SNow = nklib_util:timestamp(),
    {_,{H,M,S}} = nklib_util:timestamp_to_gmt(SNow),
    Now = SNow - H*3600 - M*60 - S,
    OffsetSecs = Offset * 60,
    io:format("Filter: ~w~nNow: ~w~n", [Data, Now]),
    case Data of
        <<"today">> ->
            Now2 = (Now - 24*60*60 + OffsetSecs)*1000,
            Filter = list_to_binary([">", nklib_util:to_binary(Now2)]);
        <<"yesterday">> ->
            Now2 = (Now - 2*24*60*60 + OffsetSecs)*1000,
            Now3 = (Now - 24*60*60 + OffsetSecs)*1000,
            Filter = list_to_binary(["<", nklib_util:to_binary(Now2), "-", nklib_util:to_binary(Now3),">"]);
        <<"last_7">> ->
            Now2 = (Now - 7*24*60*60 + OffsetSecs)*1000,
            Filter = list_to_binary([">", nklib_util:to_binary(Now2)]);
        <<"last_30">> ->
            Now2 = (Now - 30*24*60*60 + OffsetSecs)*1000,
            Filter = list_to_binary([">", nklib_util:to_binary(Now2)]);
        <<"custom">> ->
            Filter = <<"">>;
        _ ->
            Filter = <<"">>
    end,
    Acc2 = Acc#{<<"created_time">> => Filter},
    table_filter(Rest, Acc2, Info);

table_filter([_|Rest], Acc, Info) ->
    table_filter(Rest, Acc, Info).



%% @private
table_iter([], _Pos, Acc) ->
    lists:reverse(Acc);

table_iter([Entry|Rest], Pos, Acc) ->
    #{
        <<"obj_id">> := ObjId,
        <<"path">> := Path,
        <<"created_time">> := CreatedTime,
        <<"user">> := #{
            <<"name">> := Name,
            <<"surname">> := Surname
        } = User
    } = Entry,
    Email = maps:get(<<"email">>, User, <<>>),
    CreatedBy = maps:get(<<"created_by">>, Entry, <<>>),
    Enabled = case maps:get(<<"enabled">>, Entry, true) of
        true -> <<"fa-times">>;
        false -> <<"fa-check">>
    end,
    DomainUsers = nkdomain_util:class(?DOMAIN_USER),
    {ok, Domain, _ShortName} = nkdomain_util:get_parts(?DOMAIN_USER, Path),
    Data = #{
        pos => Pos,
        id => ObjId,
        html_id => <<"<a href=\"#/", DomainUsers/binary, "/", ObjId/binary, "\">", ObjId/binary, "</a>">>,
        domain => Domain,
        name => Name,
        surname => Surname,
        email => Email,
        created_by => <<"<a href=\"#/", DomainUsers/binary, "/", CreatedBy/binary, "\">", CreatedBy/binary, "</a>">>,
        created_time => CreatedTime,
        enabled_icon => Enabled
    },
    table_iter(Rest, Pos+1, [Data|Acc]).

