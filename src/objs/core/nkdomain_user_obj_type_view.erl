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

-export([view/1, table_data/2, element_updated/3]).

-include("nkdomain.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

-define(ID, <<"domain_detail_type_view__user">>).
-define(ID_SUBDOMAINS, <<"domain_detail_type_view__user__subdomains">>).

%% @doc
view(Session) ->
    Spec = #{
        table_id => ?ID,
        subdomains_id => ?ID_SUBDOMAINS,
        filters => [?ID_SUBDOMAINS],
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
                options => get_domains(Session)
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
                options => get_creators(Session),
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
        on_click => [
%            #{
%                id => <<"fa-times">>,
%                type => enable
%            },
%            #{
%                id => <<"fa-check">>,
%                type => disable
%            },
%            #{
%                id => <<"fa-trash">>,
%                type => delete
%            }
        ]},
    Table = #{
        id => ?ID,
        class => webix_ui,
        value => nkadmin_webix_datatable:datatable(Spec, Session)
    },
    KeyData = #{data_fun => fun ?MODULE:table_data/2},
    Session2 = nkadmin_util:set_key_data(?ID, KeyData, Session),
    {Table, Session2}.


%% @private
get_domains(#admin_session{srv_id=SrvId, domain_id=DomainId}) ->
    case nkdomain:search_agg_field(SrvId, DomainId, <<"domain_id">>, #{size=>50}, true) of
        {ok, _N, Data, #{agg_sum_other:=SumOther}} ->
            Base = case SumOther of
                0 ->
                    [];
                _ ->
                    [#{id => <<"...">>, value => <<"...">>}]
            end,
            Data2 = lists:foldl(
                fun({ObjId, _Num}, Acc) ->
                    case nkdomain:get_name(SrvId, ObjId) of
                        {ok, #{name:=Name}} ->
                            [#{ id => ObjId, value => Name} | Acc];
                        _ ->
                            Acc
                    end
                end,
                Base,
                lists:reverse(Data)),
            [#{id => <<>>, value => <<>>} | Data2];
        {error, _Error} ->
            #{}
    end.


%% @private
get_creators(#admin_session{srv_id=SrvId, domain_id=DomainId}) ->
    case nkdomain:search_agg_field(SrvId, DomainId, <<"created_by">>, #{size=>50}, true) of
        {ok, _N, Data, #{agg_sum_other:=SumOther}} ->
            Base = case SumOther of
                0 ->
                    [];
                _ ->
                    [#{id => <<"...">>, value => <<"...">>}]
            end,
            Data2 = lists:foldl(
                fun({ObjId, _Num}, Acc) ->
                    case nkdomain:get_name(SrvId, ObjId) of
                        {ok, #{name:=Name}} ->
                            [#{ id => ObjId, value => Name} | Acc];
                        _ ->
                            Acc
                    end
                end,
                Base,
                lists:reverse(Data)),
            [#{id => <<>>, value => <<>>} | Data2];
        {error, _Error} ->
            #{}
    end.




%% @doc
table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, #admin_session{srv_id=SrvId, domain_id=DomainId}) ->
    SortSpec = case Sort of
        {<<"obj_name">>, Order} ->
            <<Order/binary, ":path">>;
        {<<"domain">>, Order} ->
            <<Order/binary, ":path">>;
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
    case table_filter(maps:to_list(Filter), #{timezone_offset => Offset}, #{type=>user}) of
        {ok, Filters} -> 
            lager:warning("NKLOG Filters ~s", [nklib_json:encode_pretty(Filters)]),

            FindSpec = #{
                filters => Filters,
                fields => [<<"path">>,
                           <<"obj_name">>,
                           <<"created_time">>,
                           <<"created_by">>,
                           <<"user.name">>, <<"user.surname">>, <<"user.email">>],
                sort => SortSpec,
                from => Start,
                size => Size
            },
            Fun = case Filter of
                #{?ID_SUBDOMAINS := 0} -> search;
                _ -> search_all
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
table_filter([], _Info, Acc) ->
    {ok, Acc};

table_filter([{_, <<>>}|Rest], Info, Acc) ->
    table_filter(Rest, Info, Acc);

table_filter([{<<"domain">>, <<"...">>}|Rest], Info, Acc) ->
    table_filter(Rest, Info, Acc);

table_filter([{<<"domain">>, Data}|Rest], Info, Acc) ->
    Acc2 = Acc#{<<"domain_id">> => Data},
    table_filter(Rest, Info, Acc2);

table_filter([{<<"obj_name">>, Data}|Rest], Info, Acc) ->
    Acc2 = Acc#{<<"obj_name">> => nkdomain_admin_util:search_spec(Data)},
    table_filter(Rest, Info, Acc2);

table_filter([{<<"email">>, Data}|Rest], Info, Acc) ->
    Acc2 = Acc#{<<"user.email">> => nkdomain_admin_util:search_spec(Data)},
    table_filter(Rest, Info, Acc2);

table_filter([{<<"name">>, Data}|Rest], Info, Acc) ->
    Acc2 = Acc#{<<"user.fullname_norm">> => nkdomain_admin_util:search_spec(Data)},
    table_filter(Rest, Info, Acc2);

table_filter([{<<"created_by">>, <<"...">>}|Rest], Info, Acc) ->
    table_filter(Rest, Info, Acc);

table_filter([{<<"created_by">>, Data}|Rest], Info, Acc) ->
    Acc2 = Acc#{<<"created_by">> => Data},
    table_filter(Rest, Info, Acc2);

table_filter([{<<"created_time">>, <<"custom">>}|_Rest], _Acc, _Info) ->
    {error, date_needs_more_data};

table_filter([{<<"created_time">>, Data}|Rest], #{timezone_offset:=Offset} = Info, Acc) ->
    Secs = Offset * 60,
    Filter = case Data of
        <<"today">> ->
            nkdomain_admin_util:time(today, Secs);
        <<"yesterday">> ->
            nkdomain_admin_util:time(yesterday, Secs);
        <<"last_7">> ->
            nkdomain_admin_util:time(last7, Secs);
        <<"last_30">> ->
            nkdomain_admin_util:time(last30, Secs);
        <<"custom">> ->
            <<"">>;
        _ ->
            <<"">>
    end,
    Acc2 = Acc#{<<"created_time">> => Filter},
    table_filter(Rest, Info, Acc2);

table_filter([_|Rest], Info, Acc) ->
    table_filter(Rest, Info, Acc).



%% @private
table_iter([], _Pos, Acc) ->
    lists:reverse(Acc);

table_iter([Entry|Rest], Pos, Acc) ->
    #{
        <<"obj_id">> := ObjId,
        <<"path">> := Path,
        <<"created_by">> := CreatedBy,
        <<"created_time">> := CreatedTime,
        <<"user">> := #{
            <<"name">> := Name,
            <<"surname">> := Surname
        } = User
    } = Entry,
    Email = maps:get(<<"email">>, User, <<>>),
    Enabled = maps:get(<<"enabled">>, Entry, true),
    {ok, Domain, ShortName} = nkdomain_util:get_parts(?DOMAIN_USER, Path),
    Data = #{
        checkbox => <<"0">>,
        pos => Pos,
        id => ObjId,
        obj_name => <<"<a href=\"#_id/", ObjId/binary, "\">", ShortName/binary, "</a>">>,
        domain => Domain,
        name => Name,
        surname => Surname,
        email => Email,
        created_by => <<"<a href=\"#_id/", CreatedBy/binary, "\">", CreatedBy/binary, "</a>">>,
        created_time => CreatedTime,
        enabled => Enabled
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
