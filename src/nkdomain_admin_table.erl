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

%% @doc
-module(nkdomain_admin_table).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([table_data/8]).

%%-include("nkdomain.hrl").
%%-include("nkdomain_admin.hrl").
%%-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAN Admin " ++ Txt, Args)).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
table_data(Type, Mod, Start, Size, Sort, Filter, _Opts, #admin_session{domain_id=DomainId}) ->
    %% lager:error("NKLOG FF ~p", [Filter]),
    SortSpec = case Sort of
        {<<"obj_name">>, Order} ->
            <<Order/binary, ":obj_name">>;
        {<<"domain">>, Order} ->
            <<Order/binary, ":path">>;
        {Field, Order} when Field==<<"created_time">> ->
            <<Order/binary, $:, Field/binary>>;
        {Field, Order} ->
            case erlang:function_exported(Mod, sort_field, 1) andalso Mod:sort_field(Field) of
                false ->
                    <<"desc:path">>;
                <<>> ->
                    <<"desc:path">>;
                SubField ->
                    <<Order/binary, $:, SubField/binary>>
            end;
        _ ->
            <<"desc:path">>
    end,
    %% Get the timezone_offset from the filter list and pass it to table_filter
    Offset = maps:get(<<"timezone_offset">>, Filter, 0),
    case table_filter(maps:to_list(Filter), Mod, #{timezone_offset => Offset}, [{<<"type">>, eq, Type}]) of
        {ok, Filters2} ->
            % lager:warning("NKLOG Filters ~s", [nklib_json:encode_pretty(Filters)]),
            FindOpts1 = #{
                fields => Mod:fields(),
                sort => SortSpec,
                from => Start,
                size => Size
            },
            SubDomainsFilterId = nkdomain_admin_util:make_type_view_subfilter_id(Type),
            SubDomainsDeletedId = nkdomain_admin_util:make_type_view_showdeleted_id(Type),
            FindOpts2 = case maps:get(SubDomainsFilterId, Filter, 1) of
                0 -> FindOpts1;
                1 -> FindOpts1#{deep=>true}
            end,
            FindOpts3 = case maps:get(SubDomainsDeletedId, Filter, 1) of
                0 -> FindOpts2;
                1 -> FindOpts2#{get_deleted=>true}
            end,
            %% lager:error("NKLOG FILTERD ~p", [Filters2]),
            case nkdomain_db:search(core, {query_graphql, DomainId, Filters2, FindOpts3}) of
                {ok, Total, List, _Meta} ->
                    Data = table_iter(List, Mod, Type, Start+1, []),
                    {ok, Total, Data};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
table_filter([], _Mod, _Info, Acc) ->
    {ok, Acc};

table_filter([{Field, Data}|Rest], Mod, Info, Acc) ->
    case nkdomain_admin_util:table_filter(Field, Data, Info, Acc) of
        {ok, Acc2} ->
            table_filter(Rest, Mod, Info, Acc2);
        {error, Error} ->
            {error, Error};
        unknown ->
            case erlang:function_exported(Mod, filter_field, 3) andalso Mod:filter_field(Field, Data, Acc) of
                false ->
                    table_filter(Rest, Mod, Info, Acc);
                Acc2 ->
                    table_filter(Rest, Mod, Info, Acc2)
            end
    end.


%% @private
table_iter([], _Mod, _Type, _Pos, Acc) ->
    lists:reverse(Acc);

table_iter([Entry|Rest], Mod, Type, Pos, Acc) ->
    Base = nkdomain_admin_util:table_entry(Type, Entry, Pos),
    Data = case erlang:function_exported(Mod, entry, 2) andalso Mod:entry(Entry, Base) of
        false ->
            Base;
        UserEntry ->
            UserEntry
    end,
    table_iter(Rest, Mod, Type, Pos+1, [Data|Acc]).



