%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkApps management module.

-module(nkdomain_load).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([load_file/1, load_file/2, load/3, parse/2, load_domains/2]).
-export_type([load_spec/0, load_opts/0, load_result/0]).

-import(nklib_util, [to_binary/1]).


%% ===================================================================
%% Types
%% ===================================================================

-type load_spec() :: string() | binary() | map().

-type load_opts() ::
    #{
        owner => nkdomain:user_obj_id(),
        replace => boolean()               % Removes old entries
    }.


-type load_result() :: 
    loaded | not_modifed | removed | {error, term()}.


%% ===================================================================
%% Public functions
%% ===================================================================


%% @doc Loads a domain configuration from a YAML file
-spec load_file(string()|binary()) ->
    {ok, #{nkdomain:obj_id() => load_result()}} | {error, term()}.

load_file(File) ->
    load_file(File, #{}).


%% @doc Loads a domain configuration from a YAML file
-spec load_file(string()|binary(), load_opts()) ->
    {ok, #{nkdomain:obj_id() => load_result()}} | {error, term()}.

load_file(File, Opts) ->
    Type = case filename:extension(nklib_util:to_binary(File)) of
        <<".yaml">> -> yaml;
        <<".json">> -> json;
        _ -> unknown
    end,
    case file:read_file(File) of
        {ok, Data} ->
            load(Type, Data, Opts);
        {error, enoent} ->
            File1 = filename:join("objects", File),
            case file:read_file(File1) of
                {ok, Data} ->
                    load(Type, Data, Opts);
                {error, enoent} ->
                    {error, {could_not_open, File1}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Loads a domain configuration from an erlang map string or binary
-spec load(map|yaml|json, load_spec(), load_opts()) ->
    {ok, #{nkdomain:obj_id() => load_result()}} | {error, term()}.

load(Type, Data, Opts) ->
    case parse(Type, Data) of
        {ok, Domains} ->
            load_domains(Domains, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Loads a domain configuration from a text representing a YAML file
-spec parse(map|yaml|json, load_spec()) ->
    {ok, [{nkdomain:obj_id(), map()}]} | {error, term()}.

parse(map, Map) when is_map(Map) ->
    case sort_domains(Map) of
        [] ->
            {error, {invalid_map, no_domains}};
        Domains ->
            parse_domains(Domains, [])
    end;

parse(map, _) ->
    {error, {invalid_map, no_domains}};

parse(yaml, Data) when is_list(Data); is_binary(Data) ->
    % Data1 = re:replace(Data, <<"\t">>, <<"    ">>, [global, {return, binary}]),
    Data1 = nklib_util:to_binary(Data),
    case catch yaml:load(Data1) of
        {ok, [Map]} when is_map(Map), map_size(Map)>0 ->
            Domains = sort_domains(Map),
            parse_domains(Domains, []);
        {ok, _} ->
            {error, {invalid_yaml, no_domains}};
        {error, Error} ->
            {error, {invalid_yaml, Error}};
        {'EXIT', Error} ->
            {error, {yaml_parser_error, Error}}
    end;

parse(yaml, _) ->
    {error, {invalid_yaml, no_domains}};

parse(json, Data) when is_list(Data); is_binary(Data) ->
    case nklib_json:decode(Data) of
        Map when is_map(Map), map_size(Map)>0 ->
            Domains = sort_domains(Map),
            parse_domains(Domains, []);
        error ->
            {error, invalid_json};
        _ ->
            {error, {invalid_json, no_domains}}
    end;

parse(json, _) ->
    {error, {invalid_json, no_domains}};

parse(_, _) ->
    {error, invalid_file_type}.


%% @doc Loads a domain configuration from a domain config map
-spec load_domains([{nkdomain:obj_id(), map()}], load_opts()) ->
    {ok, #{nkdomain:obj_id() => load_result()}} | {error, term()}.

load_domains(Domains, Opts) ->
    load_domains(Domains, Opts, #{}).


%% @private
load_domains([], _Opts, Map) ->
    {ok, Map};

load_domains([{Domain, Data}|Rest], Opts, Map) ->
    Res = case nkdomain_obj:load(domain, Domain, Data, Opts) of
        {loaded, _} -> loaded;
        not_modified -> not_modified;
        removed -> removed;
        {error, Error} -> {error, Error}
    end,
    load_domains(Rest, Opts, maps:put(Domain, Res, Map)).



%% ===================================================================
%% Parsers
%% ===================================================================



%% @private
parse_domains([], Acc) ->
    {ok, lists:reverse(Acc)};

%% Data can be list or map
parse_domains([{Domain, Data}|Rest], Acc) ->
    Syntax = (base_syntax())#{
        members => fun parse_domain_obj_role/3,
        status => {enum, [ready, standby, stopping, stopped]},
        alias => {ulist, binary},
        groups => fun parse_domain_obj/3,
        users => fun parse_domain_obj/3,
        nodesets => fun parse_domain_obj/3,
        services => fun parse_domain_obj/3
    },
    Domain1 = nklib_util:to_binary(Domain),
    case binary:match(Domain1, <<"@">>) of
        nomatch ->
            case do_parse(Data, Syntax, Domain1, #{}) of
                {ok, Data1} ->
                    parse_domains(Rest, [{Domain1, Data1}|Acc]);
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {invalid_name, Domain1}}
    end.


%% @private
parse_domain_obj(Key, null, Opts) ->
    parse_domain_obj(Key, [], Opts);

parse_domain_obj(Key, <<>>, Opts) ->
    parse_domain_obj(Key, [], Opts);

parse_domain_obj(groups, Groups, #{path:=Path}) when is_list(Groups); is_map(Groups) ->
    Path1 = <<Path/binary, ".groups">>,
    parse_groups(Path1, nklib_util:to_list(Groups), #{});

parse_domain_obj(users, Users, #{path:=Path}) when is_list(Users); is_map(Users) ->
    Path1 = <<Path/binary, ".users">>,
    parse_users(Path1, nklib_util:to_list(Users), #{});

parse_domain_obj(services, Srvs, #{path:=Path}) when is_list(Srvs); is_map(Srvs) ->
    Path1 = <<Path/binary, ".services">>,
    parse_services(Path1, nklib_util:to_list(Srvs), #{});

parse_domain_obj(nodesets, Nodes, #{path:=Path}) when is_list(Nodes); is_map(Nodes) ->
    Path1 = <<Path/binary, ".nodesets">>,
    parse_nodesets(Path1, nklib_util:to_list(Nodes), #{});

parse_domain_obj(roles, Roles, #{path:=Path, ok:=Ok}) 
        when is_list(Roles); is_map(Roles) ->
    case lists:keymember(roles, 1, Ok) of
        false ->
            Path1 = <<Path/binary, ".roles">>,
            parse_roles(Path1, nklib_util:to_list(Roles), #{});
        true ->
            error
    end;

parse_domain_obj(_, _, _) ->
    error.


%% @private
parse_domain_obj_role(Key, null, Opts) ->
    parse_domain_obj(Key, [], Opts);

parse_domain_obj_role(Key, <<>>, Opts) ->
    parse_domain_obj(Key, [], Opts);

parse_domain_obj_role(RoleAlias, Members, #{path:=Path, ok:=Ok} )
        when is_list(Members); is_map(Members) ->
    case lists:keymember(roles, 1, Ok) of
        false ->
            Role = case RoleAlias of
                members -> <<"member">>;
                users -> <<"user">>
            end,
            Path1 = <<Path/binary, ".roles">>,
            case parse_roles(Path1, [{Role, Members}], #{}) of
                {ok, Map} -> {ok, roles, Map};
                Other -> Other
            end;
        true ->
            error
    end;

parse_domain_obj_role(_, _, _) ->
    error.




%% @private
parse_groups(_Path, [], Acc) ->
    {ok, Acc};

parse_groups(Path, [{Name, null}|Rest], Acc) ->
    parse_groups(Path, [{Name, []}|Rest], Acc);

parse_groups(Path, [{Name, Data}|Rest], Acc) ->
    Name1 = nklib_util:to_binary(Name),
    Path1 = <<Path/binary, $., Name1/binary>>,
    case binary:match(Name1, [<<"@">>, <<".">>]) of
        nomatch ->
            Syntax = (base_syntax())#{
                members => fun parse_domain_obj_role/3,
                groups => fun parse_domain_obj/3
            },
            case do_parse(Data, Syntax, Path1, #{}) of
                {ok, Data1} ->
                    parse_groups(Path, Rest, maps:put(Name1, Data1, Acc));
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {invalid_name, Path1}}
    end;

parse_groups(_Path, _, _) ->
    error.


%% @private
parse_users(_Path, [], Acc) ->
    {ok, Acc};

parse_users(Path, [{User, Data}|Rest], Acc) ->
    Name1 = nklib_util:to_binary(User),
    Path1 = <<Path/binary, $., Name1/binary>>,
    case binary:match(Name1, <<"@">>) of
        nomatch ->
            Syntax = (base_syntax())#{
                alias => {ulist, binary},
                name => binary,
                surname => binary,
                password => fun parse_user_password/3
            },
            case do_parse(Data, Syntax, Path1, #{}) of
                {ok, Data1} ->
                    parse_users(Path, Rest, maps:put(Name1, Data1, Acc));
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {invalid_name, Path1}}
    end;

parse_users(_, _, _) ->
    error.


%% @private
parse_user_password(password, Pass, _) ->
    {ok, nkdomain_obj_user:user_pass(Pass)}.


%% @private
parse_nodesets(_Path, [], Acc) ->
    {ok, Acc};

parse_nodesets(Path, [{Name, Data}|Rest], Acc) ->
    Name1 = nklib_util:to_binary(Name),
    Path1 = <<Path/binary, $., Name1/binary>>,
    case binary:match(Name1, <<"@">>) of
        nomatch ->
            Syntax = (base_syntax())#{
                users => fun parse_domain_obj_role/3,
                meta => binary
            },
            case do_parse(Data, Syntax, Path1, #{}) of
                {ok, Data1} ->
                    parse_nodesets(Path, Rest, maps:put(Name1, Data1, Acc));
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {invalid_name, Path1}}
    end;

parse_nodesets(_, _, _) ->
    error.


%% @private
parse_services(_Path, [], Acc) ->
    {ok, Acc};

parse_services(Path, [{Name, Data}|Rest], Acc) ->
    Name1 = nklib_util:to_binary(Name),
    Path1 = <<Path/binary, $., Name1/binary>>,
    case binary:match(Name1, <<"@">>) of
        nomatch ->
            case do_parse(Data, #{class=>binary}, Path1, #{unknown_ok=>true}) of
                {ok, Data1} ->
                    Data2 = maps:merge(#{class=>Name1}, Data1),
                    RawClass = maps:get(class, Data2),
                    case get_service_syntax(RawClass) of
                        {ok, Class, ClassSyntax} ->
                            Syntax1 = (base_syntax())#{
                                class => binary,
                                users => fun parse_domain_obj_role/3
                            },
                            Syntax2 = maps:merge(ClassSyntax, Syntax1),
                            case do_parse(Data, Syntax2, Path1, #{}) of
                                {ok, Data3} ->
                                    Data4 = Data3#{class=>Class},
                                    Acc1 = maps:put(Name1, Data4, Acc),
                                    parse_services(Path, Rest, Acc1);
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {invalid_name, Path1}}
    end;

parse_services(_, _, _) ->
    error.


%% @private
parse_roles(_Path, [], Map) ->
    {ok, Map};

parse_roles(Path, [{Role, RoleList}|Rest], Map) ->
    Path1 = <<Path/binary, $., (nklib_util:to_binary(Role))/binary>>,
    case parse_role_list(Path1, RoleList, []) of
        {ok, RoleList1} ->
            Map1 = maps:put(to_binary(Role), RoleList1, Map),
            parse_roles(Path, Rest, Map1);
        {error, Error} ->
            {error, Error};
        error ->
            error
    end;

parse_roles(_, _, _) ->
    error.


%% @private
parse_role_list(_Path, <<>>, []) ->
    {ok, []};

parse_role_list(_Path, null, []) ->
    {ok, []};

parse_role_list(_Path, [], Acc) ->
    {ok, lists:usort(Acc)};

%% YAML version
parse_role_list(Path, [[{SubRole, SubObj}]|Rest], Acc) ->
    case to_role(SubObj) of
        {ok, SubObj2} ->
            Acc1 = [maps:put(to_binary(SubRole), SubObj2, #{})|Acc],
            parse_role_list(Path, Rest, Acc1);
        error ->
            Path1 = <<Path/binary, $., 
                      (nklib_util:to_binary(SubRole))/binary, $:,
                      (nklib_util:to_binary(SubObj))/binary>>,
            {error, {syntax_error, Path1}}
    end;

%% JSON version
parse_role_list(Path, [Map|Rest], Acc) when is_map(Map) ->
    case maps:to_list(Map) of
        [{SubRole, SubObj}] -> 
            parse_role_list(Path, [[{SubRole, SubObj}]|Rest], Acc);
        _ -> 
            {error, {synax_error, Path}}
    end;

parse_role_list(Path, [SubObj|Rest], Acc) when is_list(SubObj); is_binary(SubObj) ->
    case to_role(SubObj) of
        {ok, SubObj2} ->
            parse_role_list(Path, Rest, [SubObj2|Acc]);
        error ->
            Path1 = <<Path/binary, $., (nklib_util:to_binary(SubObj))/binary>>,
            {error, {syntax_error, Path1}}
    end;

parse_role_list(_, _, _) ->
    error.


%% @private
to_role(Term) ->
    case nkdomain_util:get_parts(Term) of
        {ok, {Class, ObjId}} -> 
            nkdomain_util:make_user_id({Class, ObjId});
        {error, _} ->
            error
    end.



% %% @private
% parse_alias(Alias) when is_binary(Alias) ->
%     parse_alias([Alias], []);

% parse_alias(Alias) when is_list(Alias), is_integer(hd(Alias)) ->
%     parse_alias([Alias], []);

% parse_alias(Alias) when is_list(Alias) ->
%     parse_alias(Alias, []);

% parse_alias(_) ->
%     error.


% %% @private
% parse_alias([], Acc) ->
%     {ok, lists:reverse(Acc)};

% parse_alias([Term|Rest], Acc) ->
%     Term1 = nklib_util:to_binary(Term),
%     case binary:split(Term1, <<"@">>, [global]) of
%         [Name, Domain] when Name /= <<>>, Domain /= <<>> ->
%             parse_alias(Rest, [Term1|Acc]);
%         _ ->
%             error
%     end.



%% @private
do_parse(Data, Syntax, Path, Opts) when is_list(Data); is_map(Data) ->
    % lager:warning("DP: ~p, ~p", [Data, Syntax]),
    ParseOpts = #{
        return => map,
        path => Path
    },
    UnknownOk = maps:get(unknown_ok, Opts, false),
    case nklib_config:parse_config(Data, Syntax, ParseOpts) of
        {ok, Parsed, Unknown} when UnknownOk orelse map_size(Unknown)==0->
            case Parsed of
                #{remove:=true} when map_size(Parsed)==1 ->
                    {ok, Parsed};
                #{remove:=true} ->
                    {error, {syntax_error, <<Path/binary, ".remove">>}};
                _ ->
                    {ok, Parsed}
            end;
        {ok, _, Unknown} ->
            First = nklib_util:to_binary(hd(maps:keys(Unknown))),
            {error, {syntax_error, <<Path/binary, $., First/binary>>}};
        {error, Error} ->
            {error, Error}
    end;

do_parse(<<>>, _Syntax, _, _) ->
    {ok, #{}};

do_parse(Data, _, Path, _) ->
    Path1 = <<Path/binary, $., (nklib_util:to_binary(Data))/binary>>,
    {error, {syntax_error, Path1}}.


%% @private
sort_domains(Map) ->
    L1 = lists:map(
        fun({Name, Data}) ->
            Name1 = nklib_util:to_binary(Name),
            Parts = case Name1 of
                <<"root">> ->
                    [];
                _ ->
                    lists:reverse(binary:split(Name1, <<".">>, [global]))
            end,
            {Parts, Name1, Data}
        end,
        maps:to_list(Map)),
    [{Name, Data} || {_, Name, Data} <- lists:sort(L1)].


%% @private
get_service_syntax(RawClass) ->
    try
        Class = case catch binary_to_existing_atom(RawClass, utf8) of
            {'EXIT', _} -> throw(class);
            Atom -> Atom
        end,
        Module = case catch nkdomain_service:get_module(Class) of
            {'EXIT', _} -> throw(class);
            Module0 -> Module0
        end,
        case catch Module:get_syntax() of
            Syntax when is_map(Syntax) -> 
                {ok, Class, Syntax};
            _ -> 
                throw(class)
        end
    catch
        throw:class -> {error, {invalid_domain_class, RawClass}}
    end.


%% @private
base_syntax() ->
    #{
        desc => binary,
        roles => fun parse_domain_obj/3,
        disabled => boolean,
        remove => boolean
    }.


% %% @private
% bjoin(Terms) ->
%     nklib_util:bjoin(Terms, <<".">>).
