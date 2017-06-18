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


%% @doc Basic Obj utilities


-module(nkdomain_obj_make).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([make/2, create/2, create/3]).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-type make_opts() ::
    #{
        type => nkdomain:type(),                        % Mandatory
        parent_id => binary(),                          % Mandatory
        created_by => binary(),                         % Mandatory
        name => binary(),
        expires_time => nklib_util:m_timestamp(),
        referred_id => nkdomain:obj_id(),
        active => boolean(),
        description => binary(),
        aliases => [binary()],
        nkdomain:type() => map(),

        % Pseudo-field:
        obj_id => binary(),
        obj_name => binary(),
        ttl => integer()                                % secs
}.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Adds type, obj_id, parent_id, path, created_time
-spec make(nkservice:id(), make_opts()) ->
    {ok, nkdomain:obj()} | {error, term()}.

make(SrvId, Opts) ->
    #{
        type := Type,
        parent_id := Domain,
        created_by := User
    } = Opts,
    case nkdomain_lib:find(SrvId, Domain) of
        #obj_id_ext{obj_id=DomainId, path=DomainPath} ->
            case nkdomain_lib:find(SrvId, User) of
                #obj_id_ext{obj_id=UserId} ->
                    Type2 = to_bin(Type),
                    UUID = nklib_util:luid(),
                    ObjId = case Opts of
                        #{obj_id:=ObjId0} ->
                            to_bin(ObjId0);
                        _ when Type2 == ?DOMAIN_TOKEN ->
                            UUID;
                        _ ->
                            <<Type2/binary, $-, UUID/binary>>
                    end,
                    case do_make_name(UUID, maps:get(obj_name, Opts, <<>>)) of
                        {ok, Name1} ->
                            Name2 = case Type2 of
                                ?DOMAIN_DOMAIN ->
                                    Name1;
                                _ ->
                                    <<Type2/binary, "s/", Name1/binary>>
                            end,
                            BasePath = case DomainPath of
                                <<"/">> -> <<>>;
                                _ -> DomainPath
                            end,
                            Now = nkdomain_util:timestamp(),
                            Obj1 = maps:without([obj_name, ttl], Opts),
                            Obj2 = Obj1#{
                                obj_id => ObjId,
                                type => Type2,
                                parent_id => DomainId,
                                path => <<BasePath/binary, $/, Name2/binary>>,
                                created_time => Now,
                                created_by => UserId,
                                updated_time => Now,
                                updated_by => UserId
                            },
                            Obj3 = case Opts of
                                #{ttl:=SecsTTL} ->
                                    Expires = nkdomain_util:timestamp() + 1000*SecsTTL,
                                    Obj2#{expires_time=>Expires};
                                _ ->
                                    Obj2
                            end,
                            case Opts of
                                #{referred_id:=ReferId} ->
                                    case nkdomain_lib:find(SrvId, ReferId) of
                                        #obj_id_ext{obj_id=ReferObjId} ->
                                            {ok, Obj3#{referred_id=>ReferObjId}};
                                        {error, object_not_found} ->
                                            {error, referred_not_found};
                                        {error, Error} ->
                                            {error, Error}
                                    end;
                                _ ->
                                    {ok, Obj3}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, object_not_found} ->
                    {error, {could_not_load_user, User}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, object_not_found} ->
            {error, {could_not_load_parent, Domain}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_make_name(UUID, <<>>) ->
    {ok, binary:part(UUID, 0, 7)};

do_make_name(_UUID, Name) ->
    {ok, nkdomain_util:name(Name)}.


%% @doc
-spec create(nkservice:id(), make_opts()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

create(SrvId, MakeOpts) ->
    create(SrvId, MakeOpts, #{}).


%% @doc
-spec create(nkservice:id(), make_opts(), nkdomain:start_pts()) ->
    {ok, #obj_id_ext{}, [Unknown::binary()]} | {error, term()}.

create(SrvId, MakeOpts, Opts) ->
    case make(SrvId, MakeOpts) of
        {ok, Obj2} ->
            case SrvId:object_parse(SrvId, load, Obj2) of
                {ok, Obj3, Unknown} ->
                    case nkdomain_lib:create(SrvId, Obj3, Opts) of
                        #obj_id_ext{}=ObjIdExt ->
                            {ok, ObjIdExt, Unknown};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
to_bin(T) -> nklib_util:to_binary(T).