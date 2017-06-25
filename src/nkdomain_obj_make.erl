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

-export([make/2, make_obj_id/1, make_name/1, create/2, create/3]).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-type make_opts() ::
    #{
        type => nkdomain:type(),                        % Mandatory
        domain_id => binary(),                          % Mandatory
        created_by => binary(),                         % Mandatory
        name => binary(),
        expires_time => nklib_util:m_timestamp(),
        parent_id => nkdomain:obj_id(),
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


%% @doc
-spec make(nkservice:id(), make_opts()) ->
    {ok, nkdomain:obj()} | {error, term()}.

make(SrvId, Opts) ->
    #{
        type := Type,
        domain_id := Domain,
        created_by := User
    } = Opts,
    Parent = maps:get(parent_id, Opts, Domain),
    try
        {DomainId, DomainPath} = case nkdomain_lib:find(SrvId, Domain) of
            #obj_id_ext{obj_id=DomainId0, path=DomainPath0} ->
                {DomainId0, DomainPath0};
            {error, object_not_found} ->
                throw({could_not_load_domain, Domain});
            {error, DomainError} ->
                throw(DomainError)
        end,
        ParentId = case nkdomain_lib:find(SrvId, Parent) of
            #obj_id_ext{obj_id=ParentId0} ->
                ParentId0;
            {error, object_not_found} ->
                throw({could_not_load_parent, Parent});
            {error, ParentError} ->
                throw(ParentError)
        end,
        UserId = case nkdomain_lib:find(SrvId, User) of
            #obj_id_ext{obj_id=UserId0} ->
                UserId0;
            {error, object_not_found} ->
                throw({could_not_load_user, User});
            {error, UserError} ->
                throw(UserError)
        end,
        Type2 = to_bin(Type),
        ObjId1 = make_obj_id(Type2),
        ObjId2 = case Opts of
            #{obj_id:=ObjId0} -> ObjId0;
            _ -> ObjId1
        end,
        Name1 = case maps:get(obj_name, Opts, <<>>) of
            <<>> -> make_name(ObjId1);
            Name0 -> nkdomain_util:name(Name0)
        end,
        Name2 = case Type2 of
            ?DOMAIN_DOMAIN ->
                Name1;
            _ ->
                <<(nkdomain_util:class(Type2))/binary, "/", Name1/binary>>
        end,
        BasePath = case DomainPath of
            <<"/">> -> <<>>;
            _ -> DomainPath
        end,
        Now = nkdomain_util:timestamp(),
        Obj1 = maps:without([obj_name, ttl], Opts),
        Obj2 = maps:merge(#{Type2=>#{}}, Obj1),
        Obj3 = Obj2#{
            obj_id => ObjId2,
            type => Type2,
            domain_id => DomainId,
            path => <<BasePath/binary, $/, Name2/binary>>,
            created_time => Now,
            created_by => UserId,
            parent_id => ParentId,
            updated_time => Now,
            updated_by => UserId
        },
        Obj4 = case Opts of
            #{ttl:=SecsTTL} ->
                Expires = nkdomain_util:timestamp() + 1000*SecsTTL,
                Obj3#{expires_time=>Expires};
            _ ->
                Obj3
        end,
        {ok, Obj4}
    catch
        throw:Throw ->
            {error, Throw}
    end.


%% @private
make_obj_id(Type) ->
    UUID = nklib_util:luid(),
    case Type of
        ?DOMAIN_TOKEN -> UUID;
        _ -> <<(to_bin(Type))/binary, $-, UUID/binary>>
    end.


%% @private
make_name(ObjId) ->
    UUID = case binary:split(ObjId, <<"-">>) of
        [_, Rest] when byte_size(Rest) >= 7 -> Rest;
        [Rest] when byte_size(Rest) >= 7 -> Rest;
        _ -> nklib_util:luid()
    end,
    binary:part(UUID, 0, 7).


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