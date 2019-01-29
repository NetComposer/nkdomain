%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkDomain Contact Actor Graphql
-module(nkdomain_contact_actor_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([config/0, schema/1, connections/1, query/5, execute/5]).
-import(nkdomain_graphql_execute, [get_value/2, get_value/3, get_map/2, get_time/3, get_object/2]).

-behavior(nkservice_graphql_schema).

-include("nkdomain.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================

config() ->
    #{
        type => <<"Contact">>,
        actor_group => ?GROUP_CORE,
        actor_resource => ?RES_CORE_CONTACTS
    }.


%%  @doc Generates new schema entries
schema(types) ->
    Time = #{params => #{format => timeFormat}},
    #{
        'ContactUrl' => #{
            fields => #{
                type => string,
                url => {no_null, string},
                meta => {list, 'Map'}
            }
        },
        'ContactPhone' => #{
            fields => #{
                type => string,
                phone => {no_null, string},
                meta => {list, 'Map'}
            }
        },
        'ContactEmail' => #{
            fields => #{
                type => string,
                email => {no_null, string},
                meta => {list, 'Map'}
            }
        },
        'ContactIM' => #{
            fields => #{
                type => string,
                im => {no_null, string},
                meta => {list, 'Map'}
            }
        },
        'ContactAddress' => #{
            fields => #{
                type => string,
                street => string,
                code => string,
                city => string,
                province => string,
                state => string,
                country => string,
                meta => {list, 'Map'}
            }
        },
        'ContactPubkey' => #{
            fields => #{
                type => string,
                key => {no_null, string},
                meta => {list, 'Map'}
            }
        },
        'ContactProfile' => #{
            fields => #{
                type => string,
                startTime => {string, Time},
                stopTime => {string, Time},
                data => {list, 'Map'},
                meta => {list, 'Map'}
            }
        },
        'ContactPhoto' => #{
            fields => #{
                type => string,
                file => string,
                meta => {list, 'Map'}
            }
        },
        'ContactSpec' => #{
            fields => #{
                user => string,
                name => string,
                surname => string,
                normalizedName => string,
                normalizedSurname => string,
                birthTime => {string, Time},
                gender => gender,
                timezone => integer,
                url => {list, 'ContactUrl'},
                phone => {list, 'ContactPhone'},
                email => {list, 'ContactEmail'},
                im => {list, 'ContactIM'},
                address => {list, 'ContactAddress'},
                pubkey => {list, 'ContactPubkey'},
                profile => {list, 'ContactProfile'},
                photo => {list, 'ContactPhoto'}
            }
        },
        'Contact' => #{
            class => actor,
            fields => nkdomain_graphql_schema:actor_type_fields(#{
                spec => 'ContactSpec',
                status => 'ActorStatus'
            }),
            comment => "A Contact",
            filter_fields => nkdomain_graphql_schema:actor_filter_fields(#{spec=>'ContactSpecFilterFields'}),
            sort_fields => nkdomain_graphql_schema:actor_sort_fields(#{spec=>'ContactSpecSortFields'})
        }
    };

schema(inputs) ->
    #{
        'ContactSpecSortFields' => #{
            fields => #{
                normalizedName => 'SortSpec',
                normalizedSurname => 'SortSpec',
                birthTime => 'SortSpec',
                gender => 'SortSpec',
                timezone => 'SortSpec'
            }
        },
        'ContactSpecFilterFields' => #{
            fields => #{
                normalizedName => {'KeywordFilter', #{comment => "Contact name"}},
                normalizedSurname => {'KeywordFilter', #{comment => "Contact surname"}},
                birthTime => 'KeywordFilter',
                gender => 'KeywordFilter',
                timezone => 'IntegerFilter'
            }
        }
    };

schema(queries) ->
    #{
        allContacts =>nkdomain_graphql_schema:actor_query(<<"Contact">>, #{})
    };


schema(_) ->
    #{}.


%% @private
connections(<<"User">>) ->
    #{
        contactsConnection => nkdomain_graphql_schema:actor_connection(<<"Contact">>, #{
            comment => "Contacts linked to this User"
        })
    };

connections(<<"Contact">>) ->
    #{
        userConnection => 'User'
    };

connections(_) ->
    #{}.


%% @doc
query(SrvId, <<"allContacts">>, Params, _Meta, _Ctx) ->
    Opts = #{apiGroup => ?GROUP_CORE, kind => <<"Contact">>},
    nkdomain_graphql_search:search(SrvId, Params, Opts).


%% @private
execute(SrvId, Field, {nkdomain, {spec, _Type, Spec, _Actor}}, _Meta, _Params) ->
    case Field of
        <<"gender">> ->
            Gender = case maps:get(<<"gender">>, Spec, null) of
                <<"M">> -> {enum, <<"MALE">>};
                <<"F">> -> {enum, <<"FEMALE">>};
                null -> null
            end,
            {ok, Gender};
        _ when Field==<<"url">>;  Field==<<"phone">>;  Field==<<"email">>;
               Field==<<"im">>; Field==<<"address">>;  Field==<<"pubkey">>;
               Field==<<"profile">>; Field==<<"photo">> ->
            List = maps:get(Field, Spec, []),
            get_object(SrvId, {field, <<"Contact">>, Field, List});
        _ ->
            get_value(Field, Spec)
    end;

execute(_SrvId, Field, {nkdomain, {field, <<"Contact">>, Type, Data}}, _Meta, Params)
        when Type==<<"url">>; Type==<<"phone">>; Type==<<"email">>; Type==<<"im">>;
             Type==<<"address">>; Type==<<"pubkey">>; Type==<<"profile">>;
             Type==<<"photo">> ->
        if
        Field==<<"data">>; Field==<<"meta">> ->
            get_map(Field, Data);
        Field==<<"startTime">>; Field==<<"stopTime">> ->
            get_time(Field, Data, Params);
        true ->
            get_value(Field, Data)
    end;

%%execute(_SrvId, Field, {nkdomain, {field, <<"Contact">>, Type, Data}}, _Meta, _Params) ->
%%    {ok, null};

execute(SrvId, <<"contactsConnection">>, {nkdomain, {actor, _Type, Actor}}, _Meta, Params) ->
    #{<<"metadata">>:=#{<<"uid">>:=UID}} = Actor,
    LinkType = nkdomain_actor_util:link_type(?GROUP_CORE, ?LINK_CORE_CONTACT_USER),
    Opts = #{
        apiGroup => ?GROUP_CORE,
        kind => <<"Contact">>,
        search_spec => #{
            deep => true,
            filter => #{
                'and' => [
                    #{field=><<"metadata.links.", UID/binary>>, op=>eq, value=>LinkType}
                ]
            }
        }
    },
    nkdomain_graphql_search:search(SrvId, Params, Opts);

execute(SrvId, <<"userConnection">>, {nkdomain, {actor, _Type, Actor}}, _Meta, _Params) ->
    LinkType = nkdomain_actor_util:link_type(?GROUP_CORE, ?LINK_CORE_CONTACT_USER),
    case nkservice_actor_util:get_linked_uids(LinkType, Actor) of
        [UserUID] ->
            nkdomain_graphql_search:get_uid(SrvId, UserUID);
        _ ->
            {ok, null}
    end;

execute(_SrvId, _Field, _Obj, _Meta, _Params) ->
    continue.
