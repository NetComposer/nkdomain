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

%% @doc NkDomain Contact OpenAPI
-module(nkdomain_contact_actor_openapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkdomain_openapi).

-export([paths/1, schemas/1, parameters/1]).

-include("nkdomain.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


paths(SrvId) ->
    Spec = #{
        list => #{
            parameters => #{
                fieldSelector => <<"core.v1a1.ContactParamFieldSelector">>,
                sort => <<"core.v1a1.ContactParamSort">>
            }
        }
    },
    nkdomain_openapi_paths:paths(SrvId, "core", "v1a1", ?RES_CORE_CONTACTS, Spec).


schemas(SrvId) ->
    Spec = #{
        description => <<"An Actor to store personal contact information.">>,
        fields => #{
            spec => nkdomain_openapi_util:ref_schema("core.v1a1.ContactSpec")
        },
        required => [spec],
        schemas => #{
            'core.v1a1.ContactSpec' => #{
                description => <<"Specification for this actor">>,
                properties => #{
                    user => #{
                        description => <<"
                            If defined, must point to an User actor, a link of
                            type 'io.netc.core.contact-user' will be added
                        ">>,
                        type => string
                    },
                    name => #{
                        description => <<"Name of the person">>,
                        type => string
                    },
                    surname => #{
                        description => <<"Surname of the person">>,
                        type => string
                    },
                    normalizedName => #{
                        description => <<"
                            _Normalized_ (lowecase, no accents) version of the name.
                            Calculated by the server at each update.">>,
                        type => string
                    },
                    normalizedSurname => #{
                        description => <<"
                            _Normalized_ (lowecase, no accents) version of the surname.
                            Calculated by the server at each update.">>,
                        type => string
                    },
                    birthTime => #{
                        description => <<"Birth date.">>,
                        type => string,
                        format => date
                    },
                    gender => #{
                        description => <<"Gender of the person">>,
                        type => string,
                        enum => ['M', 'F']
                    },
                    timezone => #{
                        description => <<"Timezone in integer format">>,
                        type => integer
                    },
                    url => #{
                        description => <<"URLs associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactURL")
                    },
                    phone => #{
                        description => <<"Phones associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactPhone")
                    },
                    email => #{
                        description => <<"Email addresses associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactEmail")
                    },
                    im => #{
                        description => <<"Instant message addresses associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactIM")
                    },
                    address => #{
                        description => <<"Postal addresses associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactAddress")
                    },
                    pubkey => #{
                        description => <<"Public keys associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactPubkey")
                    },
                    profile => #{
                        description => <<"Profiles associated to this person">>,
                        type => array,
                        items => nkdomain_openapi_util:ref_schema("core.v1a1.ContactProfile")
                    }
                }
            },

            'core.v1a1.ContactURL' => #{
                description => <<"An user URL">>,
                type => object,
                properties => #{
                    url => #{
                        description => <<"The URL itself">>,
                        type => string
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                },
                required => [url]
            },

            'core.v1a1.ContactPhone' => #{
                description => <<"An user phone">>,
                type => object,
                properties => #{
                    phone => #{
                        description => <<"The phone itself">>,
                        type => string
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                },
                required => [phone]
            },

            'core.v1a1.ContactEmail' => #{
                description => <<"An email address">>,
                type => object,
                properties => #{
                    email => #{
                        description => <<"The email address itself">>,
                        type => string
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                },
                required => [email]
            },

            'core.v1a1.ContactIM' => #{
                description => <<"An instant message address">>,
                type => object,
                properties => #{
                    im => #{
                        description => <<"The address itself">>,
                        type => string
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                },
                required => [im]
            },

            'core.v1a1.ContactAddress' => #{
                description => <<"An postal address">>,
                type => object,
                properties => #{
                    street => #{
                        description => <<"Street name">>,
                        type => string
                    },
                    code => #{
                        description => <<"Postal code">>,
                        type => string
                    },
                    city => #{
                        description => <<"City name">>,
                        type => string
                    },
                    province => #{
                        description => <<"Province name">>,
                        type => string
                    },
                    state => #{
                        description => <<"State name">>,
                        type => string
                    },
                    country => #{
                        description => <<"Country name">>,
                        type => string
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                }
            },

            'core.v1a1.ContactPubkey' => #{
                description => <<"An public key">>,
                type => object,
                properties => #{
                    key => #{
                        description => <<"The key itself (base64)">>,
                        type => string,
                        format => base64
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                },
                required => [key]
            },

            'core.v1a1.ContactProfile' => #{
                description => <<"A profile entry">>,
                type => object,
                properties => #{
                    startTime => #{
                        description => <<"The start timestamp for the entry">>,
                        type => string,
                        format => date
                    },
                    stopTime => #{
                        description => <<"The stop timestamp for the entry">>,
                        type => string,
                        format => date
                    },
                    type => #{
                        description => <<"A type for this entry">>,
                        type => string
                    },
                    data => #{
                        description => <<"Curriculm data">>,
                        type => object,
                        additionalProperties => true
                    },
                    meta => #{
                        description => <<"Additional metadata">>,
                        type => object,
                        additionalProperties => true
                    }
                },
                required => [data]
            }
        }
    },
    nkdomain_openapi_schemas:actor_schema(SrvId, "core", "v1a1", ?RES_CORE_CONTACTS, Spec).


parameters(SrvId) ->
    {Filter, Sort} = nkdomain_openapi_util:filter_parameters(SrvId, "core", "v1a1", ?RES_CORE_CONTACTS),
    #{
        'core.v1a1.ContactParamFieldSelector' =>
            nkdomain_openapi_util:filter_parameter(Filter),

        'core.v1a1.ContactParamSort' =>
            nkdomain_openapi_util:sort_parameter(Sort)
    }.