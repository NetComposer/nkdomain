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

%% @doc NkDomain service callback module
-module(nkdomain_nkroot_svc).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/0, stop/0, load_objs/0]).


-define(LLOG(Type, Txt, Args), lager:Type("NkDOMAIN ROOT SVC: "++Txt, Args)).

-include("nkdomain.hrl").

%%-include_lib("nkapi/include/nkapi.hrl").
%%-include_lib("nkevent/include/nkevent.hrl").
%%-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================




%% @doc
start() ->
    Spec = #{
        plugins => [
            nkdomain_nkroot, nkdomain_store_es, nkadmin,
            nkapi, nkmail, nkmail_smtp_client, nkfile_filesystem, nkfile_s3, nkservice_rest
        ],
        nkdomain => nklib_config:get_env(nkdomain),
        debug => [
            %% {nkapi_client, #{nkpacket=>true}},
            %% nkapi_server,
            %% nkelastic
            %% {nkelastic, [full]},
            %% {nkdomain_obj, all}
            {nkdomain_obj, #{types=>[<<"med.encounter">>]}}
        ]
    },
    nkservice:start(?NKROOT, Spec).


%% @doc
stop() ->
    nkservice:stop(?NKROOT).


%% @doc
load_objs() ->
    nkdomain_node:make_objs(objs()).



objs() ->
    [
        #{
            path => "/file.stores/local",
            ?DOMAIN_FILE_STORE => #{
                class => <<"filesystem">>,
                config => #{
                    path => <<"files.local">>
                }
            }
        },
        #{
            path => "/file.stores/local_secure",
            ?DOMAIN_FILE_STORE => #{
                class => <<"filesystem">>,
                config => #{
                    path => <<"files.local">>
                },
                encryption => aes_cfb128
            }
        },
        #{
            path => "/file.stores/carlos.s3",
            ?DOMAIN_FILE_STORE => #{
                class => <<"s3">>,
                config => #{
                    bucket => <<"nkobjects">>,
                    aws_id => <<"AKIAITCSLY34RVKXHP4Q">>,
                    aws_secret => <<"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">>,
                    bucket_access_method => <<"auto">>,
                    bucket_after_host => false,
                    port => 80,
                    scheme => <<"https://">>
                }
            }
        },
        #{
            path => "/file.stores/carlos.s3_secure",
            ?DOMAIN_FILE_STORE => #{
                class => <<"s3">>,
                config => #{
                    bucket => <<"nkobjects">>,
                    aws_id => <<"AKIAITCSLY34RVKXHP4Q">>,
                    aws_secret => <<"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">>,
                    bucket_access_method => <<"auto">>,
                    bucket_after_host => false,
                    port => 80,
                    scheme => <<"https://">>
                },
                encryption => aes_cfb128
            }
        },
        #{
            path => "/mail.providers/direct",
            ?DOMAIN_MAIL_PROVIDER => #{
                class => smtp,
                config => #{
                    relay => <<"gmail.com">>
                },
                from =>  <<"NetComposer <carlos@netcomposer.io>">>
            }
        },
        #{
            path => "/mail.providers/direct",
            ?DOMAIN_MAIL_PROVIDER => #{
                class => smtp,
                config => #{
                    force_tls => true,
                    password => <<"carlos12">>,
                    relay => <<"smtp.gmail.com">>,
                    username => <<"info.netcomposer@gmail.com">>
                },
                from =>  <<"NetComposer <carlos@netcomposer.io>">>
            }
        }
    ].


