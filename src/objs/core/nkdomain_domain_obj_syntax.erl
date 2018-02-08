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

%% @doc User Object Syntax

-module(nkdomain_domain_obj_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([syntax/2]).

-include("nkdomain.hrl").

%% ===================================================================
%% Syntax
%% ===================================================================

%% Sample (see nkelastic_search.erl)
%% #{
%%      from => 1,
%%      size => 10,
%%      sort => ["field1", "desc:field2"],
%%      fields => ["field1", "field2"],
%%      filters => #{
%%          field1 => ">text",
%%          field2 => "!text"
%%      },
%%      simple_query => "message"
%% }

syntax(<<"check_name">>, Syntax) ->
    Syntax#{
        name => binary,
        '__mandatory' => [name]
    };

syntax(<<"find">>, Syntax) ->
    Syntax2 = Syntax#{
        id => binary
    },
    nkdomain_obj_util:search_syntax(Syntax2);

syntax(<<"find_all">>, Syntax) ->
    syntax(<<"find">>, Syntax);

%%syntax(<<"find_types">>, Syntax) ->
%%    syntax(<<"find">>, Syntax);
%%
%%syntax(<<"find_all_types">>, Syntax) ->
%%    syntax(<<"find">>, Syntax);
%%
%%syntax(<<"find_childs">>, Syntax) ->
%%    syntax(<<"find">>, Syntax);
%%
%%syntax(<<"find_all_childs">>, Syntax) ->
%%    syntax(<<"find">>, Syntax);

syntax(<<"unload_childs">>, Syntax) ->
    Syntax#{
        id => binary
    };


syntax(<<"delete_childs_of_type">>, Syntax) ->
    Syntax#{
        id => binary,
	type => binary
    };

syntax(<<"create_child">>, Syntax) ->
    Syntax#{
        path => binary,
	data => map
    };

syntax(Cmd, Syntax) ->
    nkdomain_obj_syntax:syntax(Cmd, ?DOMAIN_DOMAIN, Syntax).


%% ===================================================================
%% Search syntax
%% ===================================================================



