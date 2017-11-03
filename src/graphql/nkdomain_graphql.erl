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

%% @doc NkDomain GraphQL main module

%% When a query is received, it first go to nkdomain_graphql_object_query
%% If it is an abstract type (interface or union) it will go to find the type,
%% and the to the type manager object (nkdomain_graphql_object manages all of them










-module(nkdomain_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([load_schema/0]).



mapping_rules() ->
    #{
        scalars => #{default => nkdomain_graphql_scalar},
        enums => #{default => graphql_enum_coerce},
        interfaces => #{default => nkdomain_graphql_type},
        unions => #{default => nkdomain_graphql_type},
        objects => #{
            'Query' => nkdomain_graphql_query,
            'Mutation' => nkdomain_graphql_mutation,
            default => nkdomain_graphql_object
        }
    }.


load_schema() ->
    ok = graphql_schema:reset(),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, schema()),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.


%% tag::setupRoot[]
setup_root() ->
    Root = {
        root,
            #{
                query => 'Query',
                mutation => 'Mutation',
                interfaces => ['Node']
            }
    },
    ok = graphql:insert_schema_definition(Root),
    ok.




schema() -> <<"
+descripton(text: \"Standard miliseconds unix time\")
scalar UnixTime

enum Mood {
     TRANQUIL
     DODGY
     AGGRESSIVE
}

type Query {
  +description(text: \"Relay Modern specification Node fetcher\")
  node(id : ID!) : Node
  +description(text: \"Get all users\")
  allUsers : [User]
}



+description(text: \"Relay Modern Node Interface\")
interface Node {
  +description(text: \"Unique Identity of a Node\")
  id : ID!
}


interface Object {",
(object())/binary,
"}


type PageInfo {
  hasNextPage : Boolean!
  hasPreviousPage : Boolean!
}

+description(text: \"Representation of Domains\")
type Domain implements Node, Object {",
(object())/binary, "
}


+description(text: \"Representation of Users\")
type User implements Node, Object {",
(object())/binary, "
  userName : String,
  userSurname : String
  email : String
  #password : String
  phone : String,
  address : String
  statusConnection(         # connections to 'UserStatus' objects
    after : String
    first : Int
    before : String
    last : Int) : UserStatusConnection
}


type UserStatusConnection {                 # This is an 'abstract' concept but UserStatusEdge is not
    pageInfo : PageInfo!                    # it is the 'line' connecting Users and UserStatus objects
    edges : [UserStatusEdge]                # an 'edge' is the 'relation' between among two objects
    totalCount : Int                        # in this case, between 'User' and 'UserStatus' objects
}

type UserStatusEdge {                       # The real relation between the User and the UserStatus
    node : UserStatus                       # it could have more metadata, but it is not common
    cursor : String!
}

type UserStatus {
    domainPath : String!
    userStatus : String
    updatedTime : UnixTime
}


type File implements Node, Object {",
(object())/binary, "
  contentType : String
  size : Int
}


## -- MUTATION OBJECTS ----------

type Mutation {
    introduceUser(input: IntroduceUserInput!) : IntroduceUserPayload
}


input IntroduceUserInput {
    clientMutationId : String
    name : String!
}


type IntroduceUserPayload {
    clientMutationId : String
}

">>.



object() -> <<"
    id : ID!
    vsn : String
    objId : String!
    type : String!
    path : String!
    objName : String!
    domainId : String!
    domain : Domain!
    parentId : String!
    parent : Object!
    srvId : String
    subtype : [String]
    createdById : String!
    createdBy : User!
    createdTime : UnixTime!
    updatedBy : User!
    updatedTime : UnixTime!
    enabled : Boolean!
    active : boolean!
    expiresTime : UnixTime
    destroyed : Boolean
    destroyedTime : UnixTime,
    destroyedCode : String
    destroyedReason : String
    name : String
    description : String
    tags : [String]
    aliases : [String]
    iconId : File
    nextStatusTime : UnixTime
">>.