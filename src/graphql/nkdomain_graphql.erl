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
-module(nkdomain_graphql).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([load_schema/0]).



mapping_rules() ->
    #{
        scalars => #{ default => nkdomain_graphql_scalar },
        interfaces => #{ default => nkdomain_graphql_type },
        unions => #{ default => nkdomain_graphql_type },
        objects => #{
            'User' => nkdomain_graphql_user,
            'Query' => nkdomain_graphql_query,
            'Mutation' => nkdomain_graphql_mutation,
            default => nkdomain_graphql_object }
    }.


load_schema() ->
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, schema()),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.


%% tag::setupRoot[]
setup_root() ->
    Root = {root,
            #{ query => 'Query',
               mutation => 'Mutation',
               interfaces => ['Node']
            }},
    ok = graphql:insert_schema_definition(Root),
    ok.




schema() -> <<"
scalar UnixTime


type Query {
  +description(text: \"Relay Modern specification Node fetcher\")
  node(id : ID!) : Node
  +description(text: \"Fetch a starship with a given Id\")
  allUsers : [User]
}



+description(text: \"Relay Modern Node Interface\")
interface Node {
  +description(text: \"Unique Identity of a Node\")
  id : ID!
}


interface Object {
  id : ID!
  vsn : String
  type : String!
  path : String!
  objName : String!
  domain_id : String!
  parent_id : String!
  srv_id : String
  subtype : [String]
  created_by : User
  created_time : UnixTime
  updated_by : User
  updated_time : UnixTime
  enabled : Boolean!
  active : boolean!
  expires_time : UnixTime
  destroyed : Boolean
  destroyed_time : UnixTime,
  destroyed_code : String
  destroyed_reason : String
  name : String
  description : String
  tags : [String]
  aliases : [String]
  icon_id : File
  next_status_time : UnixTime
}


type PageInfo {
  hasNextPage : Boolean!
  hasPreviousPage : Boolean!
}

+description(text: \"Representation of Users\")
type User implements Node, Object {
  id : ID!
  vsn : String
  type : String!
  path : String!
  objName : String!
  domain_id : String!
  parent_id : String!
  srv_id : String
  subtype : [String]
  created_by : User
  created_time : UnixTime
  updated_by : User
  updated_time : UnixTime
  enabled : Boolean!
  active : boolean!
  expires_time : UnixTime
  destroyed : Boolean
  destroyed_time : UnixTime
  destroyed_code : String
  destroyed_reason : String
  name : String
  description : String
  tags : [String]
  aliases : [String]
  icon_id : File
  next_status_time : UnixTime

  userName : String,
  userSurname : String
  email : String
  password : String
  phone : String,
  address : String
  statusConnection(
    after : String
    first : Int
    before : String
    last : Int) : UserStatusConnection      # VehiclePilotsConnection
}


type UserStatusConnection {
    pageInfo : PageInfo!
    edges : [UserStatusEdge]
    totalCount : Int
}

type UserStatusEdge {
    node : Status
    cursor : String!
}

type Status {
    domainPath : String!
    userStatus : String
    updatedTime : UnixTime
}


type File implements Node, Object {
  id : ID!
  vsn : String
  type : String!
  path : String!
  objName : String!
  domain_id : String!
  parent_id : String!
  srv_id : String
  subtype : [String]
  created_by : User
  created_time : UnixTime
  updated_by : User
  updated_time : UnixTime
  enabled : Boolean!
  active : boolean!
  expires_time : UnixTime
  destroyed : Boolean
  destroyed_time : UnixTime,
  destroyed_code : String
  destroyed_reason : String
  name : String
  description : String
  tags : [String]
  aliases : [String]
  icon_id : File
  next_status_time : UnixTime

  contentType : String
  size : Int
}


## -- MUTATION OBJECTS ----------

type Mutation {
    introduceFaction(input: IntroduceUserInput!) : IntroduceUserPayload
}


input IntroduceUserInput {
    clientMutationId : String
    name : String!
}


type IntroduceUserPayload {
    clientMutationId : String
}

">>.
