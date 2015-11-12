# NkDOMAIN

NkDOMAIN is an Erlang framework to load an manage complex distributed, multi-tenant configurations in a [riak_core](https://github.com/basho/riak_core) cluster.

NkDOMAIN can read _domain_ configurations from YAML files, JSON files or Erlang maps, it checks its syntaxis and creates correspondig Erlang processes distributed in the cluster, saving the objects in a [NkBASE](https://github.com/Nekso/nkbase) database, for example:

```yaml
# Load of the 'root' domain
root:
    desc: Root Object
    status: ready
    roles:
        admin: 
            - admin@nekso.net
            - user:root2@root
            - member: group:admins.people@root      # 'Members' of admins.people@root are 'admins' of root
        role1:
            - user:root1@root
    users:
        admin:                       
            alias: admin@nekso.net            
            name: Global
            surname: Admin
            password: 1234
            roles:
        root1:                            
            alias: [user_root1@domain.com, shared@shared.com]
            roles:
                admin:
            password: NKD!!LXchlcfAoNecqJZzOSbsxPIgxzZ!         # Hash of 4321

        root2:
            alias: [user_root2@domain.com, shared@shared.com]
            roles:
                admin: 
                    - user:root1@root
    nodesets:
        group1:                           
            meta: core;id=group1
            users:
                - domainA    
        group2:
            meta: core;id=group2
            roles:
                user:
                    - member: group:admins.people@root
    services:
        admin:
            disabled: true
        dns:                                    
            users:
                - member: group:people@root
    groups:
        people:
            members:
                - member: group:admins.people@root
                - member: group:all.people@root

            groups:
                admins:                      
                    roles:
                        member:
                            - user:admin@root
                            - user:admin@domainA
                all:
                    members:
                            - user:admin@root
        nodes:
            groups:
                all:                          
                    members:
                        - nodeset:group1@root
                        - nodeset:group2@root
                        - nodeset:group1@domainA
        zones:
            groups:
                a:
                    groups:
                        a1:
                        a2:
                b:
                    groups:
                        b1:
                        b2:

domainA:
    desc: Domain A
    alias: domain_a.com
    roles:
        admin: 
            - user:admin@domainA
            - admin: root
            - member: group:admins.people@root
    status: ready
    users:
        user1:
        admin:
            alias: admin@domain_a.com            
    nodesets:
        group1:              
            meta: domainA
    services:
        admin:
            disabled: false
            users:
                - member: group:all.people@root


proy1.domainA:
    status: ready
    groups:
        a:
        b:
    alias: nekso.net
    users:
        user1:
        user2:
    services:
        dns:
```

By default, NkDOMAIN recognizes:

* Domains and subdomains (proyects) to any level. Inside domains, the following elements are recognized:
  * Groups and subgroups
  * Users
  * Services
  * Nodesets
* Aliases
* Tokens

Some other features are:
* Very high perfomance, it can scale to millions of objects.
* Sophisticated role management, based on [NkROLE](https://github.com/Nekso/nkrole).
* Objects are distributed in the cluster using [NkDIST](https://github.com/Nekso/nkdist). They can be permanent (like domains) or temporary (like users). Temporary objects are automatically reloaded if necessary.
* Domains can be re-loaded at any time, only modified items will be processed.
* Any element can be removed, and the depending elements (subgroups, etc.) will also be deleted.
* Domains and users can have any number of aliases.
* Full authentication token management, for users or any other object.
* Services are started at all nodes of the cluster automatically.

NkDOMAIN is not yet ready for normal use, but most of it is complete. See the included tests for examples of use.

## Internal

NkDOMAIN maintains a tree of Erlang objects (`nkdomain:obj()`), each of one belongs to a class (`nkdomain:class()`). Objects are defined by their class and object's id (`nkdomain:obj_id()`) or by a full id (`nkdomain:obj_usr_id()`) that usually includes the class, like <<"user:name@domain">> (but can be omitted for domain objects) All objects must be created inside other object, except the object <<"domain:root">>. 

Each class of objects have their own rules and accept the creation of other specific types of object.

Objects must implement the `nkdomain_obj` behaviour.

Each time you access an object, it is located in disk and loaded into an Erlang process distributed in the cluster unsing NkDIST. It then stay in memory permanently or it is removed after a timeout, depending on each specific class. 



### Loading and Removing

Any object and be created or updated loading and specific syntax with nkdomain:load/4. Each object class must implement a load/4 callback function and be able to update itself. The update object will be updated on disk.

Objects can be removed loading a specification with the key `remove=true`.

Usually, you will only load `domain` objects, calling nkdomain:load_file/1. The domain object will load itself all dependant objects. You can also export a domain, along all of it dependant object, calling nkdomain:export/1.



### Aliases

You can define any number of aliases, that can point to one or several objects. You can use nkdomain:resolve/1 or nkdomain:multi_resolve/1 to find all pointed objects from an alias or user id.


### Services

Objects belonging to class `service` will not only be stored on database as any other object, but also 

Services must register a callback module for any new service, calling nkdomain_service_mngr:register/2. The callback module must implement the `nkdomain_service` behaviour, where they an offer an specific syntax, and are called when 





