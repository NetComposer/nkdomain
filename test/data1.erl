-module(data1).
-compile(export_all).

yaml() -> <<"
root:
    desc: Root Object
    status: ready
    roles:
        admin: 
            - admin@nekso.net
            - user:root2@root
            - member: group:admins.people@root
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
            password: NKD!!LXchlcfAoNecqJZzOSbsxPIgxzZ!         # 4321

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
            class: test_srv_admin
            disabled: true
        test_srv_dns:                                    
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
            class: test_srv_admin
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
            class: test_srv_dns
">>.


update1() -> 
<<"root:
    roles:
        admin: 
            - user:root2@root
        role1:
    groups:
        people:
            members:
                - member: group:admins.people@root
                - user:user1@proy1.domainA

            groups:
                admins:                      
                all:
                    members:
                        - user_root1@domain.com
                        - user_root2@domain.com
                        - shared@shared.com
                        - user:user1@proy1.domainA
    nodesets:
    services:
        admin:
            class: test_srv_admin
            disabled: true

">>.

