
-module(data2).
-compile(export_all).

yaml() -> 
<<"
root:
    roles:
        admin:
            - orgA

    members:
        - member: orgA
        - member: domain:orgB

orgA:
    users:
        u01:
        u02:
        u03:
        u04:
        u05:
        u06:
        u07:
        u08:
        u10:
        u11:
        u12:
    members:
        - member: domain:dep1.orgA
        - member: dep2.orgA
        - member: dep3.orgA

dep1.orgA:
    members:
        - user:u01@orgA
        - user:u02@orgA

dep2.orgA:
    members:
        - group:group1@dep2.orgA
        - group:group2@dep2.orgA
        - member: group:group1@dep2.orgA
        - member: group:group2@dep2.orgA

    groups:
        group1:
            members:
                - user:u03@orgA
                - user:u04@orgA
        group2:
            members:
                - user:u05@orgA
                - user:u06@orgA

dep3.orgA:
    members:
        - user:u07@orgA
        - user:u08@orgA
        - u09@orgA              # Does not exists, must be an alias

orgB:
    members:
        - user:u10@orgA
        - member: dep1.orgB

dep1.orgB:
    members:
        - user:u11@orgA
        - user:u12@orgA


">>.

