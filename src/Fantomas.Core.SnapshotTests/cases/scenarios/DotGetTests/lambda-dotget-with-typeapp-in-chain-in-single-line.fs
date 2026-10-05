(*---
max_line_length = 200
---*)
services
    .AddIdentityCore(fun options -> ())
    .AddUserManager<UserManager<web.ApplicationUser>>()
