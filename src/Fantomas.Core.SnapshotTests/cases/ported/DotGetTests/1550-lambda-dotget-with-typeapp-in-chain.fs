services
    .AddIdentityCore<web.ApplicationUser>(fun options ->
            options.User.RequireUniqueEmail <- true
            options.SignIn.RequireConfirmedEmail <- true)
    .AddUserManager<UserManager<web.ApplicationUser>>()
