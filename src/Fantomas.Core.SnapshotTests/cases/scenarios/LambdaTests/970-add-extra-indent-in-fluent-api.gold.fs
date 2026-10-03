services
    .AddAuthentication(fun options ->
        options.DefaultScheme <- "Cookies"
        options.DefaultChallengeScheme <- "oidc")
    .AddCookie("Cookies")
    .AddOpenIdConnect(fun options ->
        options.Authority <- "http://localhost:7001"
        options.ClientId <- "mvc"
        options.ClientSecret <- "secret"
        options.ResponseType <- "code"
        options.SaveTokens <- true)
