let private authenticateRequest (logger: ILogger) header =
    let token =
        System.Text.RegularExpressions.Regex.Replace(header, "bearer\s?", System.String.Empty)

    printfn "token: %s" token
    Microsoft.IdentityModel.Logging.IdentityModelEventSource.ShowPII <- true
    let parameters = TokenValidationParameters()
    parameters.ValidIssuer <- (sprintf "https://%s/" Auth0Domain)
    parameters.ValidAudiences <- Auth0Audiences
    parameters.ValidateIssuer <- true
    parameters.NameClaimType <- ClaimTypes.NameIdentifier // Auth0 related, see https://community.auth0.com/t/access-token-doesnt-contain-a-sub-claim/17671/2

    let manager =
        ConfigurationManager<OpenIdConnectConfiguration>
            (sprintf "https://%s/.well-known/openid-configuration" Auth0Domain, OpenIdConnectConfigurationRetriever())

    let handler = JwtSecurityTokenHandler()

    try
        task {
            let! config = manager.GetConfigurationAsync().ConfigureAwait(false)
            parameters.IssuerSigningKeys <- config.SigningKeys

            let user, _ = handler.ValidateToken((token: string), parameters)

            if user.HasPermission("use:application") then
                return Some user.Identity.Name
            else
                logger.LogError(sprintf "User has a valid token but lacks the correct permission")
                return None
        }
    with exn ->
        logger.LogError(sprintf "Could not authenticate token %s\n%A" token exn)
        task { return None }
