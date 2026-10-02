    task {
        let! config = manager.GetConfigurationAsync().ConfigureAwait(false)
        parameters.IssuerSigningKeys <- config.SigningKeys
        let user, _ = handler.ValidateToken((token: string), parameters)
        return Ok(user.Identity.Name, collectClaims user)
    }
