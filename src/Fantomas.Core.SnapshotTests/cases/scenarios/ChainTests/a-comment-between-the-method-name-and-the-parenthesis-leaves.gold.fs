let host =
    builder.UseUrls
        // pick the endpoint
        (
            theConfigurationValueForThePublicEndpoint,
            theFallbackEndpointValue
        )
