(*---
max_line_length = 60
---*)
let host =
    builder.UseUrls
        // pick the endpoint
        (theConfigurationValueForThePublicEndpoint, theFallbackEndpointValue)
