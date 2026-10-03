client
    .Post(
        endpointUrl,
        serializedRequestPayload,
        requestHeaders
    )
    .EnsureSuccessStatusCode()
    .Content
