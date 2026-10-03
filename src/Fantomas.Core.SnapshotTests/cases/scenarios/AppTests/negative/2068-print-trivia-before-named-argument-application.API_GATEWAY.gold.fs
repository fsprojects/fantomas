let Ok (content: string) =
    #if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        #if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
    #else
    #endif
    )
#else
#endif
