let Ok (content: string) =
    #if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        #if API_GATEWAY
        #else
        Headers = Map.empty.Add("Content-Type", "application/json")
    #endif
    )
#else
#endif
