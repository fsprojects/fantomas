let Ok (content: string) =
    #if API_GATEWAY || MADAPI
    #if API_GATEWAY
    #else
    #endif
    #else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
