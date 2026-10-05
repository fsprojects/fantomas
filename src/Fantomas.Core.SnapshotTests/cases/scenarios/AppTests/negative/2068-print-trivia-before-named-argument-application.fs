let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
