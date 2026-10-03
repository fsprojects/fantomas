let rec run
    ([<HttpTrigger(AuthorizationLevel.Anonymous,
                   "get",
                   "post",
                   Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse
    =
    logAnalyticsForRequest log req

    Http.main
        CodeFormatter.GetVersion
        format
        FormatConfig.FormatConfig.Default
        log
        req

and logAnalyticsForRequest
    (log: ILogger)
    (httpRequest: HttpRequest)
    =
    log.Info(sprintf "Meh: %A" httpRequest)
