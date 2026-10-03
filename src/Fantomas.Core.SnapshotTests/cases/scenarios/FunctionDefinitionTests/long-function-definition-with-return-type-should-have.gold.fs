module FormatCode =

    let private format filename code config =
        let checker = Fantomas.FakeHelpers.sharedChecker.Force()
        let options = Fantomas.FakeHelpers.createParsingOptionsFromFile filename
        let source = SourceOrigin.SourceString code
        CodeFormatter.FormatDocumentAsync("tmp.fsx", source, config, options, checker)

    [<FunctionName("FormatCode")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
        (log: ILogger)
        : HttpResponse =
        Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
