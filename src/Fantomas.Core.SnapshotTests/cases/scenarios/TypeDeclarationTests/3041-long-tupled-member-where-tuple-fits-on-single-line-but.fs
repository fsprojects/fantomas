type FSharpChecker with

    member this.ParseAndCheckDocument
        (
            filePath: string,
            sourceText: string,
            options: FSharpProjectOptions,
            allowStaleResults: bool
        ) : Async<(FSharpParseFileResults * ParsedInput * FSharpCheckFileResults) option> =
        ()
