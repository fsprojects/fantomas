let parse (checker: FSharpChecker) (parsingOptions: FSharpParsingOptions) { FileName = fileName; Source = source } =
    let allDefineOptions, defineHashTokens = TokenParser.getDefines source

    allDefineOptions
    |> List.map (fun conditionalCompilationDefines ->
        async {
            let parsingOptionsWithDefines =
                { parsingOptions with
                    ConditionalCompilationDefines = conditionalCompilationDefines
                    SourceFiles = Array.map safeFileName parsingOptions.SourceFiles }
            // Run the first phase (untyped parsing) of the compiler
            let sourceText = FSharp.Compiler.Text.SourceText.ofString source

            let! untypedRes = checker.ParseFile(fileName, sourceText, parsingOptionsWithDefines)

            if untypedRes.ParseHadErrors then
                let errors =
                    untypedRes.Diagnostics
                    |> Array.filter (fun e -> e.Severity = FSharpDiagnosticSeverity.Error)

                if not <| Array.isEmpty errors then
                    raise
                    <| FormatException(
                        sprintf "Parsing failed with errors: %A\nAnd options: %A" errors parsingOptionsWithDefines
                    )

            return (untypedRes.ParseTree, conditionalCompilationDefines, defineHashTokens)
        })
    |> Async.Parallel
