namespace Fantomas.Core

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(fileName, source, parsingOptions, checker) =
        async {
            let! asts =
                CodeFormatterImpl.createFormatContext fileName source
                |> CodeFormatterImpl.parse checker parsingOptions

            return (Array.map (fun (a, d, _) -> a, d) asts)
        }

    static member FormatASTAsync(ast, fileName, defines, source, config) =
        let formatContext =
            CodeFormatterImpl.createFormatContext fileName (Option.defaultValue (SourceOrigin.SourceString "") source)

        CodeFormatterImpl.formatAST ast defines formatContext config
        |> async.Return

    static member FormatDocumentAsync(fileName, source, config, parsingOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source
        |> CodeFormatterImpl.formatDocument checker parsingOptions config

    static member FormatSelectionAsync(fileName, selection, source, config, parsingOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source
        |> CodeFormatterImpl.formatSelection checker parsingOptions selection config

    static member IsValidFSharpCodeAsync(fileName, source, parsingOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source
        |> CodeFormatterImpl.isValidFSharpCode checker parsingOptions

    static member IsValidASTAsync ast =
        async { return CodeFormatterImpl.isValidAST ast }

    static member MakePos(line, col) = CodeFormatterImpl.makePos line col

    static member MakeRange(fileName, startLine, startCol, endLine, endCol) =
        CodeFormatterImpl.makeRange fileName startLine startCol endLine endCol

    static member GetVersion() = Version.fantomasVersion.Value
