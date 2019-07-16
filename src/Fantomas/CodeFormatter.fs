namespace Fantomas

open Fantomas

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(fileName, source) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.parse

    static member ParseAsync(fileName, source, projectOptions, checker) = 
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.parse

    static member FormatASTAsync(ast, fileName, source, config) =
        async {
            let formatContext = CodeFormatterImpl.createFormatContextNoChecker fileName (Option.defaultValue (SourceOrigin.SourceString "") source)
            return CodeFormatterImpl.formatAST ast formatContext config
        }

    static member FormatAroundCursorAsync(fileName, cursorPos, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatAroundCursor cursorPos config

    static member FormatDocumentAsync(fileName, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatDocument config

    static member FormatDocumentAsync(fileName, source, config) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.formatDocument config

    static member FormatSelectionAsync(fileName, selection, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatSelection selection config

    static member internal FormatSelectionInDocumentAsync(fileName, selection, source, config, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.formatSelectionInDocument selection config

    static member IsValidFSharpCodeAsync(fileName, source, projectOptions, checker) =
        CodeFormatterImpl.createFormatContext fileName source projectOptions checker
        |> CodeFormatterImpl.isValidFSharpCode

    static member IsValidFSharpCodeAsync(fileName, source) =
        CodeFormatterImpl.createFormatContextNoChecker fileName source
        |> CodeFormatterImpl.isValidFSharpCode

    static member IsValidASTAsync ast = 
        async { return CodeFormatterImpl.isValidAST ast }

    static member MakePos(line, col) = 
        CodeFormatterImpl.makePos line col

    static member MakeRange(fileName, startLine, startCol, endLine, endCol) = 
        CodeFormatterImpl.makeRange fileName startLine startCol endLine endCol

    static member InferSelectionFromCursorPos(fileName, cursorPos, source) =
        CodeFormatterImpl.inferSelectionFromCursorPos cursorPos fileName source