namespace Fantomas.Core

open FSharp.Compiler.Syntax

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.parse isSignature

    static member FormatASTAsync(ast: ParsedInput, source, config) : Async<string> =
        let sourceAndTokens = Option.map CodeFormatterImpl.getSourceText source

        CodeFormatterImpl.formatAST ast sourceAndTokens config None
        |> async.Return

    static member FormatDocumentAsync(isSignature, source, config) =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.formatDocument config isSignature

    // TODO: should this return the range of the actual formatted node
    // The selection might have been larger than the actual formatted node
    static member FormatSelectionAsync(isSignature, source, selection, config) =
        CodeFormatterImpl.getSourceText source
        |> Selection.formatSelection config isSignature selection

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        Validation.isValidFSharpCode isSignature source

    static member GetVersion() = Version.fantomasVersion.Value
