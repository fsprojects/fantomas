namespace Fantomas.Core

open FSharp.Compiler.Syntax

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        async {
            let! asts =
                CodeFormatterImpl.getSourceText source
                |> CodeFormatterImpl.parse isSignature

            return asts
        }

    static member FormatASTAsync(ast: ParsedInput, defines: string list, source, config) : Async<string> =
        let sourceAndTokens = Option.map CodeFormatterImpl.getSourceText source

        CodeFormatterImpl.formatAST ast defines sourceAndTokens config
        |> async.Return

    static member FormatDocumentAsync(isSignature, source, config) =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.formatDocument config isSignature

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        Validation.isValidFSharpCode isSignature source

    static member GetVersion() = Version.fantomasVersion.Value
