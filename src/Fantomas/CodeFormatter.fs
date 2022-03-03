namespace Fantomas

open FSharp.Compiler.Syntax

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        async {
            let! asts =
                CodeFormatterImpl.getSourceText source
                |> CodeFormatterImpl.parse isSignature

            return Array.map (fun (a, _, d) -> a, d) asts
        }

    static member FormatASTAsync(ast: ParsedInput, defines: string list, source, config) : Async<string> =
        let sourceAndTokens =
            Option.map
                (fun source ->
                    let sourceText = CodeFormatterImpl.getSourceText source
                    let tokens, _ = TokenParser.getDefineCombination sourceText
                    sourceText, tokens)
                source

        CodeFormatterImpl.formatAST ast defines sourceAndTokens config
        |> async.Return

    static member FormatDocumentAsync(isSignature, source, config) =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.formatDocument config isSignature

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.isValidFSharpCode isSignature

    static member IsValidASTAsync(ast: ParsedInput) = async { return true }

    static member GetVersion() = Version.fantomasVersion.Value
