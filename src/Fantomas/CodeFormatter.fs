namespace Fantomas

open FSharp.Compiler.Syntax
open Fantomas.TriviaTypes

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        async {
            let! asts =
                CodeFormatterImpl.getSourceText source
                |> CodeFormatterImpl.parse isSignature

            return
                Array.map
                    (fun (a, d) ->
                        match d with
                        | DefineCombination.NoDefines _ -> (a, [])
                        | DefineCombination.Defines defines -> (a, defines))
                    asts
        }

    static member FormatASTAsync(ast: ParsedInput, defines: string list, source, config) : Async<string> =
        let source = Option.map CodeFormatterImpl.getSourceText source

        CodeFormatterImpl.formatAST ast defines source config
        |> async.Return

    static member FormatDocumentAsync(isSignature, source, config) =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.formatDocument config isSignature

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        CodeFormatterImpl.getSourceText source
        |> CodeFormatterImpl.isValidFSharpCode isSignature

    static member IsValidASTAsync(ast: ParsedInput) = async { return true }

    static member GetVersion() = Version.fantomasVersion.Value
