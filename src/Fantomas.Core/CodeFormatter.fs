namespace Fantomas.Core

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core.SyntaxOak

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        CodeFormatterImpl.getSourceText source |> CodeFormatterImpl.parse isSignature

    static member FormatASTAsync(ast: ParsedInput, ?source, ?config) : Async<FormatResult> =
        let sourceText = Option.map CodeFormatterImpl.getSourceText source
        let config = Option.defaultValue FormatConfig.Default config

        CodeFormatterImpl.formatAST ast sourceText config None |> async.Return

    static member FormatDocumentAsync(isSignature, source, ?config, ?cursor: Position) =
        let config = Option.defaultValue FormatConfig.Default config
        CodeFormatterImpl.formatDocument config isSignature (CodeFormatterImpl.getSourceText source) cursor

    static member FormatSelectionAsync(isSignature, source, selection, config) =
        let config = Option.defaultValue FormatConfig.Default config

        CodeFormatterImpl.getSourceText source
        |> Selection.formatSelection config isSignature selection

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        Validation.isValidFSharpCode isSignature source

    static member GetVersion() = Version.fantomasVersion.Value

    static member MakeRange(fileName, startLine, startCol, endLine, endCol) =
        Range.mkRange fileName (Position.mkPos startLine startCol) (Position.mkPos endLine endCol)

    static member MakePosition(line, column) = Position.mkPos line column

    [<Experimental "Only for local development">]
    static member ParseOakAsync(isSignature: bool, source: string) : Async<(Oak * string list) array> =
        async {
            let sourceText = CodeFormatterImpl.getSourceText source
            let! ast = CodeFormatterImpl.parse isSignature sourceText

            return
                ast
                |> Array.map (fun (ast, defines) ->
                    let oak = ASTTransformer.mkOak (Some sourceText) ast
                    oak, defines)
        }

    static member FormatOakAsync(oak: Oak) : Async<string> =
        async {
            let context = Context.Context.Create FormatConfig.Default
            let result = context |> CodePrinter.genFile oak |> Context.dump false
            return result.Code
        }

    static member FormatOakAsync(oak: Oak, config: FormatConfig) : Async<string> =
        async {
            let context = Context.Context.Create config
            let result = context |> CodePrinter.genFile oak |> Context.dump false
            return result.Code
        }
