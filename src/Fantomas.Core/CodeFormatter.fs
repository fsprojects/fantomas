namespace Fantomas.Core

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        async {
            let! results = CodeFormatterImpl.getSourceText source |> CodeFormatterImpl.parse isSignature
            return results |> Array.map (fun (ast, DefineCombination(defines)) -> ast, defines)
        }

    static member FormatASTAsync(ast: ParsedInput) : Async<string> =
        async {
            let result = CodeFormatterImpl.formatAST ast None FormatConfig.Default None
            return result.Code
        }

    static member FormatASTAsync(ast: ParsedInput, config) : Async<string> =
        async {
            let result = CodeFormatterImpl.formatAST ast None config None
            return result.Code
        }

    static member FormatASTAsync(ast: ParsedInput, source) : Async<string> =
        async {
            let sourceText = Some(CodeFormatterImpl.getSourceText source)
            let result = CodeFormatterImpl.formatAST ast sourceText FormatConfig.Default None
            return result.Code
        }

    static member FormatASTAsync(ast: ParsedInput, source, config) : Async<FormatResult> =
        async {
            let sourceText = Some(CodeFormatterImpl.getSourceText source)
            let result = CodeFormatterImpl.formatAST ast sourceText config None
            return result
        }

    static member FormatDocumentAsync(isSignature, source) =
        CodeFormatterImpl.formatDocument FormatConfig.Default isSignature (CodeFormatterImpl.getSourceText source) None

    static member FormatDocumentAsync(isSignature, source, config) =
        CodeFormatterImpl.formatDocument config isSignature (CodeFormatterImpl.getSourceText source) None

    static member FormatDocumentAsync(isSignature, source, config, cursor) =
        CodeFormatterImpl.formatDocument config isSignature (CodeFormatterImpl.getSourceText source) (Some cursor)

    static member FormatSelectionAsync(isSignature, source, selection) =
        CodeFormatterImpl.getSourceText source
        |> Selection.formatSelection FormatConfig.Default isSignature selection

    static member FormatSelectionAsync(isSignature, source, selection, config) =
        CodeFormatterImpl.getSourceText source
        |> Selection.formatSelection config isSignature selection

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        Validation.isValidFSharpCode isSignature source

    static member GetVersion() = Version.fantomasVersion.Value

    static member MakeRange(fileName, startLine, startCol, endLine, endCol) =
        Range.mkRange fileName (Position.mkPos startLine startCol) (Position.mkPos endLine endCol)

    static member MakePosition(line, column) = Position.mkPos line column

    static member ParseOakAsync(isSignature: bool, source: string) : Async<(Oak * string list) array> =
        async {
            let sourceText = CodeFormatterImpl.getSourceText source
            let! ast = CodeFormatterImpl.parse isSignature sourceText

            return
                ast
                |> Array.map (fun (ast, defines) ->
                    let oak = ASTTransformer.mkOak (Some sourceText) ast
                    let oak = Trivia.enrichTree FormatConfig.Default sourceText ast oak
                    oak, defines.Value)
        }

    static member TransformAST ast = ASTTransformer.mkOak None ast

    static member TransformAST(ast, source) =
        let sourceText = SourceText.ofString source
        let oak = ASTTransformer.mkOak (Some sourceText) ast
        Trivia.enrichTree FormatConfig.Default sourceText ast oak

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
