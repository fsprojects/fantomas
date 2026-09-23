module Fantomas.Core.Tests.TestHelpers

open System
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open NUnit.Framework
open FsUnit

[<assembly: Parallelizable(ParallelScope.All)>]
do ()

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str: string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

let config = FormatConfig.Default
let newline = "\n"

/// Trivia assignment in `Trivia.fs` takes the order of `Node.Children` to be the order in the source.
/// Every `Children` array is written by hand, so check it for each input the tests format.
/// Children may overlap, as ranges from the parser can nest, but none may start before the one in front of it.
/// Nodes that `ASTTransformer` makes up carry `range0` and have no place in the source.
/// It is handed to `CodeFormatterImpl.formatDocumentWith`, which calls it on the Oak it is about to print.
let assertChildrenInSourceOrder (oak: Oak) : unit =
    let rec visit (node: Node) : unit =
        node.Children
        |> Array.filter (fun child -> not (Range.equals child.Range Range.range0))
        |> Array.pairwise
        |> Array.iter (fun (previous, next) ->
            if Position.posLt next.Range.Start previous.Range.Start then
                failwith
                    $"The children of %s{node.GetType().Name} are not in source order: %s{next.GetType().Name} %O{next.Range} comes after %s{previous.GetType().Name} %O{previous.Range}"
        )

        Array.iter visit node.Children

    visit oak

let formatFSharpString isFsiFile (s: string) config =
    async {
        // Collect comments from input
        let inputSourceText = CodeFormatterImpl.getSourceText s
        let inputAst, _ = Fantomas.FCS.Parse.parseFile isFsiFile inputSourceText []
        let inputComments = Trivia.collectCommentTextsFromAST inputSourceText inputAst

        let! formatted =
            CodeFormatterImpl.formatDocumentWith assertChildrenInSourceOrder config isFsiFile inputSourceText None

        let formattedCode = formatted.Code.Replace("\r\n", "\n")

        // Validity check — inlined, reusing AST for comment check below
        let formattedSourceText = Fantomas.FCS.Text.SourceText.ofString formattedCode

        let formattedAst, diagnostics =
            Fantomas.FCS.Parse.parseFile isFsiFile formattedSourceText []

        if not (Validation.noWarningOrErrorDiagnostics diagnostics) then
            failwith $"The formatted result is not valid F# code or contains warnings\n%s{formattedCode}"

        // Comment preservation check
        let outputComments =
            Trivia.collectCommentTextsFromAST formattedSourceText formattedAst

        if inputComments <> outputComments then
            let missing = inputComments - outputComments
            let extra = outputComments - inputComments

            failwith
                $"Comment trivia was not preserved.\nMissing: %A{missing}\nExtra: %A{extra}\nFormatted code:\n%s{formattedCode}"

        // Idempotency check
        let! secondFormat =
            CodeFormatterImpl.formatDocumentWith
                assertChildrenInSourceOrder
                config
                isFsiFile
                (CodeFormatterImpl.getSourceText formattedCode)
                None

        let secondFormattedCode = secondFormat.Code.Replace("\r\n", "\n")

        if formattedCode <> secondFormattedCode then
            failwith $"The formatted result was not idempotent.\n%s{formattedCode}\n%s{secondFormattedCode}"

        return formattedCode
    }
    |> Async.RunSynchronously

let formatSignatureString = formatFSharpString true
let formatSourceString = formatFSharpString false

let formatAST isFsiFile (source: string) config =
    async {
        let ast, _ =
            Fantomas.FCS.Parse.parseFile isFsiFile (Fantomas.FCS.Text.SourceText.ofString source) []

        let formattedCode: string =
            (CodeFormatterImpl.formatASTWith assertChildrenInSourceOrder ast None config None).Code

        let! validation = CodeFormatter.ValidateFSharpCodeAsync(isFsiFile, formattedCode)

        if not validation.IsValid then
            failwithf $"The formatted result is not valid F# code or contains warnings\n%s{formattedCode}"

        return formattedCode.Replace("\r\n", "\n")
    }
    |> Async.RunSynchronously

let formatSourceStringWithDefines defines (s: string) config =
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)

    let result =
        async {
            let source = CodeFormatterImpl.getSourceText s
            let! asts = CodeFormatterImpl.parse false source

            let ast =
                Array.filter (fun (_, DefineCombination(d)) -> List.sort d = List.sort defines) asts
                |> Array.head
                |> fst

            return CodeFormatterImpl.formatASTWith assertChildrenInSourceOrder ast (Some source) config None
        }
        |> Async.RunSynchronously

    let defines = DefineCombination(defines)

    // merge with itself to make #if go on beginning of line
    let mergedFormatResult =
        MultipleDefineCombinations.mergeMultipleFormatResults config [ (defines, result); (defines, result) ]

    String.normalizeNewLine mergedFormatResult.Code

let isValidFSharpCode isFsiFile s =
    let validation: ValidationResult =
        CodeFormatter.ValidateFSharpCodeAsync(isFsiFile, s) |> Async.RunSynchronously

    validation.IsValid

let equal x =
    let x =
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x

    equal x

let inline prepend s content = s + content
let (==) actual expected = Assert.AreEqual(expected, actual)
