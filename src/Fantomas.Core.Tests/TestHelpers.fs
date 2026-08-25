module Fantomas.Core.Tests.TestHelpers

open System
open Fantomas.Core
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

let formatFSharpString isFsiFile (s: string) config =
    async {
        // Collect comments from input
        let inputSourceText = CodeFormatterImpl.getSourceText s
        let inputAst, _ = Fantomas.FCS.Parse.parseFile isFsiFile inputSourceText []
        let inputComments = Trivia.collectCommentTextsFromAST inputSourceText inputAst

        let! formatted = CodeFormatter.FormatDocumentAsync(isFsiFile, s, config)
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
            CodeFormatter.FormatDocumentAsync(isFsiFile, formattedCode, config)

        let secondFormattedCode = secondFormat.Code.Replace("\r\n", "\n")

        if formattedCode <> secondFormattedCode then
            failwith $"The formatted result was not idempotent.\n%s{formattedCode}\n%s{secondFormattedCode}"

        return formattedCode
    }
    |> Async.RunSynchronously

let formatSignatureString = formatFSharpString true
let formatSourceString = formatFSharpString false

/// The `source` will first be parsed to AST.
let formatAST isFsiFile (source: string) config =
    async {
        let ast, _ =
            Fantomas.FCS.Parse.parseFile isFsiFile (Fantomas.FCS.Text.SourceText.ofString source) []

        let! formattedCode = CodeFormatter.FormatASTAsync(ast, config = config)
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

            return CodeFormatterImpl.formatAST ast (Some source) config None
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
