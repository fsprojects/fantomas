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

let formatSourceString isFsiFile (s: string) config =
    async {
        let! formatted = CodeFormatter.FormatDocumentAsync(isFsiFile, s, config)
        let formattedCode = formatted.Code.Replace("\r\n", "\n")
        let! isValid = CodeFormatter.IsValidFSharpCodeAsync(isFsiFile, formattedCode)

        if not isValid then
            failwithf $"The formatted result is not valid F# code or contains warnings\n%s{formattedCode}"

        let! secondFormat = CodeFormatter.FormatDocumentAsync(isFsiFile, formattedCode, config)
        let secondFormattedCode = secondFormat.Code.Replace("\r\n", "\n")

        if formattedCode <> secondFormattedCode then
            failwithf $"The formatted result was not idempotent.\n%s{formattedCode}"

        return formattedCode
    }

    |> Async.RunSynchronously

/// The `source` will first be parsed to AST.
let formatAST isFsiFile (source: string) config =
    async {
        let ast, _ =
            Fantomas.FCS.Parse.parseFile isFsiFile (FSharp.Compiler.Text.SourceText.ofString source) []

        let! formattedCode = CodeFormatter.FormatASTAsync(ast, config = config)
        let! isValid = CodeFormatter.IsValidFSharpCodeAsync(isFsiFile, formattedCode)

        if not isValid then
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
    CodeFormatter.IsValidFSharpCodeAsync(isFsiFile, s) |> Async.RunSynchronously

let equal x =
    let x =
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x

    equal x

let inline prepend s content = s + content
let (==) actual expected = Assert.AreEqual(expected, actual)
