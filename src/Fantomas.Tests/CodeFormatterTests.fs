module Fantomas.Tests.CodeFormatterTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas
open Fantomas.Extras

[<Test>]
let ``sanitize filename if Program.fs`` () =
    let fileName = "Program.fs"

    let source = """
open System

[<EntryPoint>]
let main argv _ =
    printfn "meh"
    0
"""

    let parsingOptions =
        FakeHelpers.createParsingOptionsFromFile fileName

    CodeFormatter.FormatDocumentAsync(
        fileName,
        SourceOrigin.SourceString source,
        config,
        parsingOptions,
        sharedChecker.Value
    )
    |> Async.RunSynchronously
    |> ignore
