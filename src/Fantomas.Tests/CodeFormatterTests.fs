module Fantomas.Tests.CodeFormatterTests

open NUnit.Framework
open Fantomas.Tests.TestHelper
open Fantomas.Core

[<Test>]
let ``sanitize filename if Program.fs`` () =
    let fileName = "Program.fs"

    let source =
        """
open System

[<EntryPoint>]
let main argv _ =
    printfn "meh"
    0
"""

    let parsingOptions = CodeFormatterImpl.createParsingOptionsFromFile fileName

    CodeFormatter.FormatDocumentAsync(
        fileName,
        SourceOrigin.SourceString source,
        config,
        parsingOptions,
        sharedChecker.Value
    )
    |> Async.RunSynchronously
    |> ignore

[<Test>]
let ``sanitize filename if path with Program.fs`` () =
    let fileName = @"d:\dev\bootcamp\src\Program.fs"

    let source =
        """
open System
open Poker.Main

[<EntryPoint>]
let main _ =
    processInput Console.In Console.Out
    0
"""

    let parsingOptions = CodeFormatterImpl.createParsingOptionsFromFile fileName

    CodeFormatter.FormatDocumentAsync(
        fileName,
        SourceOrigin.SourceString source,
        config,
        parsingOptions,
        sharedChecker.Value
    )
    |> Async.RunSynchronously
    |> ignore
