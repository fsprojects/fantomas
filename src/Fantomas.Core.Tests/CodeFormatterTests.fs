module Fantomas.Core.Tests.CodeFormatterTests

open NUnit.Framework
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``sanitize filename if Program.fs`` () =

    let source =
        """
open System

[<EntryPoint>]
let main argv _ =
    printfn "meh"
    0
"""

    CodeFormatter.FormatDocumentAsync(false, source, config)
    |> Async.RunSynchronously
    |> ignore

[<Test>]
let ``sanitize filename if path with Program.fs`` () =
    let source =
        """
open System
open Poker.Main

[<EntryPoint>]
let main _ =
    processInput Console.In Console.Out
    0
"""

    CodeFormatter.FormatDocumentAsync(false, source, config)
    |> Async.RunSynchronously
    |> ignore

[<Test>]
let ``trivia is parsed for Oak`` () =
    let oak =
        CodeFormatter.ParseOakAsync(false, "let a = 0\n // foo")
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    Assert.True(oak.ModulesOrNamespaces.[0].HasContentAfter)

[<Test>]
let ``parsed oak can be formatted back to source`` () =
    let source = "$\"gc{i}\""

    let oak =
        CodeFormatter.ParseOakAsync(false, source)
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    let formatted =
        CodeFormatter.FormatOakAsync(
            oak,
            { FormatConfig.Default with
                InsertFinalNewline = false }
        )
        |> Async.RunSynchronously

    Assert.AreEqual(source, formatted)
