module Fantomas.Core.Tests.CodeFormatterTests

open Fantomas.Core.SyntaxOak
open NUnit.Framework
open Fantomas.FCS.Text
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
let ``trivia is transformed to Oak`` () =
    let oak =
        CodeFormatter.ParseOakAsync(false, "let a = 0\n // foo")
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    Assert.That(oak.ModulesOrNamespaces.[0].HasContentAfter, Is.True)

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
                InsertFinalNewline = false
            }
        )
        |> Async.RunSynchronously

    Assert.That(formatted, Is.EqualTo source)

[<Test>]
let ``transform parsedInput to Oak`` () =
    let source =
        """
module A

#if DEBUG
let b = 0
#endif
"""

    let ast, _ =
        Fantomas.FCS.Parse.parseFile false (SourceText.ofString source) [ "DEBUG" ]

    let oak = CodeFormatter.TransformAST(ast, source)

    match oak.ModulesOrNamespaces.[0].Declarations.[0] with
    | ModuleDecl.TopLevelBinding _ -> Assert.Pass()
    | _ -> Assert.Fail()

[<Test>]
let ``transform parsedInput created with additional defines to Oak`` () =
    let source =
        """
module A

#if DEBUG
let b = 0
#endif
"""

    let ast, _ =
        Fantomas.FCS.Parse.parseFile false (SourceText.ofString source) [ "DEBUG"; "FOO"; "BAR" ]

    let oak = CodeFormatter.TransformAST ast

    match oak.ModulesOrNamespaces.[0].Declarations.[0] with
    | ModuleDecl.TopLevelBinding _ -> Assert.Pass()
    | _ -> Assert.Fail()

[<Test>]
let ``transform parsedInput contains trivia in Oak`` () =
    let source =
        """
module A

// foo
let b = 0
"""

    let ast, _ = Fantomas.FCS.Parse.parseFile false (SourceText.ofString source) []

    let oak = CodeFormatter.TransformAST(ast, source)

    match oak.ModulesOrNamespaces.[0].Declarations.[0] with
    | ModuleDecl.TopLevelBinding node -> Assert.That(node.HasContentBefore, Is.True)
    | _ -> Assert.Fail()

[<Test>]
let ``GetWriterEventsAsync emits NodeStart and NodeEnd events`` () =
    let events =
        CodeFormatter.GetWriterEventsAsync(false, "let x = 1") |> Async.RunSynchronously

    let nodeStarts =
        events
        |> Array.choose (
            function
            | WriterEvent.NodeStart(nodeType, _) -> Some nodeType
            | _ -> None
        )

    let nodeEnds =
        events
        |> Array.choose (
            function
            | WriterEvent.NodeEnd(nodeType, _) -> Some nodeType
            | _ -> None
        )

    Assert.That(nodeStarts, Is.Not.Empty)
    Assert.That(nodeEnds, Is.Not.Empty)
    // Every NodeStart should have a matching NodeEnd
    Assert.That(nodeStarts.Length, Is.EqualTo nodeEnds.Length)
    // Oak is the root node
    Assert.That(nodeStarts.[0], Is.EqualTo "Oak")
    Assert.That(nodeEnds |> Array.last, Is.EqualTo "Oak")
