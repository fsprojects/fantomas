module Fantomas.Core.Tests.CodeFormatterTests

open Fantomas.Core.SyntaxOak
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
let ``trivia is transformed to Oak`` () =
    let oak =
        CodeFormatter.ParseOakAsync(false, "let a = 0\n // foo")
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    Assert.True(oak.ModulesOrNamespaces.[0].HasContentAfter)

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
        Fantomas.FCS.Parse.parseFile false (FSharp.Compiler.Text.SourceText.ofString source) [ "DEBUG" ]

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
        Fantomas.FCS.Parse.parseFile false (FSharp.Compiler.Text.SourceText.ofString source) [ "DEBUG"; "FOO"; "BAR" ]

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

    let ast, _ =
        Fantomas.FCS.Parse.parseFile false (FSharp.Compiler.Text.SourceText.ofString source) []

    let oak = CodeFormatter.TransformAST(ast, source)

    match oak.ModulesOrNamespaces.[0].Declarations.[0] with
    | ModuleDecl.TopLevelBinding node -> Assert.True node.HasContentBefore
    | _ -> Assert.Fail()
