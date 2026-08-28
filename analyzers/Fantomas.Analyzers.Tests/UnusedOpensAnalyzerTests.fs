module Fantomas.Analyzers.Tests.UnusedOpensAnalyzerTests

open NUnit.Framework
open FSharp.Analyzers.SDK
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.UnusedOpensAnalyzer

[<Test>]
let ``an open nothing uses is reported`` () =
    let source: string =
        """module M

open System

let y: int = 42"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``an open something uses is not reported`` () =
    let source: string =
        """module M

open System

let y: int = Int32.MaxValue"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``only the unused one of two opens is reported`` () =
    let source: string =
        """module M

open System
open System.Collections.Generic

let y: int = Dictionary<string, int>().Count"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``an open used only from a nested module is not reported`` () =
    let source: string =
        """module M

open System.Collections.Generic

module Inner =
    let y: int = Dictionary<string, int>().Count"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``the reported range covers the module identifier alone`` () =
    let source: string =
        """module M

open System

let y: int = 42"""

    let message: Message = analyzeSource cliAnalyzer source |> List.exactlyOne

    // `open System` puts `System` at columns 5 to 11. The range names the module rather than the
    // whole declaration, so what gets deleted is the line the range sits on and not the range.
    Assert.That(
        (message.Range.StartLine, message.Range.StartColumn, message.Range.EndLine, message.Range.EndColumn),
        Is.EqualTo((3, 5, 3, 11))
    )

[<Test>]
let ``an unused open in a signature file is reported`` () =
    let signature: string =
        """module M

open System

val y: int"""

    let implementation: string =
        """module M

let y: int = 42"""

    analyzeSignature cliAnalyzer signature implementation |> assertLines [ 3 ]

[<Test>]
let ``an open a signature file uses is not reported`` () =
    let signature: string =
        """module M

open System

val y: Int32"""

    let implementation: string =
        """module M

let y: System.Int32 = 42"""

    analyzeSignature cliAnalyzer signature implementation |> assertLines []
