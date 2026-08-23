module Fantomas.Analyzers.Tests.PipeBackAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.PipeBackAnalyzer

[<Test>]
let ``backward pipe is reported`` () =
    let source: string =
        """module M

let x = id <| 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``the operator is reported, not the expression around it`` () =
    let source: string =
        """module M

let x = id <| 1"""

    let message = List.exactlyOne (analyzeSource cliAnalyzer source)
    Assert.That(message.Range.StartColumn, Is.EqualTo 11)
    Assert.That(message.Range.EndColumn, Is.EqualTo 13)

[<Test>]
let ``a backward pipe inside a string literal is not reported`` () =
    let source: string =
        """module M

let x = "id <| 1" """

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``the forward pipe is not reported`` () =
    let source: string =
        """module M

let x = 1 |> id"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``the operator used as a function value is reported`` () =
    let source: string =
        """module M

let x = (<|) id 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``every occurrence is reported`` () =
    let source: string =
        """module M

let x = id <| 1
let y = id <| 2"""

    analyzeSource cliAnalyzer source |> assertLines [ 3; 4 ]
