module Fantomas.Analyzers.Tests.UnnecessaryParensAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.UnnecessaryParensAnalyzer

[<Test>]
let ``parentheses around the body of a binding are reported`` () =
    let source: string =
        """module M

let x: int = (1 + 2)"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``parentheses that hold an operand together are not reported`` () =
    let source: string =
        """module M

let x: int = (1 + 2) * 3"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``parentheses around a parameter name are reported`` () =
    let source: string =
        """module M

let f (x) = x"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``parentheses around an annotated parameter are not reported`` () =
    let source: string =
        """module M

let f (x: int) = x"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``parentheses around the condition of an if are reported`` () =
    let source: string =
        """module M

let a: bool = true
let b: int = if (a) then 1 else 2"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``parentheses around an argument that is itself an application are not reported`` () =
    let source: string =
        """module M

let f (x: int) : int = x
let y: int = f (f 1)"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``parentheses around an argument that is a single name are reported`` () =
    let source: string =
        """module M

let f (x: int) : int = x
let y: int = 1
let z: int = f (y)"""

    analyzeSource cliAnalyzer source |> assertLines [ 5 ]

/// `- f 1` would negate `f` and then apply the result, so the pair is load bearing and the rule
/// stays quiet. This is the guard `shouldBeParenthesizedInContext` provides and the reason the rule
/// asks the compiler rather than looking for a paren whose contents parse on their own.
[<Test>]
let ``parentheses a unary minus needs are not reported`` () =
    let source: string =
        """module M

let f (x: int) : int = x
let z: int = -(f 1)"""

    analyzeSource cliAnalyzer source |> assertLines []

/// The other way round, and a surprise worth pinning: `f -1` applies `f` to a negative literal
/// rather than subtracting, so the pair really is unnecessary and the rule says so.
[<Test>]
let ``parentheses around a negative literal argument are reported`` () =
    let source: string =
        """module M

let f (x: int) : int = x
let y: int = f (-1)"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``parentheses written against a union case are not reported`` () =
    let source: string =
        """module M

let x: int option = Some(1)"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``parentheses written against a constructor are not reported`` () =
    let source: string =
        """module M

open System.Text

let sb: StringBuilder = StringBuilder(64)"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``parentheses written against a type application are not reported`` () =
    let source: string =
        """module M

open System.Collections.Generic

let d: Dictionary<int, string> = Dictionary<int, string>(HashIdentity.Structural)"""

    analyzeSource cliAnalyzer source |> assertLines []

/// The same call with a space in front of the parenthesis is a different thing to write and is
/// reported, which is what makes the guard a question about the text rather than about the tree.
[<Test>]
let ``parentheses set off from the name they apply to are reported`` () =
    let source: string =
        """module M

let x: int option = Some (1)"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``parentheses written against a union case pattern are not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some(y) -> y"""

    analyzeSource cliAnalyzer source |> assertLines []

/// The case the agent has to re-indent. The rule says nothing about the edit, so the test says
/// nothing about it either: what is asserted is that the pair is found at all.
[<Test>]
let ``parentheses around a body spanning several lines are reported`` () =
    let source: string =
        """module M

let a: bool = true

let x: int =
    (if a then
        1
     else
        2)"""

    analyzeSource cliAnalyzer source |> assertLines [ 6 ]

/// Two findings in one file, to pin the order they arrive in. The fold reaches a node before the
/// nodes under it and says nothing about siblings, so without the sort this is whatever a
/// dictionary happens to enumerate.
[<Test>]
let ``findings are reported in the order the file reads`` () =
    let source: string =
        """module M

let f (x) = x
let y: int = (1 + 2)"""

    analyzeSource cliAnalyzer source |> assertLines [ 3; 4 ]
