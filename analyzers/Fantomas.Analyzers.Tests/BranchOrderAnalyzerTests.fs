module Fantomas.Analyzers.Tests.BranchOrderAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.BranchOrderAnalyzer

[<Test>]
let ``a long then before a one line else is reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
        let a: int = 1
        a + 1
    else
        0"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``the shorter branch coming first is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
        0
    else
        let a: int = 1
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``two one line branches are not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int = if x then 1 else 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``two long branches are not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
        let a: int = 1
        a + 1
    else
        let b: int = 2
        b + 2"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an if without an else is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : unit =
    if x then
        let a: int = 1
        printfn "%i" a"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an elif chain is not reported`` () =
    let source: string =
        """module M

let f (x: int) : int =
    if x = 1 then
        let a: int = 1
        a + 1
    elif x = 2 then
        2
    else
        3"""

    analyzeSource cliAnalyzer source |> assertLines []

// Negating a condition joined by `&&` or `||` means wrapping the whole of it in `not`, which reads
// worse than the branches were worth.
[<Test>]
let ``a condition joined by and is not reported`` () =
    let source: string =
        """module M

let f (x: bool) (y: bool) : int =
    if x && y then
        let a: int = 1
        a + 1
    else
        0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a condition joined by or is not reported`` () =
    let source: string =
        """module M

let f (x: bool) (y: bool) : int =
    if x || y then
        let a: int = 1
        a + 1
    else
        0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a comparison is reported`` () =
    let source: string =
        """module M

let f (x: int) : int =
    if x > 0 then
        let a: int = x
        a + 1
    else
        0"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``an already negated condition is reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if not x then
        let a: int = 1
        a + 1
    else
        0"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``a conditional directive inside the if is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
#if DEBUG
        let a: int = 1
#else
        let a: int = 2
#endif
        a + 1
    else
        0"""

    analyzeSource cliAnalyzer source |> assertLines []
