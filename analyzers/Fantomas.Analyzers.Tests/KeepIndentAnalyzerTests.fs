module Fantomas.Analyzers.Tests.KeepIndentAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.KeepIndentAnalyzer

[<Test>]
let ``a last arm holding a block is reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some y ->
        let a: int = y
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 7 ]

[<Test>]
let ``a last arm already keeping the indentation is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some y ->

    let a: int = y
    a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a last arm holding a nested match is reported`` () =
    let source: string =
        """module M

let f (x: int option) (y: int option) : int =
    match x with
    | Some a -> a
    | None ->
        match y with
        | None -> 0
        | Some b -> b"""

    analyzeSource cliAnalyzer source |> assertLines [ 7 ]

[<Test>]
let ``a last arm holding a single expression is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : string =
    match x with
    | None -> ""
    | Some y ->
        y
        |> string
        |> String.replicate 2"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a one line last arm is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some y -> y + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a block in an arm that is not the last one is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
        let a: int = y
        a + 1
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``code following the match in the same block is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : unit =
    match x with
    | None -> ()
    | Some y ->
        let a: int = y
        printfn "%i" a

    printfn "done" """

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``code following the match further left is still reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    let r: int =
        match x with
        | None -> 0
        | Some y ->
            let a: int = y
            a + 1

    r + 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``a guarded last arm is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some y when y > 0 ->
        let a: int = y
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a conditional directive inside the match is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some y ->
#if DEBUG
        let a: int = y
#else
        let a: int = 0
#endif
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``the last arm of a function keyword is reported`` () =
    let source: string =
        """module M

let f: int option -> int =
    function
    | None -> 0
    | Some y ->
        let a: int = y
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 7 ]

[<Test>]
let ``the last arm of a match bang is reported`` () =
    let source: string =
        """module M

let f (x: Async<int option>) : Async<int> =
    async {
        match! x with
        | None -> return 0
        | Some y ->
            let a: int = y
            return a + 1
    }"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]
