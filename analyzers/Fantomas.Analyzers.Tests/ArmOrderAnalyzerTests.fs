module Fantomas.Analyzers.Tests.ArmOrderAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.ArmOrderAnalyzer

[<Test>]
let ``a long arm before a one line arm is reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
        let a: int = y
        a + 1
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``the shorter arm coming first is not reported`` () =
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
let ``two one line arms are not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y -> y
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

// A list is matched by `::` and `[]` rather than by union cases, and those two are as disjoint as
// any pair. `dataToEnd` in Queue.fs is the shape this came from, which is also a `function` rather
// than a `match`.
[<Test>]
let ``a cons arm before an empty list arm is reported`` () =
    let source: string =
        """module M

let rec f (acc: int list) : int list -> int list =
    function
    | hd :: tl ->
        let a: int = hd
        f (a :: acc) tl
    | [] -> acc"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``an empty list arm coming first is not reported`` () =
    let source: string =
        """module M

let rec f (acc: int list) : int list -> int list =
    function
    | [] -> acc
    | hd :: tl ->
        let a: int = hd
        f (a :: acc) tl"""

    analyzeSource cliAnalyzer source |> assertLines []

// `[ x ]` and `x :: rest` both match a one element list, so those two cannot be swapped and the
// rule has to stay quiet.
[<Test>]
let ``a cons arm before a single element list arm is not reported`` () =
    let source: string =
        """module M

let f (xs: int list) : int =
    match xs with
    | hd :: _ ->
        let a: int = hd
        a + 1
    | [ x ] -> x"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a wildcard arm is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
        let a: int = y
        a + 1
    | _ -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a bare binder in the second arm is not reported`` () =
    let source: string =
        """module M

let f (x: int) : int =
    match x with
    | 1 ->
        let a: int = x
        a + 1
    | other -> other"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a guarded arm is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y when y > 0 ->
        let a: int = y
        a + 1
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a guard on the second arm is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
        let a: int = y
        a + 1
    | None when true -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``the same case in both arms is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some 1 ->
        let a: int = 1
        a + 1
    | Some y -> y"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``more than two arms is not reported`` () =
    let source: string =
        """module M

type Choice3 =
    | A
    | B
    | C

let f (x: Choice3) : int =
    match x with
    | A ->
        let a: int = 1
        a + 1
    | B -> 2
    | C -> 3"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a comment between the arms is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
        let a: int = y
        a + 1
    // the empty case
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a comment inside the long arm is still reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
        // this travels with its arm
        let a: int = y
        a + 1
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 9 ]

[<Test>]
let ``a conditional directive inside the match is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some y ->
#if DEBUG
        let a: int = y
#else
        let a: int = 0
#endif
        a + 1
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a qualified case name is compared on its final identifier`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Option.Some y ->
        let a: int = y
        a + 1
    | Option.None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``a function keyword is reported`` () =
    let source: string =
        """module M

let f: int option -> int =
    function
    | Some y ->
        let a: int = y
        a + 1
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

[<Test>]
let ``a try with is not reported`` () =
    let source: string =
        """module M

open System

let f (g: unit -> int) : int =
    try
        g ()
    with
    | :? InvalidOperationException ->
        let a: int = 1
        a + 1
    | :? NotSupportedException -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []
