module Fantomas.Analyzers.Tests.SnobMatchAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.SnobMatchAnalyzer

[<Test>]
let ``a match on true and false is reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    match x with
    | true -> 1
    | false -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``a match on false and true is reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    match x with
    | false -> 0
    | true -> 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``a match on true and a wildcard is reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    match x with
    | true -> 1
    | _ -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

// The scrutinee is an expression rather than a name, and stays one: the rewrite moves it into the
// condition and writes it once, which is what holds this rule to the boolean case.
[<Test>]
let ``a match on a call returning a boolean is reported`` () =
    let source: string =
        """module M

let f (xs: int list) : int =
    match List.isEmpty xs with
    | true -> 0
    | false -> List.head xs"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``a match on a string constant and a wildcard is reported`` () =
    let source: string =
        """module M

let f (operator: string) : int =
    match operator with
    | ":?" -> 1
    | _ -> 0"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``a match on an integer constant and a wildcard is reported`` () =
    let source: string =
        """module M

let f (x: int) : int =
    match x with
    | -1 -> 0
    | _ -> x"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

// A constant arm after the wildcard is never reached; the compiler says so, and an `if` would
// have to invent the opposite test to keep the order.
[<Test>]
let ``a match on a wildcard and then a constant is not reported`` () =
    let source: string =
        """module M

let f (x: int) : int =
    match x with
    | _ -> 0
    | 1 -> 1"""

    analyzeSource cliAnalyzer source |> assertLines []

// Two constants do not cover the scrutinee between them, so the second arm is not an `else`.
[<Test>]
let ``a match on two constants is not reported`` () =
    let source: string =
        """module M

let f (x: char) : int =
    match x with
    | 'a' -> 1
    | 'b' -> 2"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a match on unit and a wildcard is not reported`` () =
    let source: string =
        """module M

let f (x: unit) : int =
    match x with
    | () -> 1
    | _ -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a match on a union is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | Some value -> value
    | None -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

// The wider shape the rule could grow into, and deliberately not part of it yet: the binder holds
// the scrutinee, so an `if` has to either evaluate it twice or bind it above.
[<Test>]
let ``a match on an integer constant and a binder is not reported`` () =
    let source: string =
        """module M

let f (text: string) : string =
    match text.IndexOf('=') with
    | -1 -> text
    | at -> text.Substring(0, at)"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a guarded arm is not reported`` () =
    let source: string =
        """module M

let f (x: bool) (y: bool) : int =
    match x with
    | true when y -> 1
    | _ -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a match bang is not reported`` () =
    let source: string =
        """module M

let f (x: Async<bool>) : Async<int> =
    async {
        match! x with
        | true -> return 1
        | false -> return 0
    }"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a function is not reported`` () =
    let source: string =
        """module M

let f: bool -> int =
    function
    | true -> 1
    | false -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a conditional directive inside the match is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    match x with
    | true ->
#if DEBUG
        1
#else
        2
#endif
    | false -> 0"""

    analyzeSource cliAnalyzer source |> assertLines []
