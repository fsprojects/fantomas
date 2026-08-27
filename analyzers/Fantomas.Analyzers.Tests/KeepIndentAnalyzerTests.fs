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

// De-indenting one of two blocks says the last is a happy path and the other is not, when they are
// the same kind of thing. `collectTriviaFromCodeComments` in Trivia.fs is the case this came from.
[<Test>]
let ``a match with a second block arm is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None ->
        let b: int = 1
        b + 1
    | Some y ->
        let a: int = y
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a single arm destructuring is reported`` () =
    let source: string =
        """module M

type Wrapper = Wrapper of int

let f (x: Wrapper) : int =
    match x with
    | Wrapper y ->
        let a: int = y
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 8 ]

// A bracketed body indents its items, and the items of a `for` inside it again. Fantomas holds it
// in the column of the bar like any other block. `overruledLines` in Report.fs is the case this
// came from.
[<Test>]
let ``a last arm holding a list is reported`` () =
    let source: string =
        """module M

let f (xs: string list) : string list =
    match xs with
    | [] -> []
    | files ->
        [
            for file in files do
                let path: string = file
                path

            "trailing line"
        ]"""

    analyzeSource cliAnalyzer source |> assertLines [ 7 ]

// `{ ... } |> Some` is an application at the top and a record underneath, and the record is what
// indents things. `walkUp` in IgnoreFile.fs is the case this came from.
[<Test>]
let ``a body piping a record into a name is reported`` () =
    let source: string =
        """module M

type T = { A: int; B: int }

let f (x: bool) : T option =
    if x then
        None
    else
        {
            A = 1
            B = 2
        }
        |> Some"""

    analyzeSource cliAnalyzer source |> assertLines [ 9 ]

[<Test>]
let ``a body piping an application into a name is not reported`` () =
    let source: string =
        """module M

let f (x: bool) (g: int -> int -> int) : int option =
    if x then
        None
    else
        g
            1
            2
        |> Some"""

    analyzeSource cliAnalyzer source |> assertLines []

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

// The same swallow, but with a statement ahead of the match, which nests the sequence holding the
// two rather than leaving the match at the top of it. This is the shape `writeDoctorFile` in
// JsonReport.fs had, and the first sweep de-indented it and moved `json.WriteEndObject()` into the
// last arm, so every doctor report came out as truncated JSON.
[<Test>]
let ``code following the match is not reported when a statement comes first`` () =
    let source: string =
        """module M

let f (x: int option) : unit =
    printfn "start"

    match x with
    | None -> ()
    | Some y ->
        let a: int = y
        printfn "%i" a

    printfn "done" """

    analyzeSource cliAnalyzer source |> assertLines []

// Not a statement after the match but an operator under it, carrying the whole match into a
// pipeline. This is the shape `genAttributesCore` in CodePrinter.fs had: `|> genNode attr` applied
// to the match, and de-indenting would have made it apply to the last arm alone, so everything
// reached through the other arm lost its trivia.
[<Test>]
let ``an operator under the match is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : string =
    match x with
    | None -> "none"
    | Some y ->
        let a: int = y
        string a
    |> sprintf "%s!" """

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a comment under the match is not reported`` () =
    let source: string =
        """module M

let f (x: int option) : int =
    match x with
    | None -> 0
    | Some y ->
        let a: int = y
        a + 1

    // this would end up inside the arm
    """

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a closing bracket on the last line of the match is still reported`` () =
    let source: string =
        """module M

let f (xs: int option list) : int list =
    xs
    |> List.map (fun x ->
        match x with
        | None -> 0
        | Some y ->
            let a: int = y
            a + 1)"""

    analyzeSource cliAnalyzer source |> assertLines [ 9 ]

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
let ``an else branch holding a block is reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
        0
    else
        let a: int = 1
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 7 ]

[<Test>]
let ``an else branch already keeping the indentation is not reported`` () =
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
let ``the final else of an elif chain is reported`` () =
    let source: string =
        """module M

let f (x: int) : string =
    if x = 1 then
        "one"
    elif x = 2 then
        "two"
    else
        let label: string = string x
        label + "!" """

    analyzeSource cliAnalyzer source |> assertLines [ 9 ]

[<Test>]
let ``an else branch beside a multiline then branch is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
        let b: int = 1
        b + 1
    else
        let a: int = 2
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an elif chain with a multiline branch is not reported`` () =
    let source: string =
        """module M

let f (x: int) : int =
    if x = 1 then
        1
    elif x = 2 then
        let b: int = 2
        b
    else
        let a: int = 3
        a + 1"""

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
let ``an else branch holding a single expression is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : string =
    if x then
        ""
    else
        "a"
        |> string
        |> String.replicate 2"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``code following the if in the same block is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : unit =
    if x then
        ()
    else
        let a: int = 1
        printfn "%i" a

    printfn "done" """

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a conditional directive inside the if is not reported`` () =
    let source: string =
        """module M

let f (x: bool) : int =
    if x then
        0
    else
#if DEBUG
        let a: int = 1
#else
        let a: int = 2
#endif
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an else branch reported once, not once per elif`` () =
    let source: string =
        """module M

let f (x: int) : int =
    if x = 1 then
        1
    elif x = 2 then
        2
    elif x = 3 then
        3
    else
        let a: int = 4
        a + 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 11 ]

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
