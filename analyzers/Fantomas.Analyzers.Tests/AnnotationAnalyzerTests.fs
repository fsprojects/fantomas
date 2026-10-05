module Fantomas.Analyzers.Tests.AnnotationAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.AnnotationAnalyzer

[<Test>]
let ``a value with no type is reported`` () =
    let source: string =
        """module M

let x = 1"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``an annotated value is not reported`` () =
    let source: string =
        """module M

let x: int = 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a missing return type is reported`` () =
    let source: string =
        """module M

let f (a: int) = a"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``an unannotated parameter is reported`` () =
    let source: string =
        """module M

let f a : int = a"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``a fully annotated function is not reported`` () =
    let source: string =
        """module M

let f (a: int) : int = a"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a unit parameter is not asked to carry a type`` () =
    let source: string =
        """module M

let f () : int = 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``every unannotated parameter is reported`` () =
    let source: string =
        """module M

let f a b : int = a + b"""

    analyzeSource cliAnalyzer source |> assertLines [ 3; 3 ]

[<Test>]
let ``a local binding is reported`` () =
    let source: string =
        """module M

let f (a: int) : int =
    let doubled = a * 2
    doubled"""

    analyzeSource cliAnalyzer source |> assertLines [ 4 ]

[<Test>]
let ``a tuple pattern is passed over`` () =
    let source: string =
        """module M

let a, b = 1, 2"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a tuple parameter annotated element by element is not reported`` () =
    let source: string =
        """module M

let f (a: int, b: string) : string = b"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a tuple parameter annotated as a whole is not reported`` () =
    let source: string =
        """module M

let f ((a, b): int * string) : string = b"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a tuple parameter with one element unannotated is reported`` () =
    let source: string =
        """module M

let f (a: int, b) : int = a"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``a struct tuple parameter annotated element by element is not reported`` () =
    let source: string =
        """module M

let f (struct (a: int, b: string)) : string = b"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a test binding is not reported`` () =
    let source: string =
        """module M

open System

type TestAttribute() =
    inherit Attribute()

[<Test>]
let someTest () =
    let x = 1
    ignore x"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a binding beside a test is still reported`` () =
    let source: string =
        """module M

open System

type TestAttribute() =
    inherit Attribute()

[<Test>]
let someTest () =
    let x = 1
    ignore x

let helper a : int = a"""

    analyzeSource cliAnalyzer source |> assertLines [ 13 ]

[<Test>]
let ``a member is not reported`` () =
    let source: string =
        """module M

type Holder(value: int) =
    member _.Value = value"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an unannotated primary constructor parameter is reported`` () =
    let source: string =
        """module M

type Holder(value) =
    member _.Value: int = value"""

    analyzeSource cliAnalyzer source |> assertLines [ 3 ]

[<Test>]
let ``every unannotated primary constructor parameter is reported`` () =
    let source: string =
        """module M

type Holder
    (
        first,
        second: int,
        third
    ) =
    member _.Sum: int = first + second + third"""

    analyzeSource cliAnalyzer source |> assertLines [ 5; 7 ]

[<Test>]
let ``an annotated primary constructor is not reported`` () =
    let source: string =
        """module M

type Holder(value: int, name: string) =
    member _.Name: string = name"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a unit primary constructor is not asked to carry a type`` () =
    let source: string =
        """module M

type Holder() =
    member _.Value: int = 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an unannotated auto property is reported`` () =
    let source: string =
        """module M

type Holder(value: int) =
    member val Value = value
    static member val Count = 0 with get, set"""

    analyzeSource cliAnalyzer source |> assertLines [ 4; 5 ]

[<Test>]
let ``an annotated auto property is not reported`` () =
    let source: string =
        """module M

type Holder(value: int) =
    member val Value: int = value"""

    analyzeSource cliAnalyzer source |> assertLines []
