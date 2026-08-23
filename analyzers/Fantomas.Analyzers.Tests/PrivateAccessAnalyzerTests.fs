module Fantomas.Analyzers.Tests.PrivateAccessAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.PrivateAccessAnalyzer

[<Test>]
let ``a private value beside a signature file is reported`` () =
    let signature: string =
        """module M

val y: int"""

    let implementation: string =
        """module M

let private x = 1
let y: int = 2"""

    analyzeWithSignature cliAnalyzer signature implementation |> assertLines [ 3 ]

[<Test>]
let ``a private function beside a signature file is reported`` () =
    let signature: string =
        """module M

val y: int"""

    let implementation: string =
        """module M

let private f (a: int) : int = a
let y: int = f 2"""

    analyzeWithSignature cliAnalyzer signature implementation |> assertLines [ 3 ]

[<Test>]
let ``a private binding with no signature file is not reported`` () =
    let source: string =
        """module M

let private x = 1"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``a binding that is not private is not reported`` () =
    let signature: string =
        """module M

val y: int"""

    let implementation: string =
        """module M

let x: int = 1
let y: int = x"""

    analyzeWithSignature cliAnalyzer signature implementation |> assertLines []

[<Test>]
let ``a private binding in a nested module is reported`` () =
    let signature: string =
        """module M

module Inner =
    val z: int

val y: int"""

    let implementation: string =
        """module M

module Inner =
    let private x = 1
    let z: int = x

let y: int = Inner.z"""

    analyzeWithSignature cliAnalyzer signature implementation |> assertLines [ 4 ]
