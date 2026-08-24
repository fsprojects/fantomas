module Fantomas.Analyzers.Tests.XmlDocAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.XmlDocAnalyzer

[<Test>]
let ``a doc comment beside a signature file is reported`` () =
    let signature: string =
        """module M

/// The answer.
val y: int"""

    let implementation: string =
        """module M

/// The answer.
let y: int = 42"""

    analyzeWithSignature cliAnalyzer signature implementation |> assertLines [ 3 ]

[<Test>]
let ``a doc comment with no signature file is not reported`` () =
    let source: string =
        """module M

/// The answer.
let y: int = 42"""

    analyzeSource cliAnalyzer source |> assertLines []

[<Test>]
let ``an ordinary comment is not reported`` () =
    let signature: string =
        """module M

val y: int"""

    let implementation: string =
        """module M

// The answer.
let y: int = 42"""

    analyzeWithSignature cliAnalyzer signature implementation |> assertLines []

[<Test>]
let ``a doc comment on a type is reported`` () =
    let signature: string =
        """module M

/// Holds a number.
type Holder =
    /// The number.
    | Number of int

val y: Holder"""

    let implementation: string =
        """module M

/// Holds a number.
type Holder =
    /// The number.
    | Number of int

let y: Holder = Number 1"""

    analyzeWithSignature cliAnalyzer signature implementation
    |> assertLines [ 3; 5 ]
