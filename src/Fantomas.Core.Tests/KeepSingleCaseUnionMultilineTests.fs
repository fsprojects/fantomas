module Fantomas.Core.Tests.KeepSingleCaseUnionMultilineTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        KeepSingleCaseUnionMultiline = true }

[<Test>]
let ``single case union with field is kept multiline`` () =
    formatSourceString
        """
type MyDU = Short of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyDU =
    | Short of int
"""

[<Test>]
let ``single case union without field is kept multiline`` () =
    formatSourceString
        """
type A = | A
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    | A
"""

[<Test>]
let ``single case union with access modifier is kept multiline`` () =
    formatSourceString
        """
type Foo = private Foo of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo =
    private | Foo of int
"""

[<Test>]
let ``result is idempotent`` () =
    let source =
        """
type MyDU =
    | Short of int
"""

    formatSourceString source config
    |> fun formatted -> formatSourceString formatted config
    |> prepend newline
    |> should
        equal
        """
type MyDU =
    | Short of int
"""

[<Test>]
let ``combines with bar before discriminated union declaration`` () =
    formatSourceString
        """
type MyDU = Short of int
"""
        { config with
            BarBeforeDiscriminatedUnionDeclaration = true }
    |> prepend newline
    |> should
        equal
        """
type MyDU =
    | Short of int
"""

[<Test>]
let ``multi case union is unaffected`` () =
    formatSourceString
        """
type MyDU =
    | Short of int
    | Long of string
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyDU =
    | Short of int
    | Long of string
"""

[<Test>]
let ``single case union with member stays multiline`` () =
    formatSourceString
        """
type MyDU =
    | Short of int
    member this.Value = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyDU =
    | Short of int

    member this.Value = 1
"""

[<Test>]
let ``single case union in signature file is kept multiline`` () =
    formatSignatureString
        """namespace meh

type Foo = Bar of int
"""
        config
    |> should
        equal
        """namespace meh

type Foo =
    | Bar of int
"""
