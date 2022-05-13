module Fantomas.Core.Tests.Stroupstrup.SynTypeDefnSimpleReprRecordTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        ExperimentalStroupstrupStyle = true }

[<Test>]
let ``record type definition`` () =
    formatSourceString
        false
        """
type V =
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V = {
    X: SomeFieldType
    Y: OhSomethingElse
    Z: ALongTypeName
}
"""

[<Test>]
[<Ignore("See https://github.com/fsprojects/fantomas/issues/2001, this will be easier to fix in future FCS version")>]
let ``record type definition with comment after equals`` () =
    formatSourceString
        false
        """
type V = // comment
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V = // comment
    {
        X: SomeFieldType
        Y: OhSomethingElse
        Z: ALongTypeName
    }
"""

// TODO: I feel like ragnarok should not work when there are members involved
// Having members would require the `with` keyword which is not recommended by the style guide: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-record-declarations

[<Test>]
let ``record type definition with members`` () =
    formatSourceString
        false
        """
type V =
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
    member this.Coordinate = (this.X, this.Y, this.Z)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V =
    {
        X: SomeFieldType
        Y: OhSomethingElse
        Z: ALongTypeName
    }
    member this.Coordinate = (this.X, this.Y, this.Z)
"""
