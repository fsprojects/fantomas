module Fantomas.Core.Tests.Stroustrup.SynTypeDefnSigReprSimpleTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup }

[<Test>]
let ``record type definition`` () =
    formatSourceString
        true
        """
namespace Foo

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
namespace Foo

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
        true
        """
namespace Foo

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
namespace Foo

type V = // comment
    {
        X: SomeFieldType
        Y: OhSomethingElse
        Z: ALongTypeName
    }
"""

[<Test>]
let ``record type definition with members`` () =
    formatSourceString
        true
        """
namespace Foo

type V =
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
    member Coordinate : SomeFieldType * OhSomethingElse * ALongTypeName
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type V = {
    X: SomeFieldType
    Y: OhSomethingElse
    Z: ALongTypeName
} with
    member Coordinate: SomeFieldType * OhSomethingElse * ALongTypeName
"""
