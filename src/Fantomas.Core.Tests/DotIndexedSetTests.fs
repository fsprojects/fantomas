module Fantomas.Core.Tests.DotIndexedSetTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``multiline expression should indent`` () =
    formatSourceString
        """
foo.Bar().[5] <- someReallyLongFunctionCall("loooooooooooooooongggStringArg",otherArg, otherReallyLongArgument)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
foo.Bar().[5] <-
    someReallyLongFunctionCall (
        "loooooooooooooooongggStringArg",
        otherArg,
        otherReallyLongArgument
    )
"""

[<Test>]
let ``multiline expression application call in set expression`` () =
    formatSourceString
        """
foo.Bar("loooooooooooooooongggStringArg",otherArg, otherReallyLongArgument).[5] <- someReallyLongFunctionCall("loooooooooooooooongggStringArg",otherArg, otherReallyLongArgument)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
foo.Bar(
    "loooooooooooooooongggStringArg",
    otherArg,
    otherReallyLongArgument
).[5] <-
    someReallyLongFunctionCall (
        "loooooooooooooooongggStringArg",
        otherArg,
        otherReallyLongArgument
    )
"""
