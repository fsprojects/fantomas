module Fantomas.Tests.DotIndexedGetTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``multiline function application inside DotIndexedGet`` () =
    formatSourceString
        false
        """
foo.Bar(
            ziggy,
            jiggy,
            "looooooooooooooooooooooooooooooooonStringValue"
        ).[5]
"""
        { config with MaxLineLength = 70 }
    |> prepend newline
    |> should
        equal
        """
foo.Bar(
    ziggy,
    jiggy,
    "looooooooooooooooooooooooooooooooonStringValue"
).[5]
"""

[<Test>]
let ``multiline function application after indexer`` () =
    formatSourceString
        false
        """
myList.[7].SomeFunctionCallOnSeven("looooooooooooooooooooooooooooooooonnggggStringArgument", otherArg1, otherArg2, otherArg3, otherArgument4)
"""
        config
    |> prepend newline
    |> should
        equal
        """
myList.[7]
    .SomeFunctionCallOnSeven(
        "looooooooooooooooooooooooooooooooonnggggStringArgument",
        otherArg1,
        otherArg2,
        otherArg3,
        otherArgument4
    )
"""

[<Test>]
let ``multiline lowercased function application after indexer`` () =
    formatSourceString
        false
        """
myList.[7].lowerSomeFunctionCallOnSeven("looooooooooooooooooooooooooooooooonnggggStringArgument", otherArg1, otherArg2, otherArg3, otherArgument4)
"""
        config
    |> prepend newline
    |> should
        equal
        """
myList.[7]
    .lowerSomeFunctionCallOnSeven (
        "looooooooooooooooooooooooooooooooonnggggStringArgument",
        otherArg1,
        otherArg2,
        otherArg3,
        otherArgument4
    )
"""
