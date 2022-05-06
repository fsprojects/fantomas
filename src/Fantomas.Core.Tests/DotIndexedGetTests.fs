module Fantomas.Core.Tests.DotIndexedGetTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

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

[<Test>]
let ``should not merge tokens inside parentheses, 1407`` () =
    formatSourceString
        false
        """
let inline (=??) x = (=!) x
let mySampleMethod() =
    let result = Ok {| Results = [] |}
    (Result.okValue result).Results.[0] |> Result.isOk =?? true
"""
        { config with
            SpaceBeforeLowercaseInvocation = false
            SpaceBeforeColon = true
            MaxIfThenElseShortWidth = 25
            MultilineBlockBracketsOnSameColumn = true
            KeepIfThenInSameLine = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
let inline (=??) x = (=!) x

let mySampleMethod () =
    let result = Ok {| Results = [] |}

    (Result.okValue result).Results.[0] |> Result.isOk
    =?? true
"""

[<Test>]
let ``multiple indexed application with unit`` () =
    formatSourceString
        false
        """
a.Some.Thing(
    "aaa",
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc").Meh().[0]
"""
        config
    |> prepend newline
    |> should
        equal
        """
a
    .Some
    .Thing(
        "aaa",
        "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    )
    .Meh().[0]
"""

[<Test>]
let ``multiline parentheses objectExpression in indexer expression, 2176`` () =
    formatSourceString
        false
        """
namespace FSX.Infrastructure

module Unix =

    let GrabTheFirstStringBeforeTheFirstColon (lines: seq<string>) =
        seq {
            for line in lines do
                yield
                    (line.Split(
                        [| ":" |],
                        StringSplitOptions.RemoveEmptyEntries
                    )).[0]
        }
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
namespace FSX.Infrastructure

module Unix =

    let GrabTheFirstStringBeforeTheFirstColon (lines: seq<string>) =
        seq {
            for line in lines do
                yield
                    (line.Split(
                        [| ":" |],
                        StringSplitOptions.RemoveEmptyEntries
                    )).[0]
        }
"""
