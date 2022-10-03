module Fantomas.Core.Tests.TypeAnnotationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``multiline type annotation`` () =
    formatSourceString
        false
        """
let f
    (x:
        {|
            x: int
            y: AReallyLongTypeThatIsMuchLongerThan40Characters
        |})
    =
    x
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f
    (x:
        {| x: int
           y: AReallyLongTypeThatIsMuchLongerThan40Characters |})
    =
    x
"""

[<Test>]
let ``multiline tuple type`` () =
    formatSourceString
        false
        """
type Meh
    (
        input: LongTupleItemTypeOneThing * LongTupleItemTypeThingTwo * LongTupleItemTypeThree * LongThingFour * LongThingFiveYow
    ) =
    class
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh
    (
        input:
            LongTupleItemTypeOneThing *
            LongTupleItemTypeThingTwo *
            LongTupleItemTypeThree *
            LongThingFour *
            LongThingFiveYow
    ) =
    class
    end
"""
