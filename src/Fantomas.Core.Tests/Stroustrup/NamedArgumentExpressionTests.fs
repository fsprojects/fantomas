module Fantomas.Core.Tests.Stroustrup.NamedArgumentExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``synExprApp with named argument with record instance`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        v =
            { A = longTypeName
              B = someOtherVariable
              C = ziggyBarX }
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        v = {
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        }
    )
"""

[<Test>]
let ``synExprApp with named argument with update record`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        v =
            { astContext with IsInsideMatchClausePattern = true
                              A = longTypeName
                              B = someOtherVariable
                              C = ziggyBarX }
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        v = {
            astContext with
                IsInsideMatchClausePattern = true
                A = longTypeName
                B = someOtherVariable
                C = ziggyBarX
        }
    )
"""

[<Test>]
let ``synExprApp with named argument with anonymous record instance`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        v =
            {| A = longTypeName
               B = someOtherVariable
               C = ziggyBarX |}
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        v = {|
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        |}
    )
"""

[<Test>]
let ``synExprApp with named argument with anonymous record instance struct`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        v =
            struct {| A = longTypeName
                      B = someOtherVariable
                      C = ziggyBarX |}
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        v = struct {|
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        |}
    )
"""

[<Test>]
let ``synExprApp with named argument with list`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        v =
                [ itemOne
                  itemTwo
                  itemThree
                  itemFour
                  itemFive ]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        v = [
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        ]
    )
"""

[<Test>]
let ``synExprApp with named argument with array`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        v =
                [| itemOne
                   itemTwo
                   itemThree
                   itemFour
                   itemFive |]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        v = [|
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        |]
    )
"""

[<Test>]
let ``synExprApp with multiple named arguments`` () =
    formatSourceString
        false
        """
let v =
    SomeConstructor(
        x =
                [| itemOne
                   itemTwo
                   itemThree
                   itemFour
                   itemFive |],
        y =
                [
                    itemOne
                    itemTwo
                    itemThree
                    itemFour
                    itemFive
                ]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    SomeConstructor(
        x = [|
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        |],
        y = [
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        ]
    )
"""

[<Test>]
let ``synExprNew with named argument with record instance`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        v =
            { A = longTypeName
              B = someOtherVariable
              C = ziggyBarX }
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        v = {
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        }
    )
"""

[<Test>]
let ``synExprNew with named argument with update record`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        v =
            { astContext with IsInsideMatchClausePattern = true
                              A = longTypeName
                              B = someOtherVariable
                              C = ziggyBarX }
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        v = {
            astContext with
                IsInsideMatchClausePattern = true
                A = longTypeName
                B = someOtherVariable
                C = ziggyBarX
        }
    )
"""

[<Test>]
let ``synExprNew with named argument with anonymous record instance`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        v =
            {| A = longTypeName
               B = someOtherVariable
               C = ziggyBarX |}
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        v = {|
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        |}
    )
"""

[<Test>]
let ``synExprNew with named argument with anonymous record instance struct`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        v =
            struct {| A = longTypeName
                      B = someOtherVariable
                      C = ziggyBarX |}
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        v = struct {|
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        |}
    )
"""

[<Test>]
let ``synExprNew with named argument with list`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        v =
                [ itemOne
                  itemTwo
                  itemThree
                  itemFour
                  itemFive ]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        v = [
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        ]
    )
"""

[<Test>]
let ``synExprNew with named argument with array`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        v =
                [| itemOne
                   itemTwo
                   itemThree
                   itemFour
                   itemFive |]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        v = [|
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        |]
    )
"""

[<Test>]
let ``synExprNew with multiple named arguments`` () =
    formatSourceString
        false
        """
let v =
    new FooBar(
        x =
                [| itemOne
                   itemTwo
                   itemThree
                   itemFour
                   itemFive |],
        y =
                [
                    itemOne
                    itemTwo
                    itemThree
                    itemFour
                    itemFive
                ]
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v =
    new FooBar(
        x = [|
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        |],
        y = [
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        ]
    )
"""
