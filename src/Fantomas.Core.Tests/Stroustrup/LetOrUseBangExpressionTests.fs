module Fantomas.Core.Tests.Stroustrup.LetOrUseBangExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``letOrUseBang with record instance`` () =
    formatSourceString
        """
opt {
    let! foo =
        { X = xFieldValueOne
          Y = yFieldValueTwo
          Z = zFieldValueThree }

    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
opt {
    let! foo = {
        X = xFieldValueOne
        Y = yFieldValueTwo
        Z = zFieldValueThree
    }

    ()
}
"""

[<Test>]
let ``letOrUseBang with update record`` () =
    formatSourceString
        """
opt {
    let! foo =
        { bar with X = xFieldValueOne
                   Y = yFieldValueTwo
                   Z = zFieldValueThree }

    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
opt {
    let! foo = {
        bar with
            X = xFieldValueOne
            Y = yFieldValueTwo
            Z = zFieldValueThree
    }

    ()
}
"""

[<Test>]
let ``letOrUseBang with anonymous record instance`` () =
    formatSourceString
        """
opt {
    let! foo =
       {| A = longTypeName
          B = someOtherVariable
          C = ziggyBarX |}

    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
opt {
    let! foo = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}

    ()
}
"""

[<Test>]
let ``letOrUseBang with anonymous record instance struct`` () =
    formatSourceString
        """
opt {
    let! foo =
       struct {| A = longTypeName
                 B = someOtherVariable
                 C = ziggyBarX |}

    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
opt {
    let! foo = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}

    ()
}
"""

[<Test>]
let ``letOrUseBang with list`` () =
    formatSourceString
        """
collect {
    let! items =
        [ itemOne
          itemTwo
          itemThree
          itemFour
          itemFive ]
    return items
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
collect {
    let! items = [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ]

    return items
}
"""

[<Test>]
let ``letOrUseBang with array`` () =
    formatSourceString
        """
collect {
    let! items =
        [|  itemOne
            itemTwo
            itemThree
            itemFour
            itemFive    |]
    return items
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
collect {
    let! items = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]

    return items
}
"""
