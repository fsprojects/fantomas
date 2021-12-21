module Fantomas.Tests.Ragnarok.LetOrUseBangExpression

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          MultilineBlockBracketsOnSameColumn = true
          Ragnarok = true }

[<Test>]
let ``letOrUseBang with record instance`` () =
    formatSourceString
        false
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
        false
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
    let! foo =
        { bar with
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
        false
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
        false
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
let ``letOrUseBang with computation expression`` () =
    formatSourceString
        false
        """
task {
    let! meh =
        task {
            // comment
            return 42
        }
    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
task {
    let! meh = task {
        // comment
        return 42
    }

    ()
}
"""

[<Test>]
let ``letOrUseBang with list`` () =
    formatSourceString
        false
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
        false
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