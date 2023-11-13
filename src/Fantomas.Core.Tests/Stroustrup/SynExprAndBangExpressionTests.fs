module Fantomas.Core.Tests.Stroustrup.SynExprAndBangExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``andBang with record instance`` () =
    formatSourceString
        """
opt {
    let! abc = def ()
    and! foo =
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
    let! abc = def ()

    and! foo = {
        X = xFieldValueOne
        Y = yFieldValueTwo
        Z = zFieldValueThree
    }

    ()
}
"""

[<Test>]
let ``andBang with update record`` () =
    formatSourceString
        """
opt {
    let! abc = def ()
    and! foo =
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
    let! abc = def ()

    and! foo = {
        bar with
            X = xFieldValueOne
            Y = yFieldValueTwo
            Z = zFieldValueThree
    }

    ()
}
"""

[<Test>]
let ``andBang with anonymous record instance`` () =
    formatSourceString
        """
opt {
    let! abc = def ()
    and! foo =
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
    let! abc = def ()

    and! foo = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}

    ()
}
"""

[<Test>]
let ``andBang with anonymous record instance struct`` () =
    formatSourceString
        """
opt {
    let! abc = def ()
    and! foo =
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
    let! abc = def ()

    and! foo = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}

    ()
}
"""

[<Test>]
let ``andBang with list`` () =
    formatSourceString
        """
collect {
    let! abc = def ()
    and! items =
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
    let! abc = def ()

    and! items = [
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
let ``andBang with array`` () =
    formatSourceString
        """
collect {
    let! abc = def ()

    and! items =
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
    let! abc = def ()

    and! items = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]

    return items
}
"""
