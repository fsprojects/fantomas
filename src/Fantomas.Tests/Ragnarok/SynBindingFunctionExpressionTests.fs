module Fantomas.Tests.Ragnarok.SynBindingFunctionExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          MultilineBlockBracketsOnSameColumn = true
          Ragnarok = true }

[<Test>]
let ``synbinding function with record instance `` () =
    formatSourceString
        false
        """
let x y =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding function with anonymous record instance `` () =
    formatSourceString
        false
        """
let x y =
    {| A = longTypeName
       B = someOtherVariable
       C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding function with computation expression`` () =
    formatSourceString
        false
        """
let x y =
    task {
        // some computation here
        ()
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = task {
    // some computation here
    ()
}
"""

[<Test>]
let ``synbinding function with list`` () =
    formatSourceString
        false
        """
let x y =
    [ itemOne
      itemTwo
      itemThree
      itemFour
      itemFive ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``synbinding function with array`` () =
    formatSourceString
        false
        """
let x y =
    [| itemOne
       itemTwo
       itemThree
       itemFour
       itemFive |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``synbinding function with update record`` () =
    formatSourceString
        false
        """
let x y =
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y =
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""
