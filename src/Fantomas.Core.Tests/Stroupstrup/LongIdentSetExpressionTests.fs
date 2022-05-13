module Fantomas.Core.Tests.Stroupstrup.LongIdentSetExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        ExperimentalStroupstrupStyle = true }

[<Test>]
let ``longIdentSet with record instance `` () =
    formatSourceString
        false
        """
myMutable <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``longIdentSet with update record`` () =
    formatSourceString
        false
        """
myMutable <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable <-
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``longIdentSet with anonymous record instance`` () =
    formatSourceString
        false
        """
myMutable <-
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable <- {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``longIdentSet with anonymous record instance struct`` () =
    formatSourceString
        false
        """
myMutable <-
   struct
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable <- struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``longIdentSet with computation expression`` () =
    formatSourceString
        false
        """
myMutable <-
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
myMutable <- task {
    // some computation here
    ()
}
"""

[<Test>]
let ``longIdentSet with list`` () =
    formatSourceString
        false
        """
myMutable <-
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
myMutable <- [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``longIdentSet with array`` () =
    formatSourceString
        false
        """
myMutable <-
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
myMutable <- [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""
