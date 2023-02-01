module Fantomas.Core.Tests.Stroustrup.SetExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``set with record instance `` () =
    formatSourceString
        false
        """
myMutable[x] <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable[x] <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``set with update record`` () =
    formatSourceString
        false
        """
myMutable[x] <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable[x] <- {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``set with anonymous record instance`` () =
    formatSourceString
        false
        """
myMutable[x] <-
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable[x] <- {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``set with anonymous record instance struct`` () =
    formatSourceString
        false
        """
myMutable[x] <-
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
myMutable[x] <- struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``set with computation expression`` () =
    formatSourceString
        false
        """
myMutable[x] <-
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
myMutable[x] <- task {
    // some computation here
    ()
}
"""

[<Test>]
let ``set with list`` () =
    formatSourceString
        false
        """
myMutable[x] <-
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
myMutable[x] <- [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``set with array`` () =
    formatSourceString
        false
        """
myMutable[x] <-
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
myMutable[x] <- [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""
