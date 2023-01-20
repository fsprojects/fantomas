module Fantomas.Core.Tests.Stroustrup.DotSetExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = ExperimentalStroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``dotSet with record instance`` () =
    formatSourceString
        false
        """
App().foo <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
App().foo <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``dotSet with record instance when SpaceBeforeUppercaseInvocation = true`` () =
    formatSourceString
        false
        """
App().foo <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
App().foo <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``dotSet with update record`` () =
    formatSourceString
        false
        """
App().foo <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
App().foo <-
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``dotSet with anonymous record instance`` () =
    formatSourceString
        false
        """
App().foo <-
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
App().foo <- {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``dotSet with anonymous record instance struct`` () =
    formatSourceString
        false
        """
App().foo <-
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
App().foo <- struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``dotSet with computation expression`` () =
    formatSourceString
        false
        """
App().foo <-
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
App().foo <- task {
    // some computation here
    ()
}
"""

[<Test>]
let ``dotSet with list`` () =
    formatSourceString
        false
        """
App().foo <-
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
App().foo <- [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``dotSet with array`` () =
    formatSourceString
        false
        """
App().foo <-
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
App().foo <- [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""
