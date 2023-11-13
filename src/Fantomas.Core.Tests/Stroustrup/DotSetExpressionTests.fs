module Fantomas.Core.Tests.Stroustrup.DotSetExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``dotSet with record instance`` () =
    formatSourceString
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
        """
App().foo <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
App().foo <- {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``dotSet with anonymous record instance`` () =
    formatSourceString
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
let ``dotSet with list`` () =
    formatSourceString
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
