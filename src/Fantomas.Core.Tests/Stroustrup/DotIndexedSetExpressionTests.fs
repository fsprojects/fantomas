module Fantomas.Core.Tests.Stroustrup.DotIndexedSetExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``dotIndexedSet with record instance `` () =
    formatSourceString
        """
myMutable.[x] <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable.[x] <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``dotIndexedSet with update record`` () =
    formatSourceString
        """
myMutable.[x] <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable.[x] <- {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``dotIndexedSet with anonymous record instance`` () =
    formatSourceString
        """
myMutable.[x] <-
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
myMutable.[x] <- {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``dotIndexedSet with anonymous record instance struct`` () =
    formatSourceString
        """
myMutable.[x] <-
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
myMutable.[x] <- struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``dotIndexedSet with list`` () =
    formatSourceString
        """
myMutable.[x] <-
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
myMutable.[x] <- [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``dotIndexedSet with array`` () =
    formatSourceString
        """
myMutable.[x] <-
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
myMutable.[x] <- [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``application unit dotIndexedSet with record instance `` () =
    formatSourceString
        """
app().[x] <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
app().[x] <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``application unit dotIndexedSet with update record`` () =
    formatSourceString
        """
app().[x] <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
app().[x] <- {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``application unit dotIndexedSet with anonymous record instance`` () =
    formatSourceString
        """
app().[x] <-
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
app().[x] <- {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``application unit dotIndexedSet with anonymous record instance struct`` () =
    formatSourceString
        """
app().[x] <-
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
app().[x] <- struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``application unit dotIndexedSet with list`` () =
    formatSourceString
        """
app().[x] <-
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
app().[x] <- [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``application unit dotIndexedSet with array`` () =
    formatSourceString
        """
app().[x] <-
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
app().[x] <- [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

// See https://github.com/fsprojects/fantomas/issues/1999

[<Test>]
let ``application parenthesis expr dotIndexedSet with record instance `` () =
    formatSourceString
        """
app(meh).[x] <-
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
app(
    meh
).[x] <- {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``application parenthesis expr dotIndexedSet with update record`` () =
    formatSourceString
        """
app(meh).[x] <-
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
app(
    meh
).[x] <- {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``application parenthesis expr dotIndexedSet with anonymous record instance`` () =
    formatSourceString
        """
app(meh).[x] <-
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
app(
    meh
).[x] <- {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``application parenthesis expr dotIndexedSet with anonymous record instance struct`` () =
    formatSourceString
        """
app(meh).[x] <-
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
app(
    meh
).[x] <- struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``application parenthesis expr dotIndexedSet with list`` () =
    formatSourceString
        """
app(meh).[x] <-
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
app(
    meh
).[x] <- [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``application parenthesis expr dotIndexedSet with array`` () =
    formatSourceString
        """
app(meh).[x] <-
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
app(
    meh
).[x] <- [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""
