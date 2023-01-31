module Fantomas.Core.Tests.Stroustrup.DotIndexedSetExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = ExperimentalStroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``dotIndexedSet with record instance `` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
let ``dotIndexedSet with computation expression`` () =
    formatSourceString
        false
        """
myMutable.[x] <-
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
myMutable.[x] <- task {
    // some computation here
    ()
}
"""

[<Test>]
let ``dotIndexedSet with list`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
let ``application unit dotIndexedSet with computation expression`` () =
    formatSourceString
        false
        """
app().[x] <-
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
app().[x] <- task {
    // some computation here
    ()
}
"""

[<Test>]
let ``application unit dotIndexedSet with list`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
let ``application parenthesis expr dotIndexedSet with computation expression`` () =
    formatSourceString
        false
        """
app(meh).[x] <-
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
app(
    meh
).[x] <- task {
    // some computation here
    ()
}
"""

[<Test>]
let ``application parenthesis expr dotIndexedSet with list`` () =
    formatSourceString
        false
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
        false
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
