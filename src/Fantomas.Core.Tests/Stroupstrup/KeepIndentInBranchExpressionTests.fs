module Fantomas.Core.Tests.Stroupstrup.KeepIndentInBranchExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        KeepIndentInBranch = true
        ExperimentalStroupstrupStyle = true }

// There currently is no conflict with this setting, but I'm guessing the case was never brought up.
// I would conclude that will never clash.

[<Test>]
let ``synMatchClause in match expression with record instance `` () =
    formatSourceString
        false
        """
match x with
| _ ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ -> {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synMatchClause in match expression with update record`` () =
    formatSourceString
        false
        """
match x with
| _ ->
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ ->
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``synMatchClause in match expression with anonymous record instance`` () =
    formatSourceString
        false
        """
match x with
| _ ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ -> {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synMatchClause in match expression with anonymous record instance struct`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synMatchClause in match expression with computation expression`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> task {
    // some computation here
    ()
}
"""

[<Test>]
let ``synMatchClause in match expression with list`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``synMatchClause in match expression with array`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""
