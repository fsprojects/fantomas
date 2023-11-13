module Fantomas.Core.Tests.Stroustrup.KeepIndentInBranchExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

// ExperimentalKeepIndentInBranch has precedence over ExperimentalStroustrupStyle

let config =
    { config with
        ExperimentalKeepIndentInBranch = true
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

// There currently is no conflict with this setting, but I'm guessing the case was never brought up.
// I would conclude that will never clash.

[<Test>]
let ``synMatchClause in match expression with record instance `` () =
    formatSourceString
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
| _ ->
    {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``synMatchClause in match expression with update record`` () =
    formatSourceString
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
    {
        astContext with
            IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``synMatchClause in match expression with anonymous record instance`` () =
    formatSourceString
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
| _ ->
    {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``synMatchClause in match expression with anonymous record instance struct`` () =
    formatSourceString
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
| _ ->
    struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``synMatchClause in match expression with computation expression`` () =
    formatSourceString
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
| _ ->
    task {
        // some computation here
        ()
    }
"""

[<Test>]
let ``synMatchClause in match expression with list`` () =
    formatSourceString
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
| _ ->
    [
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
| _ ->
    [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]
"""
