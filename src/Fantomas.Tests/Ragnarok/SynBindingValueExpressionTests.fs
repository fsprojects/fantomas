module Fantomas.Tests.Ragnarok.SynBindingValueExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        Ragnarok = true }

[<Test>]
let ``synbinding value with record instance `` () =
    formatSourceString
        false
        """
let x =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding value with update record`` () =
    formatSourceString
        false
        """
let astCtx =
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let astCtx =
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``synbinding value with anonymous record instance`` () =
    formatSourceString
        false
        """
let x =
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding value with anonymous record instance struct`` () =
    formatSourceString
        false
        """
let x =
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
let x = struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding value with computation expression`` () =
    formatSourceString
        false
        """
let t =
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
let t = task {
    // some computation here
    ()
}
"""

[<Test>]
let ``synbinding value with list`` () =
    formatSourceString
        false
        """
let t =
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
let t = [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``synbinding value with array`` () =
    formatSourceString
        false
        """
let t =
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
let t = [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``nested synbinding value with record`` () =
    formatSourceString
        false
        """
let outer =
    let inner =
        {
            X = someGreatXValue
            Y = someRatherSmallYValue
        }
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let outer =
    let inner = {
        X = someGreatXValue
        Y = someRatherSmallYValue
    }

    ()
"""

[<Test>]
let ``type member value with record instance`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar =
        { A = longTypeName
          B = someOtherVariable
          C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar = {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``type member value with update record`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar = { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar =
        { astContext with
            IsInsideMatchClausePattern = true
        }
"""

[<Test>]
let ``type member value with anonymous record instance`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar =
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member value with anonymous record instance struct`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar =
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
type Foo() =
    member this.Bar = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member value with computation expression`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar =
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
type Foo() =
    member this.Bar = task {
        // some computation here
        ()
    }
"""

[<Test>]
let ``type member value with list`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar =
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
type Foo() =
    member this.Bar = [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ]
"""

[<Test>]
let ``type member value with array`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar =
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
type Foo() =
    member this.Bar = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]
"""
