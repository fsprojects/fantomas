module Fantomas.Core.Tests.Stroustrup.SynBindingFunctionWithReturnTypeExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``synbinding function with record instance `` () =
    formatSourceString
        """
let x y : MyRecord =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y : MyRecord = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding function with update record`` () =
    formatSourceString
        """
let x y : MyRecord =
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y : MyRecord = {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``synbinding function with anonymous record instance `` () =
    formatSourceString
        """
let x y : {| A:int; B:int; C:int |} =
    {| A = longTypeName
       B = someOtherVariable
       C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y : {| A: int; B: int; C: int |} = {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding function with list`` () =
    formatSourceString
        """
let x y : int list =
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
let x y : int list = [
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
        """
let x y : int array =
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
let x y : int array = [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``type member function with record instance`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar x : MyRecord =
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
    member this.Bar x : MyRecord = {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``type member function with update record`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar x : MyRecord = { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar x : MyRecord = {
        astContext with
            IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``type member function with anonymous record instance`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar x : {| A:int; B:int; C:int |} =
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
    member this.Bar x : {| A: int; B: int; C: int |} = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member function with anonymous record instance struct`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar x : {| A:int; B:int; C:int |} =
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
    member this.Bar x : {| A: int; B: int; C: int |} = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member function with list`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar x : int list =
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
    member this.Bar x : int list = [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ]
"""

[<Test>]
let ``type member function with array`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar x : int array =
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
    member this.Bar x : int array = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]
"""
