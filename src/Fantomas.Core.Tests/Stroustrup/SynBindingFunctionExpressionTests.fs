module Fantomas.Core.Tests.Stroustrup.SynBindingFunctionExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.FormatConfig
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        MultilineBracketStyle = ExperimentalStroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``synbinding function with record instance `` () =
    formatSourceString
        false
        """
let x y =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding function with update record`` () =
    formatSourceString
        false
        """
let x y =
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y =
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``synbinding function with anonymous record instance `` () =
    formatSourceString
        false
        """
let x y =
    {| A = longTypeName
       B = someOtherVariable
       C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding function with computation expression`` () =
    formatSourceString
        false
        """
let x y =
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
let x y = task {
    // some computation here
    ()
}
"""

[<Test>]
let ``synbinding function with list`` () =
    formatSourceString
        false
        """
let x y =
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
let x y = [
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
        false
        """
let x y =
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
let x y = [|
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
        false
        """
type Foo() =
    member this.Bar x =
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
    member this.Bar x = {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``type member function with update record`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar x = { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar x =
        { astContext with
            IsInsideMatchClausePattern = true
        }
"""

[<Test>]
let ``type member function with anonymous record instance`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar x =
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
    member this.Bar x = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member function with anonymous record instance struct`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar x =
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
    member this.Bar x = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member function with computation expression`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar x =
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
    member this.Bar x = task {
        // some computation here
        ()
    }
"""

[<Test>]
let ``type member function with list`` () =
    formatSourceString
        false
        """
type Foo() =
    member this.Bar x =
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
    member this.Bar x = [
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
        false
        """
type Foo() =
    member this.Bar x =
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
    member this.Bar x = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]
"""

[<Test>]
let ``doesn't trigger stroustrup styling when an expression has trivia before it`` () =
    formatSourceString
        false
        """
let inline skipNoFail count (source: seq<_>) =
//if FABLE_COMPILER
    seq {
      yield "Hello"
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline skipNoFail count (source: seq<_>) =
    //if FABLE_COMPILER
    seq { yield "Hello" }
"""

[<Test>]
let ``conditional directives before expression, 2517`` () =
    formatSourceString
        false
        """
    let inline skipNoFail count (source: seq<_>) =
#if FABLE_COMPILER
        seq {
          let mutable i = 0
          let e = source.GetEnumerator ()
          while e.MoveNext () do
            if i < count
            then
              i <- i + 1
            else
              yield e.Current
        }
#else
        Enumerable.Skip(source, count)
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline skipNoFail count (source: seq<_>) =
#if FABLE_COMPILER
    seq {
        let mutable i = 0
        let e = source.GetEnumerator()

        while e.MoveNext() do
            if i < count then i <- i + 1 else yield e.Current
    }
#else
    Enumerable.Skip(source, count)
#endif
"""
