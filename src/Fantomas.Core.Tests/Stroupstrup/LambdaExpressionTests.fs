module Fantomas.Core.Tests.Stroupstrup.LambdaExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        ExperimentalStroustrupStyle = true }

[<Test>]
let ``lambda with record instance `` () =
    formatSourceString
        false
        """
fun x ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x -> {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``lambda with update record`` () =
    formatSourceString
        false
        """
fun x ->
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x ->
    { astContext with
        IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``lambda with anonymous record instance`` () =
    formatSourceString
        false
        """
fun x ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x -> {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``lambda with anonymous record instance struct`` () =
    formatSourceString
        false
        """
fun x ->
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
fun x -> struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``lambda with computation expression`` () =
    formatSourceString
        false
        """
fun x ->
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
fun x -> task {
    // some computation here
    ()
}
"""

[<Test>]
let ``lambda with list`` () =
    formatSourceString
        false
        """
fun x ->
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
fun x -> [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``lambda with array`` () =
    formatSourceString
        false
        """
fun x ->
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
fun x -> [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``paren lambda with record instance `` () =
    formatSourceString
        false
        """
(fun x ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX })
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
})
"""

[<Test>]
let ``paren lambda with update record`` () =
    formatSourceString
        false
        """
(fun x ->
    { astContext with IsInsideMatchClausePattern = true })
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x ->
    { astContext with
        IsInsideMatchClausePattern = true
    })
"""

[<Test>]
let ``paren lambda with anonymous record instance`` () =
    formatSourceString
        false
        """
(fun x ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |})
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|})
"""

[<Test>]
let ``paren lambda with anonymous record instance struct`` () =
    formatSourceString
        false
        """
(fun x ->
   struct
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |})
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|})
"""

[<Test>]
let ``paren lambda with computation expression`` () =
    formatSourceString
        false
        """
(fun x ->
    task {
        // some computation here
        ()
    })
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> task {
    // some computation here
    ()
})
"""

[<Test>]
let ``paren lambda with list`` () =
    formatSourceString
        false
        """
(fun x ->
    [ itemOne
      itemTwo
      itemThree
      itemFour
      itemFive ])
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
])
"""

[<Test>]
let ``paren lambda with array`` () =
    formatSourceString
        false
        """
(fun x ->
    [| itemOne
       itemTwo
       itemThree
       itemFour
       itemFive |])
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|])
"""

[<Test>]
let ``app paren lambda with record instance `` () =
    formatSourceString
        false
        """
List.map (fun x ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX })
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x -> {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
})
"""

[<Test>]
let ``app paren lambda with update record`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    { astContext with IsInsideMatchClausePattern = true })
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x ->
    { astContext with
        IsInsideMatchClausePattern = true
    })
"""

[<Test>]
let ``app paren lambda with anonymous record instance`` () =
    formatSourceString
        false
        """
List.map (fun x ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |})
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x -> {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|})
"""

[<Test>]
let ``app paren lambda with anonymous record instance struct`` () =
    formatSourceString
        false
        """
List.map (fun x ->
   struct
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |})
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x -> struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|})
"""

[<Test>]
let ``app paren lambda with computation expression`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    task {
        // some computation here
        ()
    })
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x -> task {
    // some computation here
    ()
})
"""

[<Test>]
let ``app paren lambda with list`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    [ itemOne
      itemTwo
      itemThree
      itemFour
      itemFive ])
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x -> [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
])
"""

[<Test>]
let ``app paren lambda with array`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    [| itemOne
       itemTwo
       itemThree
       itemFour
       itemFive |])
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun x -> [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|])
"""

[<Test>]
let ``app paren lambda with record instance and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }) b c
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x -> {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    })
    b
    c
"""

[<Test>]
let ``app paren lambda with update record and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    { astContext with IsInsideMatchClausePattern = true }) b c
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x ->
        { astContext with
            IsInsideMatchClausePattern = true
        })
    b
    c
"""

[<Test>]
let ``app paren lambda with anonymous record instance and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}) b c
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x -> {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |})
    b
    c
"""

[<Test>]
let ``app paren lambda with anonymous record instance struct and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
   struct
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |}) b c
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x -> struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |})
    b
    c
"""

[<Test>]
let ``app paren lambda with computation expression and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    task {
        // some computation here
        ()
    }) b c
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x -> task {
        // some computation here
        ()
    })
    b
    c
"""

[<Test>]
let ``app paren lambda with list and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    [ itemOne
      itemTwo
      itemThree
      itemFour
      itemFive ]) b c
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x -> [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ])
    b
    c
"""

[<Test>]
let ``app paren lambda with array and other args`` () =
    formatSourceString
        false
        """
List.map (fun x ->
    [| itemOne
       itemTwo
       itemThree
       itemFour
       itemFive |]) b c
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map
    (fun x -> [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |])
    b
    c
"""

[<Test>]
let ``dotGetApp with lambda with record instance`` () =
    formatSourceString
        false
        """
Bar.Foo(fun x -> {  A = longTypeName
                    B = someOtherVariable
                    C = ziggyBarX
                    D = evenMoreZigBarry }).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x -> {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
        D = evenMoreZigBarry
    })
    .Bar()
"""

[<Test>]
let ``dotGetApp with lambda with update record`` () =
    formatSourceString
        false
        """
Bar.Foo(fun x -> {  other with
                                A = longTypeName
                                B = someOtherVariable
                                C = ziggyBarX
                                D = evenMoreZigBarry }).Bar()
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x ->
        { other with
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
            D = evenMoreZigBarry
        })
    .Bar()
"""

[<Test>]
let ``dotGetApp with lambda with anonymous record instance`` () =
    formatSourceString
        false
        """
Bar.Foo(fun x ->
                   {| A = longTypeName
                      B = someOtherVariable
                      C = ziggyBarX |}).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x -> {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |})
    .Bar()
"""

[<Test>]
let ``dotGetApp with lambda with anonymous record instance struct`` () =
    formatSourceString
        false
        """
Bar
    .Foo(fun x ->
               struct
                    {| A = longTypeName
                       B = someOtherVariable
                       C = ziggyBarX |}).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x -> struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |})
    .Bar()
"""

[<Test>]
let ``dotGetApp with lambda with computation expression`` () =
    formatSourceString
        false
        """
Bar
    .Foo(fun x ->
                    task {
                        // some computation here
                        ()
                    }).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x -> task {
        // some computation here
        ()
    })
    .Bar()
"""

[<Test>]
let ``dotGetApp with lambda with list`` () =
    formatSourceString
        false
        """
Bar
    .Foo(fun x ->
                    [ itemOne
                      itemTwo
                      itemThree
                      itemFour
                      itemFive ]).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x -> [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ])
    .Bar()
"""

[<Test>]
let ``dotGetApp with lambda with array`` () =
    formatSourceString
        false
        """
Bar
    .Foo(fun x ->
                    [| itemOne
                       itemTwo
                       itemThree
                       itemFour
                       itemFive |]).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Bar
    .Foo(fun x -> [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |])
    .Bar()
"""
