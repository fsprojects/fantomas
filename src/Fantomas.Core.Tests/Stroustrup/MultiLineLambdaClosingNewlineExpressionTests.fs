module Fantomas.Core.Tests.Stroustrup.MultiLineLambdaClosingNewlineExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultiLineLambdaClosingNewline = true
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``paren lambda with record instance`` () =
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
(fun x -> {
    astContext with
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
List.map (fun x -> {
    astContext with
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
    (fun x -> {
        astContext with
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
    .Foo(fun x -> {
        other with
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

[<Test>]
let ``non stroustrup expression lambda body expression stills adds newline`` () =
    formatSourceString
        false
        """
List.map (fun e ->
    try
        f e
    with
    | ex -> "meh")
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun e ->
    try
        f e
    with ex ->
        "meh"
)
"""

[<Test>]
let ``non stroustrup expression lambda body expression stills adds newline, multiple arguments in function application``
    ()
    =
    formatSourceString
        false
        """
fn a b c (fun e ->
    try
        f e
    with
    | ex -> "meh")
"""
        config
    |> prepend newline
    |> should
        equal
        """
fn
    a
    b
    c
    (fun e ->
        try
            f e
        with ex ->
            "meh"
    )
"""

[<Test>]
let ``application with match lambda should not be affected by stroustrup`` () =
    formatSourceString
        false
        """
List.map (function
    | X x -> ()
    | Y y -> ())
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (
    function
    | X x -> ()
    | Y y -> ()
)
"""

[<Test>]
let ``non stroustrup argument in application in chain`` () =
    formatSourceString
        false
        """
Foo.Bar().Meh(fun m -> 
        try
            f e
        with ex ->
            "meh"
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo
    .Bar()
    .Meh(fun m ->
        try
            f e
        with ex ->
            "meh"
    )
"""

[<Test>]
let ``non stroustrup body in paren lambda`` () =
    formatSourceString
        false
        """
(fun m ->
        try
            f e
        with ex ->
            "meh"
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun m ->
    try
        f e
    with ex ->
        "meh"
)
"""
