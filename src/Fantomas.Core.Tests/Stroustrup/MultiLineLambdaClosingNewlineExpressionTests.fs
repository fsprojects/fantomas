module Fantomas.Core.Tests.Stroustrup.MultiLineLambdaClosingNewlineExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultiLineLambdaClosingNewline = true
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``paren lambda with record instance`` () =
    formatSourceString
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
