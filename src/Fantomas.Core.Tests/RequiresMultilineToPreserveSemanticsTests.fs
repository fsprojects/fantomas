module Fantomas.Core.Tests.RequiresMultilineToPreserveSemanticsTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

// Expr.InfixApp (single infix operator)

[<Test>]
let ``lambda on LHS of pipe operator stays multiline`` () =
    formatSourceString
        """
fun x -> x + 1
|> g
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x -> x + 1
|> g
"""

[<Test>]
let ``if-then-else on LHS of pipe operator stays multiline`` () =
    formatSourceString
        """
if x then y else z
|> g
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then y else z
|> g
"""

[<Test>]
let ``if-then-else on LHS of non-pipe infix operator stays multiline`` () =
    formatSourceString
        """
if x then y else z
+ 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then y else z
+ 1
"""

[<Test>]
let ``infix app with lambda RHS on LHS of pipe operator stays multiline`` () =
    formatSourceString
        """
x = fun y -> y
|> g
"""
        config
    |> prepend newline
    |> should
        equal
        """
x = fun y -> y
|> g
"""

[<Test>]
let ``lambda on LHS of composition operator stays multiline`` () =
    formatSourceString
        """
fun x -> x + 1
>> g
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x -> x + 1
>> g
"""

// Expr.SameInfixApps (chained same-operator expressions)

[<Test>]
let ``lambda leading chained pipe operators stays multiline`` () =
    formatSourceString
        """
fun x -> x + 1
|> g
|> h
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x -> x + 1
|> g
|> h
"""

[<Test>]
let ``if-then-else leading chained pipe operators stays multiline`` () =
    formatSourceString
        """
if x then y else z
|> g
|> h
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then y else z
|> g
|> h
"""

[<Test>]
let ``nested open-ended expression leading chained pipe operators stays multiline`` () =
    formatSourceString
        """
x = fun y -> y
|> g
|> h
"""
        config
    |> prepend newline
    |> should
        equal
        """
x = fun y -> y
|> g
|> h
"""

[<Test>]
let ``open-ended expression in middle of chained pipe stays multiline`` () =
    formatSourceString
        """
a
|> fun x -> x + 1
|> h
"""
        config
    |> prepend newline
    |> should
        equal
        """
a
|> fun x -> x + 1
|> h
"""

// Expr.Tuple (open-ended non-last element)

[<Test>]
let ``lambda as non-last tuple element stays multiline`` () =
    formatSourceString
        """
fun x -> x
, y
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun x -> x
, y
"""

[<Test>]
let ``nested open-ended as non-last tuple element stays multiline`` () =
    formatSourceString
        """
x = fun y -> y
, z
"""
        config
    |> prepend newline
    |> should
        equal
        """
x = fun y -> y
, z
"""

[<Test>]
let ``if-then-else as non-last tuple element stays multiline`` () =
    formatSourceString
        """
if a then b else c
, y
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b else c
, y
"""

[<Test>]
let ``match as non-last tuple element stays multiline`` () =
    formatSourceString
        """
match x with
| true -> 1
| false -> 2
, y
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| true -> 1
| false -> 2
, y
"""

// Expr.ArrayOrList (open-ended non-last element)

[<Test>]
let ``lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[ fun x -> x
  y ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    fun x -> x
    y
]
"""

[<Test>]
let ``if-then-else as non-last list element stays multiline`` () =
    formatSourceString
        """
[ if a then b else c
  y ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    if a then b else c
    y
]
"""

[<Test>]
let ``nested open-ended as non-last list element stays multiline`` () =
    formatSourceString
        """
[ x = fun y -> y
  z ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    x = fun y -> y
    z
]
"""

[<Test>]
let ``lambda in middle of list stays multiline`` () =
    formatSourceString
        """
[ a
  fun x -> x
  b ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    a
    fun x -> x
    b
]
"""

// Record fields (open-ended non-last field value)

[<Test>]
let ``lambda in non-last record field stays multiline`` () =
    formatSourceString
        """
{ A = 1
  B = fun x -> x
  C = 3 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{
    A = 1
    B = fun x -> x
    C = 3
}
"""

[<Test>]
let ``if-then-else in non-last record field stays multiline`` () =
    formatSourceString
        """
{ A = 1
  B = if a then b else c
  C = 3 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{
    A = 1
    B = if a then b else c
    C = 3
}
"""

[<Test>]
let ``nested open-ended in non-last record field stays multiline`` () =
    formatSourceString
        """
{ A = 1
  B = x <+> fun y -> y
  C = 3 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{
    A = 1
    B = x <+> fun y -> y
    C = 3
}
"""

// Open-ended expression forms

[<Test>]
let ``let-in in non-last record field stays multiline`` () =
    formatSourceString
        """
{ A = 1
  B = let y = 1 in y
  C = 3 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{
    A = 1
    B = let y = 1 in y
    C = 3
}
"""

[<Test>]
let ``let-in in non-last anonymous record field stays multiline`` () =
    formatSourceString
        """
{| A = 1
   B = let y = 1 in y
   C = 3 |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
{|
    A = 1
    B = let y = 1 in y
    C = 3
|}
"""

[<Test>]
let ``lazy wrapping a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    lazy fun x -> x
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    lazy fun x -> x
    2
]
"""

[<Test>]
let ``lazy wrapping a plain expression as non-last list element stays on one line`` () =
    formatSourceString
        """
[
    lazy a
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[ lazy a; 2 ]
"""

[<Test>]
let ``yield wrapping a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    yield fun x -> x
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    yield fun x -> x
    2
]
"""

[<Test>]
let ``assert wrapping a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    assert fun x -> x
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    assert fun x -> x
    2
]
"""

[<Test>]
let ``property assignment of a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    x.P <- fun y -> y
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    x.P <- fun y -> y
    2
]
"""

[<Test>]
let ``property assignment of a plain expression as non-last list element stays on one line`` () =
    formatSourceString
        """
[
    x.P <- 1
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[ x.P <- 1; 2 ]
"""

[<Test>]
let ``indexed assignment of a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    a.[0] <- fun y -> y
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    a.[0] <- fun y -> y
    2
]
"""

[<Test>]
let ``indexed assignment of a plain expression as non-last list element stays on one line`` () =
    formatSourceString
        """
[
    a.[0] <- 1
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[ a.[0] <- 1; 2 ]
"""

[<Test>]
let ``dynamic assignment of a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    x?y <- fun z -> z
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    x?y <- fun z -> z
    2
]
"""

[<Test>]
let ``named indexed property assignment of a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    a.Item(0) <- fun y -> y
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    a.Item(0) <- fun y -> y
    2
]
"""

[<Test>]
let ``dot named indexed property assignment of a lambda as non-last list element stays multiline`` () =
    formatSourceString
        """
[
    (f x).Item(0) <- fun y -> y
    2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    (f x).Item(0) <- fun y -> y
    2
]
"""

// Regression tests

[<Test>]
let ``lambda in tuple in list preserves semantics, 3278`` () =
    formatSourceString
        """
module A

let x =
    [
        1, fun () -> 1
        1, fun () -> 1
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A

let x =
    [
        1, fun () -> 1
        1, fun () -> 1
    ]
"""

[<Test>]
let ``lambda with custom operator preserves semantics, 3274`` () =
    formatSourceString
        """
let a =
    fun x -> {| X = x |}
    <*| op
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    fun x -> {| X = x |}
    <*| op
"""

[<Test>]
let ``constructor with 3 args and no open-ended elements stays on one line`` () =
    formatSourceString
        """
Foo.Bar(Title = "hello", Url = "world", Count = 3)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo.Bar(Title = "hello", Url = "world", Count = 3)
"""

[<Test>]
let ``constructor with 4 args and no open-ended elements stays on one line`` () =
    formatSourceString
        """
Foo.Bar(Title = "hello", Url = "world", Count = 3, Extra = "more")
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo.Bar(Title = "hello", Url = "world", Count = 3, Extra = "more")
"""

[<Test>]
let ``constructor with 3 args and if-then-else in first uses comma-leading`` () =
    formatSourceString
        """
Foo.Bar(
    Title = if true then Some "" else None
    , Url = "world"
    , Count = 3
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo.Bar(
    Title = if true then Some "" else None
    , Url = "world"
    , Count = 3
)
"""

[<Test>]
let ``constructor with 3 args and lambda in middle uses comma-leading`` () =
    formatSourceString
        """
Foo.Bar(
    Title = "hello"
    , Url = fun x -> x
    , Count = 3
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo.Bar(
    Title = "hello"
    , Url = fun x -> x
    , Count = 3
)
"""

[<Test>]
let ``3-element tuple with lambda in last position stays on one line`` () =
    formatSourceString
        """
let x = 1, 2, fun y -> y
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = 1, 2, fun y -> y
"""

[<Test>]
let ``3-element tuple with lambda in first position uses comma-leading`` () =
    formatSourceString
        """
let x =
    fun y -> y
    , 2
    , 3
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    fun y -> y
    , 2
    , 3
"""

[<Test>]
let ``3-element tuple with lambda in middle uses comma-leading`` () =
    formatSourceString
        """
let x =
    1
    , fun y -> y
    , 3
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    1
    , fun y -> y
    , 3
"""
