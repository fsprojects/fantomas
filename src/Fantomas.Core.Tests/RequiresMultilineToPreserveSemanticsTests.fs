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
