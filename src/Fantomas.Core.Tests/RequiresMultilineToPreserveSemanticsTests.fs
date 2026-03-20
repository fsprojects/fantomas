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
