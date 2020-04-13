module Fantomas.Tests.SynExprSetTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

/// See https://github.com/fsharp/fsharp/blob/master/src/fsharp/ast.fs#L633
/// F# syntax: expr <- expr
/// | Set of SynExpr * SynExpr * range:range

[<Test>]
let ``array indexer set`` () =
    formatSourceString false """
let arr = [|0|]
(arr.[0]) <- 1
"""  config
    |> should equal """let arr = [| 0 |]
(arr.[0]) <- 1
"""

[<Test>]
let ``setter of type set`` () =
    formatSourceString false """
type T() =
    member val X = 0 with get, set
(T().X) <- 1
"""  config
    |> prepend newline
    |> should equal """
type T() =
    member val X = 0 with get, set

(T().X) <- 1
"""

[<Test>]
let ``mutable value set`` () =
    formatSourceString false """
let mutable x = 0
(x) <- 1
"""  config
    |> should equal """let mutable x = 0
(x) <- 1
"""
