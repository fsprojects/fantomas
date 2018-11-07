module Fantomas.Tests.FormatAstTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``Format the ast works correctly with no source code``() =
    let inputExp =
        "()" |> Input
        |> toSynExprs
        |> List.head
    
    fromSynExpr inputExp
    |> function Input x -> x.TrimEnd('\r', '\n')
    |> should equal "()"