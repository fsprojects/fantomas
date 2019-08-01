module Fantomas.Tests.FormatAstTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let formatAst code =
    let inputExp =
        code |> Input
        |> toSynExprs
        |> List.head
    
    fromSynExpr inputExp
    |> function Input x -> x.TrimEnd('\r', '\n')
    |> fun s -> s.Replace("\r\n", "\n")

[<Test>]
let ``Format the ast works correctly with no source code``() =
    formatAst "()"
    |> should equal "()"
    
[<Test>]
let ``let in should not be used``() =
    formatAst "let x = 1 in ()"
    |> should equal """let x = 1
()"""

[<Test>]
let ``elif keyword is not present in raw AST`` () =
    let source = """
    if a then ()
    elif b then ()
    else ()"""
    
    formatAst source
    |> should equal """if a then ()
else if b then ()
else ()"""