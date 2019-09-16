module Fantomas.Tests.FormatAstTests

open Fantomas
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

/// There is no dead code in this test
/// The trivia (newline on line 2) is kept in tact after formatting

[<Test>]
let ``editor format with existing AST and source code`` () =
    let source = """let a =   42

let b =   1""" |> SourceOrigin.SourceString
    let fileName = "/tmp.fsx"
    let ast =
        CodeFormatter.ParseAsync(fileName, source, sharedChecker.Value)
        |> Async.RunSynchronously
        |> Seq.head
        |> fst

    let formattedCode =
        CodeFormatter.FormatASTAsync(ast, fileName, Some source, config)
        |> Async.RunSynchronously
        |> String.normalizeNewLine

    formattedCode
    |> prepend newline
    |> should equal """
let a = 42

let b = 1
"""