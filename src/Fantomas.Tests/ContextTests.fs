module Fantomas.Tests.ContextTests

open NUnit.Framework
open FsUnit
open Fantomas.Context
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig
open Fantomas

[<Test>]
let ``sepSpace should not add an additional space if the line ends with a space`` () =
    let expr = !- "let a = " +> sepSpace
    let result = dump (expr Context.Default)
    result |> should equal "let a = "

[<Test>]
let ``sepColon should not add a space when nothing proceeds it``() =
    let expr =
        !-"let add a b" +> indent +> sepNln +> sepColon +> !-"int =" +> indent +> sepNln +> !-"a + b" +> unindent
        +> unindent +> sepNln
    let config = { FormatConfig.Default with SpaceBeforeColon = true }
    let ctx = { Context.Default with Config = config }
    let result = dump (expr ctx)
    result
    |> prepend newline
    |> String.normalizeNewLine
    |> should equal """
let add a b
    : int =
        a + b
"""

[<Test>]
let ``sepColon should not add a space when space proceeds it`` () =
    let expr =  !- "let a " +> sepNone +> sepColon
    let config = { FormatConfig.Default with SpaceBeforeColon = true }
    let ctx = { Context.Default with Config = config }
    let result = dump (expr ctx)
    result
    |> should equal "let a : "

[<Test>]
let ``don't add space before block comment`` () =
    let comment = """(*

Long comment

*)"""
    let expr = sepNone +> sepSpace -- comment +> sepSpace +> sepNone
    let result = dump (expr Context.Default)
    result
    |> prepend newline
    |> String.normalizeNewLine
    |> should equal """
(*

Long comment

*) """