module Fantomas.Core.Tests.CursorTests

open FSharp.Compiler.Text
open NUnit.Framework
open FsUnit
open Fantomas.Core

let assertCursor (expectedLine: int, expectedColumn: int) (actualCursor: pos) : unit =
    Assert.AreEqual(Position.mkPos expectedLine expectedColumn, actualCursor)

[<Test>]
let ``cursor inside of a node`` () =
    let source =
        """
let a =
    "foobar"
"""

    let formattedResult =
        CodeFormatter.FormatDocumentAsync(false, source, CodeFormatter.MakePosition(3, 8), FormatConfig.Default)
        |> Async.RunSynchronously

    // After formatting the let binding will be on one line

    match formattedResult.Cursor with
    | None -> Assert.Fail "Expected a cursor"
    | Some cursor -> assertCursor (1, 12) cursor

[<Test>]
let ``cursor outside of a node`` () =
    let source =
        """
let a =
    () 
"""

    let formattedResult =
        CodeFormatter.FormatDocumentAsync(false, source, CodeFormatter.MakePosition(3, 7), FormatConfig.Default)
        |> Async.RunSynchronously

    match formattedResult.Cursor with
    | None -> Assert.Fail "Expected a cursor"
    | Some cursor -> assertCursor (1, 11) cursor
