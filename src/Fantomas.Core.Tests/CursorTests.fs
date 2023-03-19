module Fantomas.Core.Tests.CursorTests

open FSharp.Compiler.Text
open NUnit.Framework
open FsUnit
open Fantomas.Core

let formatWithCursor source (line, column) =
    CodeFormatter.FormatDocumentAsync(false, source, FormatConfig.Default, CodeFormatter.MakePosition(line, column))
    |> Async.RunSynchronously

let assertCursor (expectedLine: int, expectedColumn: int) (result: FormatResult) : unit =
    match result.Cursor with
    | None -> Assert.Fail "Expected a cursor"
    | Some cursor -> Assert.AreEqual(Position.mkPos expectedLine expectedColumn, cursor)

[<Test>]
let ``cursor inside of a node`` () =
    formatWithCursor
        """
let a =
    "foobar"
"""
        (3, 8)
    |> assertCursor (1, 12)

[<Test>]
let ``cursor outside of a node`` () =
    formatWithCursor
        """
let a =
    () 
"""
        (3, 7)
    |> assertCursor (1, 11)

[<Test>]
let ``cursor inside a node between defines`` () =
    formatWithCursor
        """
#if FOO
    ()
#endif
"""
        (3, 4)
    |> assertCursor (2, 0)
