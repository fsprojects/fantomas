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
        CodeFormatter.FormatDocumentAsync(false, source, cursor = CodeFormatter.MakePosition(3, 8))
        |> Async.RunSynchronously

    // After formatting the let binding will be on one line

    match formattedResult.Cursor with
    | None -> Assert.Fail "Expected a cursor"
    | Some cursor -> assertCursor (1, 12) cursor

// [<Test>]
// let ``marker cursor`` () =
//     let source =
//         """
// let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest) (log: ILogger) =
//       ()
// """
//
//     let formatted, markers =
//         let marker = CodeFormatter.MakeRange("test.fs", 3, 4, 3, 4)
//
//         CodeFormatter.FormatDocumentAsync(false, source, marker = marker)
//         |> Async.RunSynchronously
//
//     printfn "%s" formatted
//
//     match markers with
//     | [ actualMarker ] -> assertRange (5, 4) (5, 4) actualMarker
//     | markers -> Assert.Fail $"Expected a single marker, got {markers}"
