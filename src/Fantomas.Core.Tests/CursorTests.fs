module Fantomas.Core.Tests.CursorTests

open Fantomas.FCS.Text
open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

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

[<Test>]
let ``cursor after try keyword`` () =
    formatWithCursor
        """
namespace JetBrains.ReSharper.Plugins.FSharp.Services.Formatter

[<CodeCleanupModule>]
type FSharpReformatCode(textControlManager: ITextControlManager) =
        member x.Process(sourceFile, rangeMarker, _, _, _) =
            if isNotNull rangeMarker then
                try
                    let range = ofDocumentRange rangeMarker.DocumentRange
                    let formatted = fantomasHost.FormatSelection(filePath, range, text, settings, parsingOptions, newLineText)
                    let offset = rangeMarker.DocumentRange.StartOffset.Offset
                    let oldLength = rangeMarker.DocumentRange.Length
                    let documentChange = DocumentChange(document, offset, oldLength, formatted, stamp, modificationSide)
                    use _ = WriteLockCookie.Create()
                    document.ChangeDocument(documentChange, TimeStamp.NextValue)
                    sourceFile.GetPsiServices().Files.CommitAllDocuments()
                with _ -> ()
            else
                let textControl = textControlManager.VisibleTextControls |> Seq.find (fun c -> c.Document == document)
                cursorPosition = textControl.Caret.Position.Value.ToDocLineColumn();
"""
        (8, 19)
    |> assertCursor (7, 15)

[<Test>]
let ``cursor should not be considered as content before, 3007`` () =
    let result =
        formatWithCursor
            """pipeline "init" {
    stage "restore-sln" {
        parallel
        run "dotnet tool restore"
    }
}
"""
            (4, 0)

    result |> assertCursor (4, 0)

let assertKeywordCursors
    (source: string)
    (config: FormatConfig)
    (expectedCode: string)
    (positions: ((int * int) * (int * int)) list)
    : unit
    =
    let baseline: string = formatSourceString source config
    baseline |> prepend newline |> should equal expectedCode

    for (line, column), expectedCursor in positions do
        let result: FormatResult =
            CodeFormatter.FormatDocumentAsync(false, source, config, CodeFormatter.MakePosition(line, column))
            |> Async.RunSynchronously

        result.Code |> String.normalizeNewLine |> should equal baseline
        result |> assertCursor expectedCursor

[<Test>]
let ``cursor on conditional keywords, 3387`` () =
    let source: string =
        """
let f x =
    if x = 1 then
        "one"
    else if x = 2 then
        "two"
    else
        "many"
"""

    let positions: ((int * int) * (int * int)) list =
        [
            for column in 4..6 do
                (3, column), (2, column)
            for column in 13..17 do
                (3, column), (2, column)
            for column in 4..8 do
                (5, column), (3, column)
            for column in 9..11 do
                (5, column), (3, column)
            for column in 18..22 do
                (5, column), (3, column)
            for column in 4..8 do
                (7, column), (4, column)
            // Keep the existing indentation and condition anchors unchanged.
            (3, 0), (2, 0)
            (3, 7), (2, 7)
            (5, 0), (3, 0)
            (5, 12), (3, 12)
            (7, 0), (4, 0)
        ]

    assertKeywordCursors
        source
        config
        """
let f x =
    if x = 1 then "one"
    else if x = 2 then "two"
    else "many"
"""
        positions

[<Test>]
let ``cursor on single line conditional keywords`` () =
    assertKeywordCursors
        """
let f x = if x then 1 else 2
"""
        config
        """
let f x = if x then 1 else 2
"""
        [
            for column in 10..12 do
                (2, column), (1, column)
            for column in 15..19 do
                (2, column), (1, column)
            for column in 22..26 do
                (2, column), (1, column)
        ]

[<Test>]
let ``cursor on conditional keywords without else`` () =
    assertKeywordCursors
        """
let f x = if x then ()
"""
        config
        """
let f x =
    if x then
        ()
"""
        [ (2, 10), (2, 4); (2, 12), (2, 6); (2, 15), (2, 9); (2, 19), (2, 13) ]

[<Test>]
let ``cursor on elif keywords`` () =
    assertKeywordCursors
        """
let f x = if x = 1 then 1 elif x = 2 then 2 else 3
"""
        config
        """
let f x =
    if x = 1 then 1
    elif x = 2 then 2
    else 3
"""
        [
            for offset in 0..4 do
                (2, 26 + offset), (3, 4 + offset)
            for offset in 0..4 do
                (2, 37 + offset), (3, 15 + offset)
        ]

[<Test>]
let ``cursor on else if keywords with extra source spacing`` () =
    assertKeywordCursors
        """
let f x = if x = 1 then 1 else     if x = 2 then 2 else 3
"""
        config
        """
let f x =
    if x = 1 then 1
    else if x = 2 then 2
    else 3
"""
        [
            for offset in 0..4 do
                (2, 26 + offset), (3, 4 + offset)
            for offset in 0..2 do
                (2, 35 + offset), (3, 9 + offset)
            // Compatibility checks for the old whitespace fallback, not a token-mapping contract.
            (2, 32), (3, 6)
            (2, 33), (3, 7)
            (2, 34), (3, 8)
        ]

[<Test>]
let ``cursor on else if keywords on separate source lines`` () =
    assertKeywordCursors
        """
let f x =
    if x = 1 then 1
    else
        if x = 2 then 2
        else 3
"""
        config
        """
let f x =
    if x = 1 then 1
    else if x = 2 then 2
    else 3
"""
        [
            for offset in 0..4 do
                (4, 4 + offset), (3, 4 + offset)
            for offset in 0..2 do
                (5, 8 + offset), (3, 9 + offset)
            (6, 8), (4, 4)
        ]

[<Test>]
let ``cursor on keywords in a multiline conditional header`` () =
    assertKeywordCursors
        """
let f x = if x = 1 && x = 2 && x = 3 then 1 else 2
"""
        { config with MaxLineLength = 25 }
        """
let f x =
    if
        x = 1
        && x = 2
        && x = 3
    then
        1
    else
        2
"""
        [
            for offset in 0..2 do
                (2, 10 + offset), (2, 4 + offset)
            for offset in 0..4 do
                (2, 37 + offset), (6, 4 + offset)
        ]

[<Test>]
let ``cursor on match expression keywords`` () =
    assertKeywordCursors
        """
let f x = match x with | _ -> 1
"""
        config
        """
let f x =
    match x with
    | _ -> 1
"""
        [
            for offset in 0..5 do
                (2, 10 + offset), (2, 4 + offset)
            for offset in 0..4 do
                (2, 18 + offset), (2, 12 + offset)
        ]

[<Test>]
let ``cursor on else if keywords preserves comments between keywords`` () =
    assertKeywordCursors
        """
if a then ()
else
    // Comment 1
    if b then ()
    // Comment 2
    else ()
"""
        config
        """
if a then
    ()
else if
    // Comment 1
    b
then
    ()
// Comment 2
else
    ()
"""
        [
            (2, 0), (1, 0)
            (2, 5), (1, 5)
            (3, 0), (3, 0)
            (3, 4), (3, 4)
            (5, 4), (3, 5)
            (5, 6), (3, 7)
            (5, 9), (6, 0)
            (7, 4), (9, 0)
        ]

[<Test>]
let ``cursor on conditional keywords after a leading comment`` () =
    assertKeywordCursors
        """
let f x =
    // Comment
    if x then 1 else 2
"""
        config
        """
let f x =
    // Comment
    if x then 1 else 2
"""
        [ (4, 4), (3, 4); (4, 6), (3, 6); (4, 9), (3, 9); (4, 13), (3, 13) ]

[<Test>]
let ``cursor on keywords in successive else if branches`` () =
    assertKeywordCursors
        """
let f x = if x = 1 then 1 else if x = 2 then 2 else if x = 3 then 3 else 4
"""
        config
        """
let f x =
    if x = 1 then 1
    else if x = 2 then 2
    else if x = 3 then 3
    else 4
"""
        [
            (2, 26), (3, 4)
            (2, 31), (3, 9)
            (2, 40), (3, 18)
            (2, 47), (4, 4)
            (2, 52), (4, 9)
            (2, 61), (4, 18)
        ]

[<Test>]
let ``cursor on keywords in a multiline else if header`` () =
    assertKeywordCursors
        """
let f x = if x = 0 then 0 else if x = 1 && x = 2 && x = 3 then 1 else 2
"""
        { config with MaxLineLength = 25 }
        """
let f x =
    if x = 0 then
        0
    else if
        x = 1
        && x = 2
        && x = 3
    then
        1
    else
        2
"""
        [
            (2, 26), (4, 4)
            (2, 30), (4, 8)
            (2, 31), (4, 9)
            (2, 33), (4, 11)
            (2, 58), (8, 4)
        ]

[<Test>]
let ``cursor on match bang expression keywords`` () =
    assertKeywordCursors
        """
let f x = async { match! x with | _ -> return 1 }
"""
        config
        """
let f x =
    async {
        match! x with
        | _ -> return 1
    }
"""
        [
            for offset in 0..6 do
                (2, 18 + offset), (3, 8 + offset)
            for offset in 0..4 do
                (2, 27 + offset), (3, 17 + offset)
        ]

[<Test>]
let ``cursor on then on a separate source line with a trailing comment`` () =
    assertKeywordCursors
        """
let f x =
    if x
    then // Keep this comment
        1
    else
        2
"""
        config
        """
let f x =
    if x then // Keep this comment
        1
    else
        2
"""
        [
            for offset in 0..2 do
                (3, 4 + offset), (2, 4 + offset)
            for offset in 0..4 do
                (4, 4 + offset), (2, 9 + offset)
        ]
