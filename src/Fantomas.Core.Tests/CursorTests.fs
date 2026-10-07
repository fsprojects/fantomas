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

let formatStroustrupRecordTypeArgumentWithCursor (line: int, column: int) : FormatResult =
    CodeFormatter.FormatDocumentAsync(
        false,
        "f<{| a: int |}>",
        { FormatConfig.Default with
            MultilineBracketStyle = Stroustrup
            RecordMultilineFormatter = NumberOfItems
            MaxRecordNumberOfItems = 0
        },
        CodeFormatter.MakePosition(line, column)
    )
    |> Async.RunSynchronously

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
let ``cursor on generic opening angle bracket does not change stroustrup layout`` () =
    let result = formatStroustrupRecordTypeArgumentWithCursor (1, 1)

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|}>
"""

    result |> assertCursor (1, 1)

[<Test>]
let ``cursor on anonymous record opening brace does not change stroustrup layout`` () =
    let result = formatStroustrupRecordTypeArgumentWithCursor (1, 2)

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|}>
"""

    result |> assertCursor (1, 2)

[<Test>]
let ``cursor on anonymous record opening bar does not change stroustrup layout`` () =
    let result = formatStroustrupRecordTypeArgumentWithCursor (1, 3)

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|}>
"""

    result |> assertCursor (1, 3)

[<Test>]
let ``cursor on anonymous record closing bar does not change stroustrup layout`` () =
    let result = formatStroustrupRecordTypeArgumentWithCursor (1, 12)

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|}>
"""

    result |> assertCursor (3, 0)

[<Test>]
let ``cursor on anonymous record closing brace does not change stroustrup layout`` () =
    let result = formatStroustrupRecordTypeArgumentWithCursor (1, 13)

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|}>
"""

    result |> assertCursor (3, 1)

[<Test>]
let ``cursor on generic closing angle bracket does not change stroustrup layout`` () =
    let result = formatStroustrupRecordTypeArgumentWithCursor (1, 14)

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|}>
"""

    result |> assertCursor (3, 2)

[<Test>]
let ``cursor on separate generic closing angle bracket follows offside padding`` () =
    let result =
        CodeFormatter.FormatDocumentAsync(
            false,
            """f<{| a: int |} // keep me
 >""",
            { FormatConfig.Default with
                MultilineBracketStyle = Stroustrup
                RecordMultilineFormatter = NumberOfItems
                MaxRecordNumberOfItems = 0
            },
            CodeFormatter.MakePosition(2, 1)
        )
        |> Async.RunSynchronously

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<{|
    a: int
|} // keep me
 >
"""

    result |> assertCursor (4, 1)

[<Test>]
let ``cursor on generic closing bracket survives fallback with comments`` () =
    let result =
        CodeFormatter.FormatDocumentAsync(
            false,
            """f<{| a: int // field
 |}, int -> int -> int -> string // closing
 >""",
            { FormatConfig.Default with
                MultilineBracketStyle = Stroustrup
                MaxRecordWidth = 0
                MaxLineLength = 20
            },
            CodeFormatter.MakePosition(3, 1)
        )
        |> Async.RunSynchronously

    result.Code
    |> String.normalizeNewLine
    |> prepend newline
    |> should
        equal
        """
f<
    {|
        a: int // field
    |},
    int
        -> int
        -> int
        -> string // closing
 >
"""

    result |> assertCursor (9, 1)

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

[<Test>]
let ``cursor on if keyword, 3387`` () =
    formatWithCursor
        """
let f x = if x then ()
"""
        (2, 11)
    |> assertCursor (2, 5)

[<Test>]
let ``cursor on then keyword`` () =
    formatWithCursor
        """
let f x = if x then ()
"""
        (2, 16)
    |> assertCursor (2, 10)

[<Test>]
let ``cursor on elif keyword`` () =
    formatWithCursor
        """
let f x = if x = 1 then 1 elif x = 2 then 2 else 3
"""
        (2, 28)
    |> assertCursor (3, 6)

[<Test>]
let ``cursor on match keyword`` () =
    formatWithCursor
        """
let f x = match x with | _ -> 1
"""
        (2, 12)
    |> assertCursor (2, 6)

[<Test>]
let ``cursor on with keyword`` () =
    formatWithCursor
        """
let f x = match x with | _ -> 1
"""
        (2, 20)
    |> assertCursor (2, 14)

[<Test>]
let ``cursor on match bang keyword`` () =
    formatWithCursor
        """
let f x = async { match! x with | _ -> return 1 }
"""
        (2, 23)
    |> assertCursor (3, 13)

[<Test>]
let ``cursor on else keyword of else if`` () =
    formatWithCursor
        """
let f x = if x = 1 then 1 else if x = 2 then 2 else 3
"""
        (2, 28)
    |> assertCursor (3, 6)

[<Test>]
let ``cursor on if keyword of else if`` () =
    formatWithCursor
        """
let f x = if x = 1 then 1 else if x = 2 then 2 else 3
"""
        (2, 32)
    |> assertCursor (3, 10)

[<Test>]
let ``cursor on if keyword of else if on its own line`` () =
    formatWithCursor
        """
let f x =
    if x = 1 then 1
    else
        if x = 2 then 2
        else 3
"""
        (5, 9)
    |> assertCursor (3, 10)

[<Test>]
let ``cursor on closing bracket of a cramped list`` () =
    CodeFormatter.FormatDocumentAsync(
        false,
        """
let xs = [ aaaaaaaaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccccccccccc; dddddddddddddddddddddd ]
""",
        { FormatConfig.Default with
            MultilineBracketStyle = Cramped
        },
        CodeFormatter.MakePosition(2, 117)
    )
    |> Async.RunSynchronously
    |> assertCursor (5, 29)
