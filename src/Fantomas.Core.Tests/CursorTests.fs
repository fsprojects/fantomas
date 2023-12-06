module Fantomas.Core.Tests.CursorTests

open Fantomas.FCS.Text
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
let ``cursor is be consider as content before, 3007`` () =
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
