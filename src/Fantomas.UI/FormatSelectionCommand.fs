namespace FSharpVSPowerTools.CodeFormatting

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open Microsoft.FSharp.Compiler
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open Fantomas.FormatConfig
open Fantomas.CodeFormatter

type FormatSelectionCommand(getConfig: Func<FormatConfig>) =
    inherit FormatCommand(getConfig)

    let mutable isFormattingCursorPosition = false
    let mutable selStartPos = 0
    let mutable selOffsetFromEnd = 0
    
    override x.Execute() =
        isFormattingCursorPosition <- x.TextView.Selection.IsEmpty
        // Need to capture current snapshot before formatting is executed
        let originalSnapshot = x.TextBuffer.CurrentSnapshot
        selStartPos <- x.TextView.Selection.Start.Position.Position
        selOffsetFromEnd <- originalSnapshot.Length - x.TextView.Selection.End.Position.Position

        use disposable = Cursor.wait()
        x.ExecuteFormat()

        if not isFormattingCursorPosition then
            // We're going to take advantage of the fact that nothing before or after the selection
            // should change, so the post-formatting range will start at the same point, and end at
            // the same offset from the end of the file.
            let activePointPos = x.TextView.Selection.ActivePoint.Position.Position
            let anchorPointPos = x.TextView.Selection.AnchorPoint.Position.Position
            // They should always be different but just in case
            let activePointIsAtStart = activePointPos <= anchorPointPos  

            let selOffsetFromStart = x.TextView.Selection.Start.Position.Position
            let selOffsetFromEnd = originalSnapshot.Length - x.TextView.Selection.End.Position.Position

            let newSelStartPos = selOffsetFromStart
            let newSelEndPos = x.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd
            let newActivePointPos = if activePointIsAtStart then newSelStartPos else newSelEndPos
            let newAnchorPointPos = if activePointIsAtStart then newSelEndPos else newSelStartPos
            let newActivePoint = VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, newActivePointPos) 
            let newAnchorPoint = VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, newAnchorPointPos)
            x.TextView.Selection.Select(newAnchorPoint, newActivePoint)

    override x.GetFormatted(isSignatureFile: bool, source: string, config: FormatConfig) =
        if isFormattingCursorPosition then
            let caretPos = VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, int x.TextView.Caret.Position.BufferPosition)
            let pos = TextUtils.getFSharpPos(caretPos)
            formatAroundCursor isSignatureFile pos source config
        else
            let startPos = TextUtils.getFSharpPos(x.TextView.Selection.Start)
            let endPos = TextUtils.getFSharpPos(x.TextView.Selection.End)
            let range = Range.mkRange "/tmp.fsx" startPos endPos
            formatSelectionFromString isSignatureFile range source config

    override x.SetNewCaretPosition(caretPos, scrollBarPos, _originalSnapshot) =
        if isFormattingCursorPosition || caretPos = x.TextView.Selection.Start.Position then
            // The caret is at the start of selection, its position is unchanged
            let newSelStartPos = selStartPos
            let newActivePoint = new VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, newSelStartPos)
            x.TextView.Caret.MoveTo(newActivePoint) |> ignore
        else
            // The caret is at the end of selection, its offset from the end of text is unchanged
            let newSelEndPos = x.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd
            let newAnchorPoint = VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, newSelEndPos)
            x.TextView.Caret.MoveTo(newAnchorPoint) |> ignore
        x.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos)
