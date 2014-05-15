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

    let mutable isFormattingCursor = false

    override x.Execute() =
        isFormattingCursor <- x.TextView.Selection.IsEmpty
        use disposable = Cursor.wait()

        let resetSelection = 
            if isFormattingCursor then
                id
            else
                // We're going to take advantage of the fact that nothing before or after the selection
                // should change, so the post-formatting range will start at the same point, and end at
                // the same offset from the end of the file.
                let activePointPos = x.TextView.Selection.ActivePoint.Position.Position
                let anchorPointPos = x.TextView.Selection.AnchorPoint.Position.Position
                // They should always be different but just in case
                let activePointIsAtStart = activePointPos <= anchorPointPos  

                let selOffsetFromStart = x.TextView.Selection.Start.Position.Position
                let selOffsetFromEnd = x.TextView.TextBuffer.CurrentSnapshot.Length - x.TextView.Selection.End.Position.Position

                fun () ->
                    let newSelStartPos = selOffsetFromStart
                    let newSelEndPos = x.TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd
                    let newActivePointPos = if activePointIsAtStart then newSelStartPos else newSelEndPos
                    let newAnchorPointPos = if activePointIsAtStart then newSelEndPos else newSelStartPos
                    let newActivePoint = VirtualSnapshotPoint(x.TextView.TextBuffer.CurrentSnapshot, newActivePointPos) 
                    let newAnchorPoint = VirtualSnapshotPoint(x.TextView.TextBuffer.CurrentSnapshot, newAnchorPointPos)
                    x.TextView.Selection.Select(newAnchorPoint, newActivePoint)

        x.ExecuteFormat()

        resetSelection()

    override x.GetFormatted(isSignatureFile: bool, source: string, config: FormatConfig) =
        if isFormattingCursor then
            let caretPos = VirtualSnapshotPoint(x.TextView.TextBuffer.CurrentSnapshot, int x.TextView.Caret.Position.BufferPosition)
            let pos = TextUtils.getFSharpPos(caretPos)
            formatAroundCursor isSignatureFile pos source config
        else
            let startPos = TextUtils.getFSharpPos(x.TextView.Selection.Start)
            let endPos = TextUtils.getFSharpPos(x.TextView.Selection.End)
            let range = Range.mkRange "fsfile" startPos endPos

            formatSelectionFromString isSignatureFile range source config

    override x.GetNewCaretPositionSetter() =
        let caretPos = x.TextView.Caret.Position.BufferPosition

        if (isFormattingCursor || caretPos = x.TextView.Selection.Start.Position) then
            let selStartPos = x.TextView.Selection.Start.Position.Position

            // Get start line of scroll bar
            let scrollBarLine = x.TextView.TextViewLines.FirstOrDefault(fun l -> l.VisibilityState <> VisibilityState.Hidden)
            let scrollBarPos =
                if (scrollBarLine = null) then 0 else scrollBarLine.Snapshot.GetLineNumberFromPosition(int scrollBarLine.Start)

            fun () ->
                // The caret is at the start of selection, its position is unchanged
                let newSelStartPos = selStartPos
                let newActivePoint = new VirtualSnapshotPoint(x.TextView.TextBuffer.CurrentSnapshot, newSelStartPos)
                x.TextView.Caret.MoveTo(newActivePoint) |> ignore
                x.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos)
        else
            let selOffsetFromEnd = x.TextView.TextBuffer.CurrentSnapshot.Length - x.TextView.Selection.End.Position.Position

            // Get start line of scroll bar
            let scrollBarLine = x.TextView.TextViewLines.FirstOrDefault(fun l -> l.VisibilityState <> VisibilityState.Hidden);
            let scrollBarPos =
                if (scrollBarLine = null) then 0
                else scrollBarLine.Snapshot.GetLineNumberFromPosition(int scrollBarLine.Start)

            fun () ->
                // The caret is at the end of selection, its offset from the end of text is unchanged
                let newSelEndPos = x.TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd
                let newAnchorPoint = VirtualSnapshotPoint(x.TextView.TextBuffer.CurrentSnapshot, newSelEndPos)

                x.TextView.Caret.MoveTo(newAnchorPoint) |> ignore
                x.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos)
