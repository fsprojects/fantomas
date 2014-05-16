namespace FSharpVSPowerTools.CodeFormatting

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open Microsoft.FSharp.Compiler.Range
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
    let mutable selectedRange: range option = None
    
    override x.Execute() =
        isFormattingCursorPosition <- x.TextView.Selection.IsEmpty
        // Need to capture current snapshot before formatting is executed
        let originalSnapshot = x.TextBuffer.CurrentSnapshot
        selStartPos <- x.TextView.Selection.Start.Position.Position
        selOffsetFromEnd <- originalSnapshot.Length - x.TextView.Selection.End.Position.Position
        let isReversedSelection = x.TextView.Selection.IsReversed

        use disposable = Cursor.wait()
        x.ExecuteFormat()

        match isFormattingCursorPosition, selectedRange with
        | false, Some selectedRange ->
            // We're going to take advantage of the fact that nothing before or after the selection
            // should change, so the post-formatting range will start at the same point, and end at
            // the same offset from the end of the file.
            
            let startPos = 
                originalSnapshot.GetLineFromLineNumber(selectedRange.StartLine-1).Start.Position 
                    + selectedRange.StartColumn
            
            // F# range is inclusive, add one to mimic selection
            let endPos = 
                originalSnapshot.GetLineFromLineNumber(selectedRange.EndLine-1).Start.Position 
                    + selectedRange.EndColumn + 1

            let newLength = endPos - startPos + x.TextBuffer.CurrentSnapshot.Length - originalSnapshot.Length
            let newSelection = SnapshotSpan(x.TextBuffer.CurrentSnapshot, startPos, newLength)

            x.TextView.Selection.Select(newSelection, isReversedSelection)
        | _ -> ()

    override x.GetFormatted(isSignatureFile: bool, source: string, config: FormatConfig) =
        if isFormattingCursorPosition then
            let caretPos = VirtualSnapshotPoint(x.TextBuffer.CurrentSnapshot, int x.TextView.Caret.Position.BufferPosition)
            let pos = TextUtils.getFSharpPos(caretPos)
            formatAroundCursor isSignatureFile pos source config
        else
            let startPos = TextUtils.getFSharpPos(x.TextView.Selection.Start)
            let endPos = TextUtils.getFSharpPos(x.TextView.Selection.End)
            let range = mkRange "/tmp.fsx" startPos endPos
            let (formatted, modifiedRange) = formatSelectionExpanded isSignatureFile range source config
            selectedRange <- Some modifiedRange
            formatted

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
