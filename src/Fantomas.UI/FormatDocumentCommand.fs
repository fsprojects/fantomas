namespace FSharpVSPowerTools.CodeFormatting

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text
open System.Threading.Tasks
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open Fantomas.FormatConfig
open Fantomas.CodeFormatter

type FormatDocumentCommand(getConfig: Func<FormatConfig>) =
    inherit FormatCommand(getConfig)

    override x.Execute() =
        use disposable = Cursor.wait()
        x.ExecuteFormat()

    override x.GetFormatted(isSignatureFile: bool, source: string, config: FormatConfig) =
        formatSourceString isSignatureFile source config

    override x.SetNewCaretPosition(caretPos, scrollBarPos, originalSnapshot) =
        let caretLine = originalSnapshot.GetLineFromPosition(caretPos.Position)
        let line = originalSnapshot.GetLineNumberFromPosition(int caretPos)
        let column = caretPos - caretLine.Start
        let maxLine = originalSnapshot.LineCount

        let currentSnapshot = x.TextBuffer.CurrentSnapshot
        let newMaxLine = currentSnapshot.LineCount

        // Scale caret positions in a linear way
        let newLineNo = int (float line * (float newMaxLine) / (float maxLine))
        let newLine = currentSnapshot.GetLineFromLineNumber(newLineNo)
        let newColumn = min column newLine.Length
        let newCaretPos = newLine.Start.Add(newColumn)
        let caretPolet = VirtualSnapshotPoint(currentSnapshot, int newCaretPos)
        x.TextView.Caret.MoveTo(caretPolet) |> ignore

        // Assume that the document scales in a linear way
        let newScrollBarPos = int (float scrollBarPos * (float newMaxLine) / (float maxLine))
        x.TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, newScrollBarPos)