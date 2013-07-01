using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hestia.FSharpCommands.Utils;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Formatting;

namespace Hestia.FSharpCommands.Commands
{
    public class FormatDocumentCommand : FormatCommand
    {
        public override void Execute()
        {
            using (Cursor.Wait())
            {
                ExecuteFormat();
            }
        }

        protected override string GetFormatted(bool isSignatureFile, string source, Fantomas.FormatConfig.FormatConfig config)
        {        
            return Fantomas.CodeFormatter.formatSourceString(isSignatureFile, source, config);
        }

        protected override Action GetNewCaretPositionSetter()
        {
            var currentSnapshot = TextView.TextBuffer.CurrentSnapshot;

            var caretPos = TextView.Caret.Position.BufferPosition;
            var caretLine = currentSnapshot.GetLineFromPosition(caretPos.Position);
            int line = currentSnapshot.GetLineNumberFromPosition(caretPos);
            int column = caretPos - caretLine.Start;
            
            // Get start line of scroll bar
            var scrollBarLine = TextView.TextViewLines.FirstOrDefault(l => l.VisibilityState != VisibilityState.Hidden);
            int scrollBarPos = (scrollBarLine == null) ? 0 : currentSnapshot.GetLineNumberFromPosition(scrollBarLine.Start);
            int maxLine = currentSnapshot.LineCount;

            Action setNewCaretPosition = () =>
                {
                    var newCurrentSnapshot = TextView.TextBuffer.CurrentSnapshot;
                    int newMaxLine = newCurrentSnapshot.LineCount;

                    // Scale caret positions in a linear way
                    int newLine = (int) ((float)line * (float)newMaxLine / (float)maxLine);
                    var newCaretPos = newCurrentSnapshot.GetLineFromLineNumber(newLine).Start.Add(column);
                    var caretPoint = new VirtualSnapshotPoint(newCurrentSnapshot, newCaretPos);
                    TextView.Caret.MoveTo(caretPoint);

                    // Assume that the document scales in a linear way
                    int newScrollBarPos = (int) ((float)scrollBarPos * (float)newMaxLine / (float)maxLine);
                    TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, newScrollBarPos);
                };

            return setNewCaretPosition;
        }
    }
}
