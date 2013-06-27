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
            var caretPos = TextView.Caret.Position.BufferPosition;
            var caretLine = TextView.TextSnapshot.GetLineFromPosition(caretPos.Position);
            int line = TextView.TextBuffer.CurrentSnapshot.GetLineNumberFromPosition(caretLine.Start);
            int column = caretLine.End - caretLine.Start;

            // Get start line of scroll bar
            var scrollBarLine = TextView.TextViewLines.FirstOrDefault(l => l.VisibilityState != VisibilityState.Hidden);
            int scrollBarPos = (scrollBarLine == null) ? 0 : TextView.TextBuffer.CurrentSnapshot.GetLineNumberFromPosition(scrollBarLine.Start);
            int length = TextView.TextBuffer.CurrentSnapshot.Length;

            Action setNewCaretPosition = () =>
                {
                    // Scale caret positions in a linear way
                    int newLine = line * TextView.TextBuffer.CurrentSnapshot.Length / length;
                    var newCaretPos = TextView.TextBuffer.CurrentSnapshot.GetLineFromLineNumber(newLine).Start.Add(column);
                    var caretPoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newCaretPos);
                    TextView.Caret.MoveTo(caretPoint);

                    // Assume that the document scales in a linear way
                    int newScrollBarPos = scrollBarPos * TextView.TextBuffer.CurrentSnapshot.Length / length;
                    TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, newScrollBarPos);
                };

            return setNewCaretPosition;
        }
    }
}
