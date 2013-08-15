using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Hestia.FSharpCommands.Utils;
using Microsoft.FSharp.Compiler;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Formatting;

namespace Hestia.FSharpCommands.Commands
{
    public class FormatSelectionCommand : FormatCommand
    {
        private bool isFormattingCursor;

        public override void Execute()
        {
            isFormattingCursor = TextView.Selection.IsEmpty;

            using (Cursor.Wait())
            {
                var resetSelection = GetSelectionResetter();
                ExecuteFormat();

                resetSelection();
            }
        }

        protected override string GetFormatted(bool isSignatureFile, string source, Fantomas.FormatConfig.FormatConfig config)
        {
            if (isFormattingCursor) 
            {
                var caretPos = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, TextView.Caret.Position.BufferPosition);
                Range.pos pos = TextUtils.GetFSharpPos(caretPos);
                return Fantomas.CodeFormatter.formatAroundCursor(isSignatureFile, pos, source, config);
            }

            Range.pos startPos = TextUtils.GetFSharpPos(TextView.Selection.Start);
            Range.pos endPos = TextUtils.GetFSharpPos(TextView.Selection.End);
            Range.range range = Range.mkRange("fsfile", startPos, endPos);

            return Fantomas.CodeFormatter.formatSelectionFromString(isSignatureFile, range, source, config);
        }

        protected override Action GetNewCaretPositionSetter()
        {
            var caretPos = TextView.Caret.Position.BufferPosition;

            if (isFormattingCursor || caretPos == TextView.Selection.Start.Position)
            {
                int selStartPos = TextView.Selection.Start.Position.Position;

                // Get start line of scroll bar
                var scrollBarLine = TextView.TextViewLines.FirstOrDefault(l => l.VisibilityState != VisibilityState.Hidden);
                int scrollBarPos = (scrollBarLine == null) ? 0 : scrollBarLine.Snapshot.GetLineNumberFromPosition(scrollBarLine.Start);

                Action setNewCaretPosition = () =>
                {
                    // The caret is at the start of selection, its position is unchanged
                    int newSelStartPos = selStartPos;
                    var newActivePoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newSelStartPos);
                    TextView.Caret.MoveTo(newActivePoint);
                    TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos);
                };

                return setNewCaretPosition;
            }
            else
            {
                int selOffsetFromEnd = TextView.TextBuffer.CurrentSnapshot.Length - TextView.Selection.End.Position.Position;

                // Get start line of scroll bar
                var scrollBarLine = TextView.TextViewLines.FirstOrDefault(l => l.VisibilityState != VisibilityState.Hidden);
                int scrollBarPos = (scrollBarLine == null) ? 0 : scrollBarLine.Snapshot.GetLineNumberFromPosition(scrollBarLine.Start);

                Action setNewCaretPosition = () =>
                    {
                        // The caret is at the end of selection, its offset from the end of text is unchanged
                        int newSelEndPos = TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd;
                        var newAnchorPoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newSelEndPos);

                        TextView.Caret.MoveTo(newAnchorPoint);
                        TextView.ViewScroller.ScrollViewportVerticallyByLines(ScrollDirection.Down, scrollBarPos);
                    };

                return setNewCaretPosition;
            }
        }

        private Action GetSelectionResetter()
        {
            if (isFormattingCursor)
                return () => { };

            // We're going to take advantage of the fact that nothing before or after the selection
            // should change, so the post-formatting range will start at the same point, and end at
            // the same offset from the end of the file.

            int activePointPos = TextView.Selection.ActivePoint.Position.Position;
            int anchorPointPos = TextView.Selection.AnchorPoint.Position.Position;
            bool activePointIsAtStart = activePointPos <= anchorPointPos;  // they should always be different but just in case

            int selOffsetFromStart = TextView.Selection.Start.Position.Position;
            int selOffsetFromEnd = TextView.TextBuffer.CurrentSnapshot.Length - TextView.Selection.End.Position.Position;

            Action resetSelection = () =>
                {
                    int newSelStartPos = selOffsetFromStart;
                    int newSelEndPos = TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd;
                    int newActivePointPos = activePointIsAtStart ? newSelStartPos : newSelEndPos;
                    int newAnchorPointPos = activePointIsAtStart ? newSelEndPos : newSelStartPos;
                    var newActivePoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newActivePointPos); 
                    var newAnchorPoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newAnchorPointPos);
                    TextView.Selection.Select(newAnchorPoint, newActivePoint);
                };

            return resetSelection;
        }
    }
}
