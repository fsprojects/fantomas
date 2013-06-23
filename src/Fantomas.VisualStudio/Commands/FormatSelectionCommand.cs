using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Hestia.FSharpCommands.Utils;
using Microsoft.FSharp.Compiler;
using Microsoft.VisualStudio.Text;

namespace Hestia.FSharpCommands.Commands
{
    public class FormatSelectionCommand : FormatCommand
    {
        public override void Execute()
        {
            if (TextView.Selection.IsEmpty)
            {
                MessageBox.Show("No selection");
                return;
            }

            using (Cursor.Wait())
            {
                var resetSelection = GetSelectionResetter();
                ExecuteFormat();
                resetSelection();
            }
        }

        protected override string GetFormatted(bool isSignatureFile, string source, Fantomas.FormatConfig.FormatConfig config)
        {
            Range.pos startPos = TextUtils.GetFSharpPos(TextView.Selection.Start);
            Range.pos endPos = TextUtils.GetFSharpPos(TextView.Selection.End);
            Range.range range = Range.mkRange("fsfile", startPos, endPos);

            return Fantomas.CodeFormatter.formatSelectionFromString(isSignatureFile, range, source, config);
        }

        protected override Action GetNewCaretPositionSetter()
        {
            var caretPos = TextView.Caret.Position.BufferPosition;

            if (caretPos.Equals(TextView.Selection.Start.Position))
            {
                // The caret is at the start of selection, its position is unchanged
                int selStartPos = TextView.Selection.Start.Position.Position;
                //Range.pos startPos = TextUtils.GetFSharpPos(new VirtualSnapshotPoint(caretPos));
                //MessageBox.Show("True", String.Format("{0}:{1}", startPos.Line, startPos.Column));

                Action setter = () =>
                    {
                        int newSelStartPos = selStartPos;
                        var newActivePoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newSelStartPos);
                        TextView.Caret.MoveTo(newActivePoint);
                    };

                return setter;
            }
            else 
            {
                // The caret is at the end of selection, its offset from the end of text is unchanged
                int selOffsetFromEnd = TextView.TextBuffer.CurrentSnapshot.Length - TextView.Selection.End.Position.Position;

                //Range.pos endPos = TextUtils.GetFSharpPos(TextView.Selection.End);
                //Range.pos caret = TextUtils.GetFSharpPos(new VirtualSnapshotPoint(caretPos));
                //MessageBox.Show("False", String.Format("{0}:{1}:{2}:{3}", endPos.Line, endPos.Column, caret.Line, caret.Column));

                Action setter = () =>
                    {
                        int newSelEndPos = TextView.TextBuffer.CurrentSnapshot.Length - selOffsetFromEnd;
                        var newAnchorPoint = new VirtualSnapshotPoint(TextView.TextBuffer.CurrentSnapshot, newSelEndPos);
                        TextView.Caret.MoveTo(newAnchorPoint);
                    };

                return setter;
            }
        }

        private Action GetSelectionResetter()
        {
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
