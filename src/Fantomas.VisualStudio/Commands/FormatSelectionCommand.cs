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
            // This still seems to give "The indent level cannot be negative"
            // in a lot of cases that feel like they should work, e.g. 'let' forms

            Range.pos startPos = TextUtils.GetFSharpPos(TextView.Selection.Start);
            Range.pos endPos = TextUtils.GetFSharpPos(TextView.Selection.End);
            Range.range range = Range.mkRange("fsfile", startPos, endPos);

            return Fantomas.CodeFormatter.formatSelectionFromString(isSignatureFile, range, source, config);
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
