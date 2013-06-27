using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.FSharp.Compiler;
using Microsoft.VisualStudio.Text;

namespace Hestia.FSharpCommands.Utils
{
    internal static class TextUtils
    {
        internal static Range.pos GetFSharpPos(VirtualSnapshotPoint point)
        {
            var containingLine = point.Position.GetContainingLine();
            // F# compiler line numbers start at 1
            int lineNumber = containingLine.LineNumber + 1;  
            int charIndex = point.Position.Position - containingLine.Start.Position;
            return Range.mkPos(lineNumber, charIndex);
        }
    }
}
