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
    }
}
