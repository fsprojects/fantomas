using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;

namespace Hestia.FSharpCommands
{
    [Guid("9F2500E9-9A2E-4741-B13E-32FE96C01268")]
    public class FantomasOptionsPage : DialogPage
    {
        public FantomasOptionsPage()
        {
            PageWidth = Fantomas.FormatConfig.FormatConfig.Default.PageWidth;
            SemicolonAtEndOfLine = Fantomas.FormatConfig.FormatConfig.Default.SemicolonAtEndOfLine;
            SpaceBeforeArgument = Fantomas.FormatConfig.FormatConfig.Default.SpaceBeforeArgument;
            SpaceBeforeColon = Fantomas.FormatConfig.FormatConfig.Default.SpaceBeforeColon;
            SpaceAfterComma = Fantomas.FormatConfig.FormatConfig.Default.SpaceAfterComma;
            SpaceAfterSemicolon = Fantomas.FormatConfig.FormatConfig.Default.SpaceAfterSemicolon;
            IndentOnTryWith = Fantomas.FormatConfig.FormatConfig.Default.IndentOnTryWith;
        }

        [Category("Layout")]
        [DisplayName("Page Width")]
        public int PageWidth { get; set; }

        [Category("Syntax")]
        [DisplayName("Semicolon at End of Line")]
        public bool SemicolonAtEndOfLine { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Before Argument")]
        public bool SpaceBeforeArgument { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Before Colon")]
        public bool SpaceBeforeColon { get; set; }

        [Category("Spacing")]
        [DisplayName("Space After Comma")]
        public bool SpaceAfterComma { get; set; }

        [Category("Spacing")]
        [DisplayName("Space After Semicolon")]
        public bool SpaceAfterSemicolon { get; set; }

        [Category("Indentation")]
        [DisplayName("Indent on Try...With")]
        public bool IndentOnTryWith { get; set; }
    }
}
