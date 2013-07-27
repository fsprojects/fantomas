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
            var config = Fantomas.FormatConfig.FormatConfig.Default;

            PageWidth = config.PageWidth;
            SemicolonAtEndOfLine = config.SemicolonAtEndOfLine;
            SpaceBeforeArgument = config.SpaceBeforeArgument;
            SpaceBeforeColon = config.SpaceBeforeColon;
            SpaceAfterComma = config.SpaceAfterComma;
            SpaceAfterSemicolon = config.SpaceAfterSemicolon;
            IndentOnTryWith = config.IndentOnTryWith;
            ReorderOpenDeclaration = config.ReorderOpenDeclaration;
            SpaceAroundDelimiter = config.SpaceAroundDelimiter;
        }

        [Category("Layout")]
        [DisplayName("Page Width")]
        [Description("Set maximum width.")]
        public int PageWidth { get; set; }

        [Category("Syntax")]
        [DisplayName("Semicolon at End of Line")]
        [Description("Insert ';' at EOL between list, array and record elements.")]
        public bool SemicolonAtEndOfLine { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Before Argument")]
        [Description("Insert ' ' before arguments in function applications.")]
        public bool SpaceBeforeArgument { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Before Colon")]
        [Description("Insert ' ' before ':' in type annotations.")]
        public bool SpaceBeforeColon { get; set; }

        [Category("Spacing")]
        [DisplayName("Space After Comma")]
        [Description("Insert ' ' after ',' in tuples.")]
        public bool SpaceAfterComma { get; set; }

        [Category("Spacing")]
        [DisplayName("Space After Semicolon")]
        [Description("Insert ' ' after ';' in list, array and record elements.")]
        public bool SpaceAfterSemicolon { get; set; }

        [Category("Spacing")]
        [DisplayName("Space Around Delimiter")]
        [Description("Insert ' ' after starting and before ending of lists, arrays, sequences and records.")]
        public bool SpaceAroundDelimiter { get; set; }

        [Category("Indentation")]
        [DisplayName("Indent on Try...With")]
        [Description("Indent one level in all clauses of with blocks.")]
        public bool IndentOnTryWith { get; set; }

        [Category("Refactoring")]
        [DisplayName("Reorder open declarations")]
        [Description("Reorder and deduplicate open statements while doing formatting.")]
        public bool ReorderOpenDeclaration { get; set; }
    }
}
