using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;

namespace Hestia.FSharpCommands.Commands
{
    public abstract class FormatCommand : CommandBase
    {
        protected void ExecuteFormat()
        {
            string text = TextView.TextSnapshot.GetText();

            ITextBuffer buffer = TextView.TextBuffer;

            IEditorOptions editorOptions = Services.EditorOptionsFactory.GetOptions(buffer);
            int indentSize = editorOptions.GetOptionValue<int>(new IndentSize().Key);
            FantomasOptionsPage customOptions = (FantomasOptionsPage)(Package.GetGlobalService(typeof(FantomasOptionsPage)));

            string source = GetAllText(buffer);

            var isSignatureFile = IsSignatureFile(buffer);

            var config = new Fantomas.FormatConfig.FormatConfig(
                indentSpaceNum: indentSize,
                pageWidth: customOptions.PageWidth,
                semicolonAtEndOfLine: customOptions.SemicolonAtEndOfLine,
                spaceBeforeArgument: customOptions.SpaceBeforeArgument,
                spaceBeforeColon: customOptions.SpaceBeforeColon,
                spaceAfterComma: customOptions.SpaceAfterComma,
                spaceAfterSemicolon: customOptions.SpaceAfterSemicolon,
                indentOnTryWith: customOptions.IndentOnTryWith
                );

            try
            {
                var formatted = GetFormatted(isSignatureFile, source, config);

                using (var edit = buffer.CreateEdit())
                {
                    edit.Replace(0, text.Length, formatted);
                    edit.Apply();

                    // TODO: return cursor to the correct position
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("Unable to format.  " + ex.Message);
                return;
            }
        }

        protected abstract string GetFormatted(bool isSignatureFile, string source, Fantomas.FormatConfig.FormatConfig config);

        private static string GetAllText(ITextBuffer buffer)
        {
            string source;

            using (var writer = new StringWriter())
            {
                buffer.CurrentSnapshot.Write(writer);
                writer.Flush();
                source = writer.ToString();
            }
            return source;
        }

        private static bool IsSignatureFile(ITextBuffer buffer)
        {
            ITextDocument document = buffer.Properties.GetProperty<ITextDocument>(typeof(ITextDocument));
            var fileExtension = Path.GetExtension(document.FilePath);
            var isSignatureFile = ".fsi".Equals(fileExtension, StringComparison.OrdinalIgnoreCase);  // There isn't a distinct content type for FSI files, so we have to use the file extension
            return isSignatureFile;
        }
    }
}
