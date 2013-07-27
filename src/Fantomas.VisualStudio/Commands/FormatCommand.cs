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
using Microsoft.VisualStudio.Text.Operations;

namespace Hestia.FSharpCommands.Commands
{
    public abstract class FormatCommand : CommandBase
    {
        protected void ExecuteFormat()
        {
            var editorOperations = Services.EditorOperationsFactoryService.GetEditorOperations(TextView);
            using (var textUndoTransaction = TryCreateTextUndoTransaction())
            {
                // Handle the special case of a null ITextUndoTransaction up here because it simplifies
                // the rest of the method.  The implementation of operations such as 
                // AddBeforeTextBufferUndoChangePrimitive will directly access the ITextUndoHistory of 
                // the ITextBuffer.  If there is no history then this operation will throw a NullReferenceException
                // instead of failing gracefully.  If we have an ITextUndoTransaction then we know that the 
                // ITextUndoHistory exists and can call all of the methods as appropriate. 
                if (textUndoTransaction == null)
                {
                    ExecuteFormatCore();
                    return;
                }

                // This command will capture the caret position as it currently exists inside the undo 
                // transaction.  That way the undo command will reset the caret back to this position.  
                editorOperations.AddBeforeTextBufferChangePrimitive();

                if (ExecuteFormatCore())
                {
                    // Capture the caret as it exists now.  This way any redo of this edit will 
                    // reposition the caret as it exists now. 
                    editorOperations.AddAfterTextBufferChangePrimitive();
                    textUndoTransaction.Complete();
                }
                else
                {
                    textUndoTransaction.Cancel();
                }
            }
        }

        private ITextUndoTransaction TryCreateTextUndoTransaction()
        {
            var textBufferUndoManager = Services.TextBufferUndoManagerProvider.GetTextBufferUndoManager(TextBuffer);

            // It is possible for an ITextBuffer to have a null ITextUndoManager.  This will happen in 
            // cases like the differencing viewer.  If VS doesn't consider the document to be editable then 
            // it won't create an undo history for it.  Need to be tolerant of this behavior. 
            if (textBufferUndoManager == null)
            {
                return null;
            }

            return textBufferUndoManager.TextBufferUndoHistory.CreateTransaction("Format Code");
        }

        private bool ExecuteFormatCore()
        {
            string text = TextView.TextSnapshot.GetText();

            ITextBuffer buffer = TextView.TextBuffer;

            IEditorOptions editorOptions = Services.EditorOptionsFactory.GetOptions(buffer);
            int indentSize = editorOptions.GetOptionValue<int>(new IndentSize().Key);
            FantomasOptionsPage customOptions = (FantomasOptionsPage)(Package.GetGlobalService(typeof(FantomasOptionsPage)));

            string source = GetAllText(buffer);

            var isSignatureFile = IsSignatureFile(buffer);

            var config = Fantomas.FormatConfig.FormatConfig.create(
                            indentSize,
                            customOptions.PageWidth,
                            customOptions.SemicolonAtEndOfLine,
                            customOptions.SpaceBeforeArgument,
                            customOptions.SpaceBeforeColon,
                            customOptions.SpaceAfterComma,
                            customOptions.SpaceAfterSemicolon,
                            customOptions.IndentOnTryWith,
                            customOptions.ReorderOpenDeclaration,
                            customOptions.SpaceAroundDelimiter
                );

            try
            {
                var formatted = GetFormatted(isSignatureFile, source, config);

                using (var edit = buffer.CreateEdit())
                {
                    var setCaretPosition = GetNewCaretPositionSetter();

                    edit.Replace(0, text.Length, formatted);
                    edit.Apply();

                    setCaretPosition();

                    return true;
                }
            }
            catch (Fantomas.FormatConfig.FormatException ex)
            {
                MessageBox.Show(ex.Message, "Fantomas");
                return false;
            }
            catch (Exception ex)
            {
                MessageBox.Show("Unable to format. " + ex.Message, "Fantomas");
                return false;
            }
        }

        protected abstract string GetFormatted(bool isSignatureFile, string source, Fantomas.FormatConfig.FormatConfig config);

        protected abstract Action GetNewCaretPositionSetter();

        private static string GetAllText(ITextBuffer buffer)
        {
            return buffer.CurrentSnapshot.GetText();
        }

        private bool IsSignatureFile(ITextBuffer buffer)
        {
            ITextDocument textDocument;
            if (!Services.TextDocumentFactoryService.TryGetTextDocument(buffer, out textDocument))
            {
                // If this isn't backed by an actual document then it can't be considered 
                // a signature file
                return false;
            }

            var fileExtension = Path.GetExtension(textDocument.FilePath);
            // There isn't a distinct content type for FSI files, so we have to use the file extension
            var isSignatureFile = ".fsi".Equals(fileExtension, StringComparison.OrdinalIgnoreCase);
            return isSignatureFile;
        }
    }
}
