using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text;

namespace Hestia.FSharpCommands
{
    public class Services
    {
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;
        private readonly IEditorOperationsFactoryService _editorOperationsFactoryService;
        private readonly ITextBufferUndoManagerProvider _textBufferUndoManagerProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;

        public Services(
            IEditorOptionsFactoryService editorOptionsFactory, 
            IEditorOperationsFactoryService editorOperatiosnFactoryService,
            ITextBufferUndoManagerProvider textBufferUndoManagerProvider,
            ITextDocumentFactoryService textDocumentFactoryService)
        {
            _editorOptionsFactory = editorOptionsFactory;
            _editorOperationsFactoryService = editorOperatiosnFactoryService;
            _textBufferUndoManagerProvider = textBufferUndoManagerProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
        }

        public IEditorOptionsFactoryService EditorOptionsFactory
        {
            get { return _editorOptionsFactory; }
        }

        public ITextBufferUndoManagerProvider TextBufferUndoManagerProvider
        {
            get { return _textBufferUndoManagerProvider; }
        }

        public IEditorOperationsFactoryService EditorOperationsFactoryService
        {
            get { return _editorOperationsFactoryService; }
        }

        public ITextDocumentFactoryService TextDocumentFactoryService
        {
            get { return _textDocumentFactoryService; }
        }
    }
}
