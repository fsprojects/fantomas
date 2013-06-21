using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text.Editor;

namespace Hestia.FSharpCommands
{
    public class Services
    {
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;

        public Services(IEditorOptionsFactoryService editorOptionsFactory)
        {
            _editorOptionsFactory = editorOptionsFactory;
        }

        public IEditorOptionsFactoryService EditorOptionsFactory
        {
            get { return _editorOptionsFactory; }
        }
    }
}
