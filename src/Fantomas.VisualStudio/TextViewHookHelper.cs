using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;

namespace Hestia.FSharpCommands
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [Name("F# Dummy Command Hook")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public sealed class TextViewHookHelper : IWpfTextViewCreationListener
    {
        private readonly IVsEditorAdaptersFactoryService _adaptersFactory;
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;

        [ImportingConstructor]
        public TextViewHookHelper(IVsEditorAdaptersFactoryService adaptersFactory, IEditorOptionsFactoryService editorOptionsFactory)
        {
            _adaptersFactory = adaptersFactory;
            _editorOptionsFactory = editorOptionsFactory;
        }

        public void TextViewCreated(IWpfTextView wpfTextView)
        {
            System.Windows.Threading.Dispatcher.CurrentDispatcher.BeginInvoke(new Action(() =>
            {
                var view = _adaptersFactory.GetViewAdapter(wpfTextView);
                if (view != null)
                {
                    StandardCommandDispatcher.Register(view, wpfTextView, GetServices());
                }
            }));
        }

        private Services GetServices()
        {
            return new Services(_editorOptionsFactory);
        }
    }
}
