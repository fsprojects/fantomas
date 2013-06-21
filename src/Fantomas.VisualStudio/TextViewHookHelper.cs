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
    // This class doesn't actually do any mouse processing -- it exists
    // solely as a way to hook the Visual Studio command chain.  There
    // is undoubtedly a correct way of doing it from a simple VSIX, but
    // it's eluded me so far.

    [Export(typeof(IMouseProcessorProvider))]
    [Name("F# Dummy Command Hook")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class TextViewHookHelper : IMouseProcessorProvider
    {
        [Import]
        internal IVsEditorAdaptersFactoryService AdaptersFactory { get; set; }

        [Import]
        internal IEditorOptionsFactoryService EditorOptionsFactory { get; set; }

        public IMouseProcessor GetAssociatedProcessor(IWpfTextView wpfTextView)
        {
            System.Windows.Threading.Dispatcher.CurrentDispatcher.BeginInvoke(new Action(() =>
            {
                var view = AdaptersFactory.GetViewAdapter(wpfTextView);
                if (view != null)
                {
                    StandardCommandDispatcher.Register(view, wpfTextView, GetServices());
                }
            }));

            return null;
        }

        private Services GetServices()
        {
            return new Services(EditorOptionsFactory);
        }
    }
}
