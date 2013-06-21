using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hestia.FSharpCommands.Utils
{
    internal static class Cursor
    {
        internal static IDisposable Wait()
        {
            var currentCursor = System.Windows.Forms.Cursor.Current;
            System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.WaitCursor;
            return Disposable.Create(() => System.Windows.Forms.Cursor.Current = currentCursor);
        }
    }
}
