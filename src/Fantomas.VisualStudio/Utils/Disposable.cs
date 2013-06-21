using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hestia.FSharpCommands.Utils
{
    internal static class Disposable
    {
        internal static IDisposable Create(Action onDispose)
        {
            return new ActionDisposable(onDispose);
        }

        private class ActionDisposable : IDisposable
        {
            private readonly Action _onDispose;

            public ActionDisposable(Action onDispose)
            {
                _onDispose = onDispose;
            }

            public void Dispose()
            {
                _onDispose();
            }
        }

    }
}
