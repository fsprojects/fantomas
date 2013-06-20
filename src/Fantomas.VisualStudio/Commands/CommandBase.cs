using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text.Editor;

namespace Hestia.FSharpCommands.Commands
{
    public abstract class CommandBase
    {
        public IWpfTextView TextView { get; set; }

        public Services Services { get; set; }

        public abstract void Execute();
    }
}
