using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hestia.FSharpCommands.Commands;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Hestia.FSharpCommands
{
    public class StandardCommandDispatcher : IOleCommandTarget
    {
        private IWpfTextView _textView;
        private Services _services;
        private IOleCommandTarget _commandChain;

        public static void Register(IVsTextView interopTextView, IWpfTextView textView, Services services)
        {
            var dispatcher = new StandardCommandDispatcher();
            dispatcher._textView = textView;
            dispatcher._services = services;
            interopTextView.AddCommandFilter(dispatcher, out dispatcher._commandChain);
        }

        private StandardCommandDispatcher()
        {
        }

        private static readonly CommandMapping[] Commands = new CommandMapping[]
        {
            new CommandMapping(typeof(VSConstants.VSStd2KCmdID).GUID, (int)(VSConstants.VSStd2KCmdID.FORMATDOCUMENT), OLECMDF.OLECMDF_ENABLED | OLECMDF.OLECMDF_SUPPORTED, () => new FormatDocumentCommand()),
            new CommandMapping(typeof(VSConstants.VSStd2KCmdID).GUID, (int)(VSConstants.VSStd2KCmdID.FORMATSELECTION), OLECMDF.OLECMDF_ENABLED | OLECMDF.OLECMDF_SUPPORTED, () => new FormatSelectionCommand()),
        };

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            foreach (var commandMapping in Commands)
            {
                int fmtindex = commandMapping.GetMatchingIndex(pguidCmdGroup, prgCmds);
                if (fmtindex >= 0)
                {
                    prgCmds[fmtindex].cmdf = commandMapping.CommandOptions;
                    return VSConstants.S_OK;
                }
            }

            return _commandChain.QueryStatus(pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            foreach (var commandMapping in Commands)
            {
                if (commandMapping.Matches(pguidCmdGroup, nCmdID))
                {
                    var command = commandMapping.CreateCommand();
                    command.TextView = _textView;
                    command.Services = _services;
                    command.Execute();
                    return VSConstants.S_OK;
                }
            }

            return _commandChain.Exec(pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);
        }

        private class CommandMapping
        {
            private readonly Guid _commandGroup;
            private readonly uint _commandId;
            private readonly uint _commandOptions;
            private readonly Func<CommandBase> _commandCreator;

            public CommandMapping(Guid commandGroup, int commandId, OLECMDF commandOptions, Func<CommandBase> commandCreator)
            {
                _commandGroup = commandGroup;
                _commandId = (uint)commandId;
                _commandOptions = (uint)commandOptions;
                _commandCreator = commandCreator;
            }

            public bool Matches(Guid commandGroup, uint commandId)
            {
                return commandGroup == _commandGroup && commandId == _commandId;
            }

            public int GetMatchingIndex(Guid commandGroup, OLECMD[] commands)
            {
                if (commandGroup == _commandGroup)
                {
                    return Array.FindIndex(commands, cmd => cmd.cmdID == _commandId);
                }
                return -1;
            }

            public uint CommandOptions
            {
                get { return _commandOptions; }
            }

            public CommandBase CreateCommand()
            {
                return _commandCreator();
            }
        }
    }
}
