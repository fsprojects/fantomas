using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;

namespace Hestia.FSharpCommands
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [ProvideOptionPage(typeof(FantomasOptionsPage), "Fantomas", "Formatting", 0, 0, false, 0)]
    [Guid("684211D1-B47C-44FE-AECF-E9D3B5FF67E3")]
    [ProvideService(typeof(FantomasOptionsPage))]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.FSharpProject_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.NoSolution_string)]
    public class FSharpVisualStudioCommandsPackage : Package
    {
        private static FSharpVisualStudioCommandsPackage _instance;

        public FSharpVisualStudioCommandsPackage()
        {
            _instance = this;
        }

        protected override void Initialize()
        {
            base.Initialize();

            IServiceContainer serviceContainer = this;
            serviceContainer.AddService(typeof(FantomasOptionsPage),
              delegate { return GetDialogPage(typeof(FantomasOptionsPage)); }, true);
        }
    }
}
