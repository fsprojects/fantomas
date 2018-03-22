namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("Fantomas.Tests")>]
[<assembly: InternalsVisibleToAttribute("Fantomas.Tests.Core")>]
[<assembly: AssemblyTitleAttribute("FantomasLib")>]
[<assembly: AssemblyProductAttribute("Fantomas")>]
[<assembly: AssemblyDescriptionAttribute("Source code formatter for F#")>]
[<assembly: AssemblyVersionAttribute("2.6.1")>]
[<assembly: AssemblyFileVersionAttribute("2.6.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.6.1"
