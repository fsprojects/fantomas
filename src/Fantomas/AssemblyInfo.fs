namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("Fantomas.Tests")>]
[<assembly: AssemblyTitleAttribute("FantomasLib")>]
[<assembly: AssemblyProductAttribute("Fantomas")>]
[<assembly: AssemblyDescriptionAttribute("Source code formatter for F#")>]
[<assembly: AssemblyVersionAttribute("2.4.0")>]
[<assembly: AssemblyFileVersionAttribute("2.4.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.4.0"
