namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Fantomas")>]
[<assembly: AssemblyProductAttribute("Fantomas")>]
[<assembly: AssemblyDescriptionAttribute("Source code formatting tool for F#")>]
[<assembly: AssemblyVersionAttribute("2.1.0")>]
[<assembly: AssemblyFileVersionAttribute("2.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.1.0"
