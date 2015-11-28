namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Fantomas")>]
[<assembly: AssemblyProductAttribute("Fantomas")>]
[<assembly: AssemblyDescriptionAttribute("Source code formatting tool for F#")>]
[<assembly: AssemblyVersionAttribute("2.0.2")>]
[<assembly: AssemblyFileVersionAttribute("2.0.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.2"
