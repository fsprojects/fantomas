namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Fantomas")>]
[<assembly: AssemblyProductAttribute("Fantomas")>]
[<assembly: AssemblyDescriptionAttribute("Source code formatting tool for F#")>]
[<assembly: AssemblyVersionAttribute("1.10.0")>]
[<assembly: AssemblyFileVersionAttribute("1.10.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.10.0"
