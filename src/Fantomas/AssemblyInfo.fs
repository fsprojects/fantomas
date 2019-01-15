namespace System
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleToAttribute("Fantomas.Tests")>]

do ()

module internal AssemblyVersionInformation =
    let [<Literal>] InternalsVisibleTo = "Fantomas.Tests"