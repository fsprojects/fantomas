namespace System

open System.Runtime.CompilerServices

[<assembly:InternalsVisibleToAttribute("Fantomas.Tests")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let InternalsVisibleTo = "Fantomas.Tests"