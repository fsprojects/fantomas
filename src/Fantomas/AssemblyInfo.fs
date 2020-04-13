namespace System

open System.Runtime.CompilerServices

[<assembly:InternalsVisibleTo("Fantomas.Tests")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let InternalsVisibleTo = "Fantomas.Tests"