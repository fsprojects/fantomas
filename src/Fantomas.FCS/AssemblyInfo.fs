namespace System

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Fantomas")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let InternalsVisibleTo = "Fantomas"