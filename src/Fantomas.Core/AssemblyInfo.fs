namespace Fantomas.Core

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Fantomas.Core.Tests")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let InternalsVisibleTo: string = "Fantomas.Tests"
