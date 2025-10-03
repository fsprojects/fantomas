namespace Fantomas.Core

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Fantomas.Core.Tests")>]
[<assembly: InternalsVisibleTo("FSI-ASSEMBLY")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let InternalsVisibleTo = "Fantomas.Tests"
