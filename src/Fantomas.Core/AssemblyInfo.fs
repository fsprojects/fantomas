namespace Fantomas.Core

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Fantomas.Core.Tests")>]
[<assembly: InternalsVisibleTo("Fantomas.MCP")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let InternalsVisibleTo = "Fantomas.Tests"
