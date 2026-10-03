(*---
fsharp_multiline_bracket_style = cramped
---*)
namespace Blah

open System

module Test =
    type ISomething =
        inherit IDisposable
        abstract DoTheThing: string -> unit

    let test (something: IDisposable) (somethingElse: IDisposable) =
        { new ISomething with

            member __.DoTheThing whatever =
                printfn "%s" whatever
                printfn "%s" whatever

            member __.Dispose() =
                something.Dispose()
                somethingElse.Dispose() }
