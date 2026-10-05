namespace Fantomas.Core.Tests

open NUnit.Framework
open Fantomas.FCS.Text

/// Parses once, on one thread, before NUnit runs any test in parallel.
///
/// With `--realsig+` every module of the compiler's `range.fs` is a class with a static
/// constructor of its own, and each of them runs the one initializer of the whole file. A thread
/// that enters through `FileIndex`, as the first parse does, and a thread that enters through
/// `Range`, as a test reading `Range.range0` does, can each hold the lock the other waits on. The
/// runtime breaks that cycle by letting one thread go on before the file is initialized, and a
/// parse then finds the file index null. The parser caches that failure, so every parse of the run
/// fails with it. Once the file is initialized, nothing is left to race.
[<SetUpFixture>]
type ParserWarmupFixture() =

    [<OneTimeSetUp>]
    member _.Warmup() : unit =
        Fantomas.FCS.Parse.parseFile false (SourceText.ofString "") [] |> ignore
