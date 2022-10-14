module Fantomas.Benchmarks.Runners

open System.IO
open BenchmarkDotNet.Attributes
open Fantomas.Core

let config = FormatConfig.FormatConfig.Default

[<MemoryDiagnoser>]
[<RankColumn>]
type CodePrinterTest() =
    [<Benchmark>]
    member _.Format() =
        let path = __SOURCE_DIRECTORY__ + "/../../test/data/CodePrinter.fs"

        let content = File.ReadAllText(path)

        CodeFormatter.FormatDocumentAsync(false, content, config)
        |> Async.RunSynchronously
        |> ignore

        ()
