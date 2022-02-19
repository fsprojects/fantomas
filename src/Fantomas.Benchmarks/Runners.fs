module Fantomas.Benchmarks.Runners

open System.IO
open BenchmarkDotNet.Attributes
open Fantomas

let config = FormatConfig.FormatConfig.Default

[<MemoryDiagnoser>]
[<RankColumn>]
type CodePrinterTest() =
    [<Benchmark>]
    member _.Format() =
        let path =
            __SOURCE_DIRECTORY__
            + "/../../paket-files/fsprojects/fantomas/src/Fantomas/CodePrinter.fs"

        let fileName = Path.GetFileName(path)


        let content =
            File.ReadAllText(path)
            |> SourceOrigin.SourceString

        CodeFormatter.FormatDocumentAsync(fileName, content, config)
        |> Async.RunSynchronously
        |> ignore

        ()
