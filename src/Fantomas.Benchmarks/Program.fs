open BenchmarkDotNet.Running
open Fantomas.Benchmarks.Runners
open Fantomas.CLI.Benchmarks.Runners

[<EntryPoint>]
let main _ =

    // BenchmarkRunner.Run<ParallelVsSequentialFormatting>() |> ignore
    BenchmarkRunner.Run<CodePrinterTest>() |> ignore
    0
