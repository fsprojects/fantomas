open BenchmarkDotNet.Running
open Fantomas.Benchmarks.Runners

[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run<CodePrinterTest>()
    |> ignore
    0
