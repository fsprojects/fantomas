module Fantomas.CLI.Benchmarks.Program

open System.Reflection
open BenchmarkDotNet.Running
open Fantomas.CLI.Benchmarks.Runners

[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run(Assembly.GetAssembly(typeof<ColdStart>)) |> ignore
    //ColdStart().CheckInParallelServerGc()
    0
