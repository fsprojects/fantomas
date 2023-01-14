module Fantomas.CLI.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open Fantomas.Core
open System.IO

let config = FormatConfig.FormatConfig.Default

let (</>) x y = Path.Combine(x, y)

[<MemoryDiagnoser>]
[<RankColumn>]
[<SimpleJob(runStrategy = RunStrategy.ColdStart, targetCount = 1)>]
type ColdStart() =

    let tmpDir = __SOURCE_DIRECTORY__ </> ".." </> ".." </> "tmp"

    [<Params("FsToolkit.ErrorHandling", "FAKE", "fsharp", "Plotly.NET")>]
    member val projects = "" with get, set

    [<Params(true, false)>]
    member val processInParallel = false with get, set

    [<Benchmark>]
    [<GcServer(true)>]
    member this.FormatCode() =
        let projectDir = tmpDir </> this.projects
        Directory.SetCurrentDirectory(projectDir)

        let args =
            Array.append (if this.processInParallel then [| "--parallel" |] else [||]) [| "-r"; projectDir |]

        Program.main args |> ignore
