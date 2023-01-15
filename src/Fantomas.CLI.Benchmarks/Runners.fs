module Fantomas.CLI.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open Fantomas.Core
open System.IO

let config = FormatConfig.FormatConfig.Default

let (</>) x y = Path.Combine(x, y)

[<MemoryDiagnoser>]
[<SimpleJob(runStrategy = RunStrategy.ColdStart, targetCount = 3)>]
type ColdStart() =

    let tmpDir = __SOURCE_DIRECTORY__ </> ".." </> ".." </> "tmp"

    //? Should we download these repositories automatically?
    [<Params("FsToolkit.ErrorHandling", "FAKE", "fsharp", "Plotly.NET")>]
    member val projects = "" with get, set

    [<Params(true, false)>]
    member val processInParallel = false with get, set

    [<Benchmark>]
    [<GcServer(true)>]
    member this.FormatCode() =
        let projectDir = tmpDir </> this.projects
        // to use .fantomasignore for these repositories, set the current directory
        Directory.SetCurrentDirectory(projectDir)

        let args =
            Array.append (if not this.processInParallel then [| "--sequential" |] else [||]) [| "-r"; projectDir |]

        Program.main args |> ignore
