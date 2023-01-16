module Fantomas.CLI.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open Fantomas.Core
open System.IO
open BenchmarkDotNet.Configs

let config = FormatConfig.FormatConfig.Default

let (</>) x y = Path.Combine(x, y)

[<MemoryDiagnoser>]
[<RankColumn>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByParams)>]
[<SimpleJob(runStrategy = RunStrategy.ColdStart, targetCount = 3)>]
type ColdStart() =

    let tmpDir = __SOURCE_DIRECTORY__ </> ".." </> ".." </> "tmp"

    let formatCode processInParallel project =
        let projectDir = tmpDir </> project
        // to use .fantomasignore for these repositories, set the current directory
        Directory.SetCurrentDirectory(projectDir)

        let args =
            Array.append (if not processInParallel then [| "--sequential" |] else [||]) [| "-r"; projectDir |]

        Program.main args |> ignore

    //? Should we download these repositories automatically?
    [<Params("FsToolkit.ErrorHandling", "FAKE", "fsharp", "Plotly.NET")>]
    member val projects = "" with get, set

    [<IterationSetup>]
    member this.Setup() =
        let projectDir = tmpDir </> this.projects

        let psi =
            System.Diagnostics.ProcessStartInfo("git", "--reset HARD", WorkingDirectory = projectDir)

        let p = System.Diagnostics.Process.Start(psi)
        p.WaitForExit()

    [<Benchmark(Baseline = true)>]
    member this.FormatCode_Sequentially() = formatCode false this.projects

    [<Benchmark>]
    member this.FormatCode_Parallel() = formatCode true this.projects
