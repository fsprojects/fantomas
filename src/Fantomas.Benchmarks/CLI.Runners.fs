module Fantomas.CLI.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open Fantomas.Core
open System.IO
open BenchmarkDotNet.Configs

let config = FormatConfig.Default

let (</>) x y = Path.Combine(x, y)

[<MemoryDiagnoser>]
[<RankColumn>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByParams)>]
[<SimpleJob(runStrategy = RunStrategy.ColdStart, targetCount = 20)>]
type ParallelVsSequentialFormatting() =

    let tmpDir = __SOURCE_DIRECTORY__ </> ".." </> ".." </> "tmp"

    let formatCode processInParallel project =
        let projectDir = tmpDir </> project
        // to use .fantomasignore for these repositories, set the current directory
        Directory.SetCurrentDirectory(projectDir)

        let args =
            Array.append (if not processInParallel then [| "--sequential" |] else [||]) [| "-r"; projectDir |]

        Program.main args |> ignore

    // Currently have to manually pull these repositories
    [<Params("FsToolkit.ErrorHandling", "FAKE", "fsharp", "Plotly.NET")>]
    member val projects = "" with get, set

    [<IterationSetup>]
    member this.Setup() =
        let projectDir = tmpDir </> this.projects
        printfn $"git reset --hard  {projectDir}"

        let psi =
            System.Diagnostics.ProcessStartInfo("git", "reset --hard", WorkingDirectory = projectDir)

        psi.RedirectStandardError <- true
        psi.RedirectStandardOutput <- true
        let p = System.Diagnostics.Process.Start(psi)
        p.WaitForExit()
        let output = p.StandardOutput.ReadToEnd()
        let err = p.StandardError.ReadToEnd()
        printfn "%s" (output)
        printfn "%s" (err)

    [<Benchmark(Baseline = true)>]
    member this.FormatCode_Sequentially() = formatCode false this.projects

    [<Benchmark>]
    member this.FormatCode_Parallel() = formatCode true this.projects
