#r "nuget: Fun.Build, 0.1.6"
#r "nuget: CliWrap, 3.5.0"

open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered

let (</>) a b = Path.Combine(a, b)

let cleanFolders (input: string seq) =
    async {
        input
        |> Seq.iter (fun dir ->
            if Directory.Exists(dir) then
                Directory.Delete(dir, true))
    }

let benchmarkAssembly =
    "src"
    </> "Fantomas.Benchmarks"
    </> "bin"
    </> "Release"
    </> "net6.0"
    </> "Fantomas.Benchmarks.dll"

let semanticVersioning =
    __SOURCE_DIRECTORY__
    </> "src"
    </> "Fantomas"
    </> "bin"
    </> "Release"
    </> "net6.0"
    </> "SemanticVersioning.dll"

let pushPackage nupkg =
    async {
        let! result =
            Cli.Wrap("dotnet").WithArguments($"paket push {nupkg}").ExecuteAsync().Task
            |> Async.AwaitTask

        return result.ExitCode
    }

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__

    stage "Clean" {
        run (
            cleanFolders
                [| "bin"
                   "src/Fantomas.FCS/bin"
                   "src/Fantomas.FCS/obj"
                   "src/Fantomas.Core/bin"
                   "src/Fantomas.Core/obj"
                   "src/Fantomas/bin"
                   "src/Fantomas/obj"
                   "src/Fantomas.Client/bin"
                   "src/Fantomas.Client/obj" |]
        )
    }

    stage "CheckFormat" { run "dotnet fantomas src docs build.fsx --recurse --check" }

    stage "Build" { run "dotnet build -c Release" }

    stage "UnitTests" { run "dotnet test -c Release" }

    stage "Benchmark" { run $"dotnet {benchmarkAssembly}" }

    stage "Pack" { run "dotnet pack --no-restore -c Release -o ./bin" }

    stage "Docs" {
        run "dotnet fsi ./docs/.style/style.fsx"

        run
            $"dotnet fsdocs build --clean --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval"
    }

    stage "Push" {
        whenCmdArg "--push"

        run (fun _ ->
            async {
                let! exitCodes =
                    Directory.EnumerateFiles("bin", "*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
                    |> Seq.map pushPackage
                    |> Async.Sequential

                return Seq.max exitCodes
            })
    }

    runIfOnlySpecified false
}

let runGitCommand (arguments: string) =
    async {
        let! result =
            Cli.Wrap("git").WithArguments(arguments).WithWorkingDirectory(
                __SOURCE_DIRECTORY__
            )
                .ExecuteBufferedAsync()
                .Task
            |> Async.AwaitTask

        return result.ExitCode, result.StandardOutput, result.StandardError
    }

let runCmd file (arguments: string) =
    async {
        let! result = Cli.Wrap(file).WithArguments(arguments).ExecuteAsync().Task |> Async.AwaitTask
        return result.ExitCode
    }

pipeline "FormatChanged" {
    workingDir __SOURCE_DIRECTORY__

    stage "Format" {
        run (fun _ ->
            async {
                let! exitCode, stdout, stdErr = runGitCommand "status --porcelain"

                if exitCode <> 0 then
                    return exitCode
                else
                    let files =
                        stdout.Split('\n')
                        |> Array.choose (fun line ->
                            let line = line.Trim()

                            if
                                (line.StartsWith("AM") || line.StartsWith("M"))
                                && (line.EndsWith(".fs") || line.EndsWith(".fsx") || line.EndsWith(".fsi"))
                            then
                                Some(line.Replace("AM ", "").Replace("M ", ""))
                            else
                                None)
                        |> String.concat " "

                    return! runCmd "dotnet" $"fantomas {files}"
            })
    }

    runIfOnlySpecified true
}

pipeline "PushClient" {
    stage "Push" {
        run (fun _ ->
            async {
                return!
                    Directory.EnumerateFiles("bin", "Fantomas.Client.*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.tryExactlyOne
                    |> Option.map pushPackage
                    |> Option.defaultValue (
                        async {
                            printfn "Fantomas.Client package was not found."
                            return -1
                        }
                    )
            })
    }
}
