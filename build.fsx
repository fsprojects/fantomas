#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators
open System
open System.IO
open Fake.DotNet

let configuration = DotNet.BuildConfiguration.Release

// (<solutionFile>.sln is built during the building process)
let solutionFile = "fantomas.sln"
//// Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas.FCS/bin"
      "src/Fantomas.FCS/obj"
      "src/Fantomas.Core/bin"
      "src/Fantomas.Core/obj"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.Client/bin"
      "src/Fantomas.Client/obj" ]
    |> List.iter Shell.cleanDir)

// --------------------------------------------------------------------------------------
// Build library & test project
Target.create "Build" (fun _ -> DotNet.build (fun p -> { p with Configuration = configuration }) solutionFile)

Target.create "UnitTests" (fun _ ->
    DotNet.test
        (fun p ->
            { p with
                Configuration = configuration
                NoRestore = true
                NoBuild = true
            // TestAdapterPath = Some "."
            // Logger = Some "nunit;LogFilePath=../../TestResults.xml"
            // Current there is an issue with NUnit reporter, https://github.com/nunit/nunit3-vs-adapter/issues/589
             })
        "src/Fantomas.Core.Tests/Fantomas.Core.Tests.fsproj"

    DotNet.test
        (fun p ->
            { p with
                Configuration = configuration
                NoRestore = true
                NoBuild = true
            // TestAdapterPath = Some "."
            // Logger = Some "nunit;LogFilePath=../../TestResults.xml"
            // Current there is an issue with NUnit reporter, https://github.com/nunit/nunit3-vs-adapter/issues/589
             })
        "src/Fantomas.Tests/Fantomas.Tests.fsproj")

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "Pack" (fun _ ->
    DotNet.pack
        (fun p ->
            { p with
                NoRestore = true
                Configuration = configuration
                OutputPath = Some "./bin" })
        solutionFile)

// Workaround for https://github.com/fsharp/FAKE/issues/2242
let pushPackage nupkg =
    let args = [ yield "push"; yield nupkg ]

    CreateProcess.fromRawCommand "dotnet" ("paket" :: args)
    |> CreateProcess.disableTraceCommand
    |> CreateProcess.redirectOutput
    |> CreateProcess.withOutputEventsNotNull Trace.trace Trace.traceError
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

Target.create "Push" (fun _ ->
    Directory.EnumerateFiles("bin", "*.nupkg", SearchOption.TopDirectoryOnly)
    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
    |> Seq.iter pushPackage)

Target.create "PushClient" (fun _ ->
    Directory.EnumerateFiles("bin", "Fantomas.Client.*.nupkg", SearchOption.TopDirectoryOnly)
    |> Seq.tryExactlyOne
    |> Option.iter pushPackage)

let git command =
    CreateProcess.fromRawCommandLine "git" command
    |> CreateProcess.redirectOutput
    |> Proc.run
    |> fun p -> p.Result.Output.Trim()

open Microsoft.Azure.Cosmos.Table

Target.create "Benchmark" (fun _ ->
    DotNet.exec
        id
        ("src"
         </> "Fantomas.Benchmarks"
         </> "bin"
         </> "Release"
         </> "net6.0"
         </> "Fantomas.Benchmarks.dll")
        ""
    |> ignore

// TODO: the Azure storage account was removed, migrate this to a new solution.
)

Target.create "Format" (fun _ ->
    let result = DotNet.exec id "fantomas" "--recurse src docs build.fsx"

    if not result.OK then
        printfn "Errors while formatting all files: %A" result.Messages)

Target.create "FormatChanged" (fun _ ->
    let result =
        Fake.Tools.Git.FileStatus.getChangedFilesInWorkingCopy "." "HEAD"
        |> Seq.choose (fun (_, file) ->
            let ext = Path.GetExtension(file)

            if file.StartsWith("src") && (ext = ".fs" || ext = ".fsi") then
                Some(sprintf "\"%s\"" file)
            else
                None)
        |> String.concat " "
        |> DotNet.exec id "fantomas"

    if not result.OK then
        printfn "Problem when formatting changed files:\n\n%A" result.Errors)

Target.create "CheckFormat" (fun _ ->
    let result = DotNet.exec id "fantomas" "--check --recurse src docs build.fsx"

    if result.ExitCode = 0 then
        Trace.log "No files need formatting"
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, run `dotnet fake build -t Format` to format them"
    else
        Trace.logf "Errors while formatting: %A" result.Errors
        failwith "Unknown errors while formatting")

Target.create "EnsureRepoConfig" (fun _ ->
    // Configure custom git hooks
    // * Currently only used to ensure that code is formatted before pushing
    Fake.Tools.Git.CommandHelper.gitCommand "" "config core.hooksPath .githooks")

Target.create "Docs" (fun _ ->
    let compileResult = DotNet.exec id "fsi" "./docs/.style/style.fsx"

    let semanticVersioning =
        __SOURCE_DIRECTORY__
        </> "src"
        </> "Fantomas"
        </> "bin"
        </> string configuration
        </> "net6.0"
        </> "SemanticVersioning.dll"

    DotNet.exec
        id
        "fsdocs"
        $"build --clean --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval"
    |> ignore)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean"
==> "CheckFormat"
==> "Build"
==> "UnitTests"
==> "Benchmark"
==> "Pack"
==> "Docs"
==> "All"
==> "Push"

Target.runOrDefault "All"
