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

// Types and helper functions for building external projects (see the TestExternalProjects target below)
type ProcessStartInfo =
    { ProcessName: string
      Arguments: string list }

[<NoComparison>]
[<NoEquality>]
type ExternalProjectInfo =
    { GitUrl: string
      DirectoryName: string
      Tag: string
      SourceSubDirectory: string
      BuildConfigurationFn: string -> ProcessStartInfo }

// Construct the commands/arguments for running an external project build script for both windows and linux
// For linux we run this by invoking sh explicitly and passing the build.sh script as an argument as some
// projects generated on windows don't have the executable permission set for .sh scripts. On windows we
// treat .cmd files as executable
let configureBuildCommandFromFakeBuildScripts scriptPrefix argument pathToProject =
    if Environment.isWindows then
        { ProcessName = Path.combine pathToProject (sprintf "%s.cmd" scriptPrefix)
          Arguments = [ argument ] }
    else
        { ProcessName = "sh"
          Arguments = [ sprintf "%s/%s.sh" pathToProject scriptPrefix; argument ] }

let configureBuildCommandFromDefaultFakeBuildScripts pathToProject =
    configureBuildCommandFromFakeBuildScripts "build" "Build" pathToProject

let configureBuildCommandDotnetBuild pathToProject =
    { ProcessName = "dotnet"
      Arguments = [ "build"; pathToProject ] }

// Construct the path of the fantomas executable to use for external project tests
let fantomasExecutableForExternalTests projectdir =
    let configuration =
        match configuration with
        | DotNet.BuildConfiguration.Debug -> "Debug"
        | DotNet.BuildConfiguration.Release -> "Release"
        | DotNet.BuildConfiguration.Custom s -> s

    { ProcessName = "dotnet"
      Arguments = [ $"%s{projectdir}/src/Fantomas/bin/%s{configuration}/net6.0/fantomas.dll" ] }

let externalProjectsToTest =
    [
      //    { GitUrl = @"https://github.com/fsprojects/Argu"
      //      DirectoryName = "Argu"
      //      Tag = "5.1.0"
      //      SourceSubDirectory = "src"
      //      BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/jack-pappas/ExtCore"
        DirectoryName = "ExtCore"
        Tag = "master"
        SourceSubDirectory = "."
        BuildConfigurationFn = configureBuildCommandDotnetBuild } ]

let externalProjectsToTestFailing =
    [ { GitUrl = @"https://github.com/fsprojects/Chessie"
        DirectoryName = "Chessie"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fscheck/FsCheck"
        DirectoryName = "FsCheck"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fsprojects/fantomas"
        DirectoryName = "Fantomas"
        Tag = "v2.9.0"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/SAFE-Stack/SAFE-BookStore"
        DirectoryName = "SAFE-BookStore"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fsprojects/Paket"
        DirectoryName = "Paket"
        Tag = "5.181.1"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fsprojects/FSharpPlus"
        DirectoryName = "FSharpPlus"
        Tag = "v1.0.0"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/MangelMaxime/fulma-demo"
        DirectoryName = "fulma-demo"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromFakeBuildScripts "fake" "build" } ]

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

// This takes the list of external projects defined above, does a git checkout of the specified repo and tag,
// tries to build the project, then reformats with fantomas and tries to build the project again. If this fails
// then there was a regression in fantomas that mangles the source code
let testExternalProjects externalProjectsToTest =
    let externalBuildErrors =
        let project = Environment.environVar "project"

        externalProjectsToTest
        |> if String.IsNullOrWhiteSpace(project) then
               id
           else
               List.filter (fun p -> p.DirectoryName = project)
        |> List.map (fun project ->
            let relativeProjectDir = sprintf "external-project-tests/%s" project.DirectoryName

            Shell.cleanDir relativeProjectDir
            // Use "shallow" clone by setting depth to 1 to only check out the one commit we want to build
            Fake.Tools.Git.CommandHelper.gitCommand
                "."
                (sprintf "clone --branch %s --depth 1 %s %s" project.Tag project.GitUrl relativeProjectDir)

            let fullProjectPath = sprintf "%s/%s" __SOURCE_DIRECTORY__ relativeProjectDir

            let buildStartInfo = project.BuildConfigurationFn fullProjectPath

            let buildExternalProject () =
                buildStartInfo.Arguments
                |> CreateProcess.fromRawCommand buildStartInfo.ProcessName
                |> CreateProcess.withWorkingDirectory relativeProjectDir
                |> CreateProcess.withTimeout (TimeSpan.FromMinutes 5.0)
                |> Proc.run

            let cleanResult = buildExternalProject ()

            if cleanResult.ExitCode <> 0 then
                failwithf
                    "Initial build of external project %s returned with a non-zero exit code"
                    project.DirectoryName

            let fantomasStartInfo = fantomasExecutableForExternalTests __SOURCE_DIRECTORY__

            let arguments =
                fantomasStartInfo.Arguments @ [ "--recurse"; project.SourceSubDirectory ]

            let invokeFantomas () =
                CreateProcess.fromRawCommand fantomasStartInfo.ProcessName arguments
                |> CreateProcess.withWorkingDirectory (sprintf "%s/%s" __SOURCE_DIRECTORY__ relativeProjectDir)
                |> CreateProcess.withTimeout (TimeSpan.FromMinutes 5.0)
                |> Proc.run

            let fantomasResult = invokeFantomas ()

            if fantomasResult.ExitCode <> 0 then
                Some
                <| sprintf "Fantomas invokation for %s returned with a non-zero exit code" project.DirectoryName
            else
                let formattedResult = buildExternalProject ()

                if formattedResult.ExitCode <> 0 then
                    Some
                    <| sprintf
                        "Build of external project after fantomas formatting failed for project %s"
                        project.DirectoryName
                else
                    printfn "Successfully built %s after reformatting" project.DirectoryName
                    None)
        |> List.choose id

    if not (List.isEmpty externalBuildErrors) then
        failwith (String.Join("\n", externalBuildErrors))

Target.create "TestExternalProjects" (fun _ -> testExternalProjects externalProjectsToTest)

Target.create "TestExternalProjectsFailing" (fun _ -> testExternalProjects externalProjectsToTestFailing)

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
    let result = DotNet.exec id "fantomas" "--recurse src docs/.style/ build.fsx"

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
    let result =
        DotNet.exec id "fantomas" "--check --recurse src docs/.style/ build.fsx"

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
    DotNet.exec id "fsi" "./docs/.style/style.fsx" |> ignore

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
        $"build --clean --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\""
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

"Build" ==> "TestExternalProjects"

Target.runOrDefault "All"
