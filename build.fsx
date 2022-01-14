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

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitHome = "https://github.com/fsprojects"
// The name of the project on GitHub
let gitName = "fantomas"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "Fantomas"

let projectUrl = sprintf "%s/%s" gitHome gitName

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Source code formatter for F#"

let copyright = sprintf "Copyright © %d" DateTime.UtcNow.Year

let configuration = DotNet.BuildConfiguration.Release

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description =
    """This library aims at formatting F# source files based on a given configuration.
Fantomas will ensure correct indentation and consistent spacing between elements in the source files.
Some common use cases include
(1) Reformatting a code base to conform a universal page width
(2) Converting legacy code from verbose syntax to light syntax
(3) Formatting auto-generated F# signatures."""

// List of author names (for NuGet package)
let authors =
    [ "Florian Verdonck"
      "Jindřich Ivánek" ]

let owner = "Anh-Dung Phan"
// Tags for your project (for NuGet package)
let tags = "F# fsharp formatting beautifier indentation indenter"

let fantomasClientVersion = "0.5.1"

// (<solutionFile>.sln is built during the building process)
let solutionFile = "fantomas"
//// Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = ReleaseNotes.parse (File.ReadAllLines "RELEASE_NOTES.md")

// Files to format
let sourceFiles =
    !! "src/**/*.fs" ++ "src/**/*.fsi" ++ "build.fsx"
    -- "src/**/obj/**/*.fs"

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
          Arguments =
            [ sprintf "%s/%s.sh" pathToProject scriptPrefix
              argument ] }

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
      Arguments = [ sprintf "%s/src/Fantomas.CoreGlobalTool/bin/%s/net6.0/fantomas-tool.dll" projectdir configuration ] }

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
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj"
      "src/Fantomas.Client/bin"
      "src/Fantomas.Client/obj" ]
    |> List.iter Shell.cleanDir)

Target.create "ProjectVersion" (fun _ ->
    let version = release.NugetVersion

    let setProjectVersion project =
        let file = sprintf "src/%s/%s.fsproj" project project

        Xml.poke
            file
            "Project/PropertyGroup/Version/text()"
            (if project = "Fantomas.Client" then
                 fantomasClientVersion
             else
                 version)

    setProjectVersion "Fantomas"
    setProjectVersion "Fantomas.CoreGlobalTool"
    setProjectVersion "Fantomas.CoreGlobalTool.Tests"
    setProjectVersion "Fantomas.Tests"
    setProjectVersion "Fantomas.Extras"
    setProjectVersion "Fantomas.Client")

// --------------------------------------------------------------------------------------
// Build library & test project
Target.create "Build" (fun _ ->
    let sln = sprintf "%s.sln" solutionFile
    DotNet.build (fun p -> { p with Configuration = configuration }) sln)

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
        "src/Fantomas.Tests/Fantomas.Tests.fsproj"

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
        "src/Fantomas.CoreGlobalTool.Tests/Fantomas.CoreGlobalTool.Tests.fsproj")

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "Pack" (fun _ ->
    let nugetVersion = release.NugetVersion

    let pack project =
        let projectPath = sprintf "src/%s/%s.fsproj" project project

        let args =
            let defaultArgs = MSBuild.CliArguments.Create()

            { defaultArgs with
                Properties =
                    [ "Title", project
                      "PackageVersion",
                      (if project = "Fantomas.Client" then
                           fantomasClientVersion
                       else
                           nugetVersion)
                      "Authors", (String.Join(" ", authors))
                      "Owners", owner
                      "PackageRequireLicenseAcceptance", "false"
                      "Description", description
                      "Summary", summary
                      "PackageReleaseNotes", ((String.toLines release.Notes).Replace(",", ""))
                      "Copyright", copyright
                      "PackageTags", tags
                      "PackageProjectUrl", projectUrl ] }

        DotNet.pack
            (fun p ->
                { p with
                    NoRestore = true
                    Configuration = configuration
                    OutputPath = Some "./bin"
                    MSBuildParams = args })
            projectPath

    pack "Fantomas"
    pack "Fantomas.Extras"
    pack "Fantomas.CoreGlobalTool"
    pack "Fantomas.Client")

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
                fantomasStartInfo.Arguments
                @ [ "--recurse"
                    project.SourceSubDirectory ]

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

    match Environment.environVarOrNone "TABLE_STORAGE_CONNECTION_STRING" with
    | Some conn ->
        let branchName = git "rev-parse --abbrev-ref HEAD"
        let commit = git "rev-parse HEAD"
        let operatingSystem = Environment.environVar "RUNNER_OS"

        let results =
            File.ReadLines("./BenchmarkDotNet.Artifacts/results/Fantomas.Benchmarks.Runners.CodePrinterTest-report.csv")
            |> Seq.map (fun line -> line.Split(',') |> Array.toList)
            |> Seq.toList
            |> fun lineGroups ->
                match lineGroups with
                | [ header; values ] ->
                    let csvValues = List.zip header values

                    let metaData =
                        [ "Branch", branchName
                          "Commit", commit
                          "Operating System", operatingSystem ]

                    [ yield! csvValues; yield! metaData ]
                | _ -> []

        let storageAccount = CloudStorageAccount.Parse(conn)
        let tableClient = storageAccount.CreateCloudTableClient()

        let table = tableClient.GetTableReference("FantomasBenchmarks")

        let entry = DynamicTableEntity()
        entry.PartitionKey <- "GithubActions"

        entry.RowKey <-
            (sprintf "%s|%s|%s" branchName commit operatingSystem)
                .ToLower()

        results
        |> List.iter (fun (k, v) ->
            let key = k.Replace(' ', '_')

            if not (isNull v) then
                entry.Properties.Add(key, EntityProperty.CreateEntityPropertyFromObject(v)))

        let tableOperation = TableOperation.InsertOrReplace(entry)

        table.Execute(tableOperation) |> printfn "%O"
    | None -> printfn "Not saving benchmark results to the cloud")

Target.create "Format" (fun _ ->
    let result =
        sourceFiles
        |> Seq.map (sprintf "\"%s\"")
        |> String.concat " "
        |> DotNet.exec id "fantomas"

    if not result.OK then
        printfn "Errors while formatting all files: %A" result.Messages)

Target.create "FormatChanged" (fun _ ->
    let result =
        Fake.Tools.Git.FileStatus.getChangedFilesInWorkingCopy "." "HEAD"
        |> Seq.choose (fun (_, file) ->
            let ext = Path.GetExtension(file)

            if
                file.StartsWith("src")
                && (ext = ".fs" || ext = ".fsi")
            then
                Some(sprintf "\"%s\"" file)
            else
                None)
        |> String.concat " "
        |> DotNet.exec id "fantomas"

    if not result.OK then
        printfn "Problem when formatting changed files:\n\n%A" result.Errors)

Target.create "CheckFormat" (fun _ ->
    let result =
        sourceFiles
        |> Seq.map (sprintf "\"%s\"")
        |> String.concat " "
        |> sprintf "%s --check"
        |> DotNet.exec id "fantomas"

    if result.ExitCode = 0 then
        Trace.log "No files need formatting"
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, run `dotnet fake build -t Format` to format them"
    else
        Trace.logf "Errors while formatting: %A" result.Errors)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean"
==> "ProjectVersion"
==> "CheckFormat"
==> "Build"
==> "UnitTests"
==> "Benchmark"
==> "Pack"
==> "All"
==> "Push"

"Build" ==> "TestExternalProjects"

Target.runOrDefault "All"
