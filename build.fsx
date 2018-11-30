// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.ReleaseNotesHelper
open System


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

let copyright = "Copyright \169 2018"
let iconUrl = "https://raw.githubusercontent.com/fsprojects/fantomas/master/fantomas_logo.png"
let licenceUrl = "https://github.com/fsprojects/fantomas/blob/master/LICENSE.md"
let configuration = "Release"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = """This library aims at formatting F# source files based on a given configuration.
Fantomas will ensure correct indentation and consistent spacing between elements in the source files.
Some common use cases include
(1) Reformatting a code base to conform a universal page width
(2) Converting legacy code from verbose syntax to light syntax
(3) Formatting auto-generated F# signatures."""

// List of author names (for NuGet package)
let authors = [ "Anh-Dung Phan"; "Gustavo Guerra" ]
let owner = "Anh-Dung Phan"
// Tags for your project (for NuGet package)
let tags = "F# fsharp formatting beautifier indentation indenter"

// (<solutionFile>.sln is built during the building process)
let solutionFile  = "fantomas"
// Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Types and helper functions for building external projects (see the TestExternalProjects target below)
type ProcessStartInfo =
    { ProcessName : string
      Arguments : string list }

type ExternalProjectInfo =
    { GitUrl : string
      DirectoryName : string
      Tag : string
      SourceSubDirectory : string
      BuildConfigurationFn : (string -> ProcessStartInfo) }

// Construct the commands/arguments for running an external project build script for both windows and linux
// For linux we run this by invoking sh explicitly and passing the build.sh script as an argument as some
// projects generated on windows don't have the executable permission set for .sh scripts. On windows we
// treat .cmd files as executable
let configureBuildCommandFromDefaultFakeBuildScripts pathToProject =
    if Fake.EnvironmentHelper.isWindows
    then { ProcessName = pathToProject </> "build.cmd"; Arguments = [ "Build" ] }
    else { ProcessName = "sh"; Arguments = [ sprintf "%s/build.sh Build" pathToProject ] }

let configureBuildCommandDotnetBuild pathToProject =
    { ProcessName = "dotnet"; Arguments = [ "build"; pathToProject ] }

// Construct the path of the fantomas executable to use for external project tests
let fantomasExecutableForExternalTests projectdir =
    if Fake.EnvironmentHelper.isWindows
    then { ProcessName = sprintf "%s/src/Fantomas.Cmd/bin/%s/net452/dotnet-fantomas.exe" projectdir configuration; Arguments = [] }
    else { ProcessName = "dotnet"; Arguments = [ sprintf "%s/src/Fantomas.CoreGlobalTool/bin/%s/netcoreapp2.1/fantomas-tool.dll" projectdir configuration ] }

let externalProjectsToTest = [
    { GitUrl = @"https://github.com/fsprojects/Argu"
      DirectoryName = "Argu"
      Tag = "5.1.0"
      SourceSubDirectory = "src"
      BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
    ]

let externalProjectsToTestFailing = [
    { GitUrl = @"https://github.com/fsprojects/Chessie"
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
    { GitUrl = @"https://github.com/jack-pappas/ExtCore"
      DirectoryName = "ExtCore"
      Tag = "master"
      SourceSubDirectory = "."
      BuildConfigurationFn = configureBuildCommandDotnetBuild }
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
    ]

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs [
        "bin"
        "nuget"
        "src/Fantomas/bin"
        "src/Fantomas/obj"
        "src/Fantomas.Cmd/bin"
        "src/Fantomas.Cmd/obj"
        "src/Fantomas.CoreGlobalTool/bin"
        "src/Fantomas.CoreGlobalTool/obj"
    ]
)

let isAppVeyor = Fake.BuildServerHelper.buildServer = BuildServerHelper.AppVeyor

Target "ProjectVersion" (fun _ ->
    let version =
        if isAppVeyor then
            sprintf "%s.%s" release.NugetVersion BuildServerHelper.appVeyorBuildVersion
        else
            release.NugetVersion

    let setProjectVersion project =
        XMLHelper.XmlPoke ("src/"+project+"/"+project+".fsproj")
            "Project/PropertyGroup/Version/text()" version
    setProjectVersion "Fantomas"
    setProjectVersion "Fantomas.Cmd"
    setProjectVersion "Fantomas.CoreGlobalTool"
    setProjectVersion "Fantomas.Tests"
)

// --------------------------------------------------------------------------------------
// Build library & test project
Target "Build" (fun _ ->
    DotNetCli.Build (fun p ->
        { p with
            Project = (sprintf "src/%s.sln" solutionFile)
            Configuration = configuration
        }
    )
)

Target "UnitTests" (fun _ ->
    DotNetCli.Test (fun p ->
        { p with
            Project = "src/Fantomas.Tests/Fantomas.Tests.fsproj"
            Configuration = configuration
            AdditionalArgs = ["--no-build --no-restore --test-adapter-path:. --logger:nunit;LogFilePath=../../TestResults.xml"]
        }
    )
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "Pack" (fun _ ->
    let nugetVersion =
        if isAppVeyor then
            sprintf "%s-latest" release.NugetVersion
        else
            release.NugetVersion

    let pack project =
        let packParameters =
            [
                "--no-build"
                "--no-restore"
                sprintf "/p:Title=\"%s\"" project
                "/p:PackageVersion=" + nugetVersion
                sprintf "/p:Authors=\"%s\"" (String.Join(" ", authors))
                sprintf "/p:Owners=\"%s\"" owner
                "/p:PackageRequireLicenseAcceptance=false"
                sprintf "/p:Description=\"%s\"" description
                sprintf "/p:Summary=\"%s\"" (description.Substring(0,100))
                sprintf "/p:PackageReleaseNotes=\"%O\"" ((toLines release.Notes).Replace(",",""))
                sprintf "/p:Copyright=\"%s\"" copyright
                sprintf "/p:PackageTags=\"%s\"" tags
                sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
                sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
                sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
            ] |> String.concat " "
        "pack src/"+project+"/"+project+".fsproj -c "+ configuration + " -o ../../bin " + packParameters
        |> DotNetCli.RunCommand id

    pack "Fantomas"
    pack "Fantomas.Cmd"
    pack "Fantomas.CoreGlobalTool"
)


// This takes the list of external projects defined above, does a git checkout of the specified repo and tag,
// tries to build the project, then reformats with fantomas and tries to build the project again. If this fails
// then there was a regression in fantomas that mangles the source code
let testExternalProjects externalProjectsToTest =
    let externalBuildErrors =
        let project = getBuildParam "project"
        externalProjectsToTest
        |> if project="" then id else List.filter (fun p -> p.DirectoryName = project)
        |> List.map (fun project ->
            let relativeProjectDir = sprintf "external-project-tests/%s" project.DirectoryName

            Fake.FileHelper.CleanDir relativeProjectDir
            // Use "shallow" clone by setting depth to 1 to only check out the one commit we want to build
            Fake.Git.CommandHelper.gitCommand "." (sprintf "clone --branch %s --depth 1 %s %s" project.Tag project.GitUrl relativeProjectDir)

            let fullProjectPath = sprintf "%s/%s" __SOURCE_DIRECTORY__ relativeProjectDir
            let buildStartInfo = project.BuildConfigurationFn fullProjectPath

            let buildExternalProject() = ExecProcess
                                            (fun info -> info.FileName <- buildStartInfo.ProcessName
                                                         info.WorkingDirectory <- relativeProjectDir
                                                         info.Arguments <- String.Join(" ", buildStartInfo.Arguments))
                                            (TimeSpan.FromMinutes 5.0)

            let cleanResult = buildExternalProject()
            if cleanResult <> 0 then failwithf "Initial build of external project %s returned with a non-zero exit code" project.DirectoryName

            let fantomasStartInfo =
                fantomasExecutableForExternalTests __SOURCE_DIRECTORY__
            let arguments =
                fantomasStartInfo.Arguments @ [ sprintf "--recurse %s" project.SourceSubDirectory ]
                |> fun args -> String.Join(" ", args)
            let invokeFantomas() = ExecProcess
                                    (fun info -> info.FileName <- fantomasStartInfo.ProcessName
                                                 info.WorkingDirectory <- sprintf "%s/%s" __SOURCE_DIRECTORY__ relativeProjectDir
                                                 info.Arguments <- arguments)
                                    (TimeSpan.FromMinutes 5.0)
            let fantomasResult = invokeFantomas()

            if fantomasResult <> 0
            then Some <| sprintf "Fantomas invokation for %s returned with a non-zero exit code" project.DirectoryName
            else
                let formattedResult = buildExternalProject()
                if formattedResult <> 0
                then Some <| sprintf "Build of external project after fantomas formatting failed for project %s" project.DirectoryName
                else
                    printfn "Successfully built %s after reformatting" project.DirectoryName
                    None
        )
        |> List.choose id
    if not (List.isEmpty externalBuildErrors)
    then failwith (String.Join("\n", externalBuildErrors) )


Target "TestExternalProjects" (fun _ -> testExternalProjects externalProjectsToTest)
Target "TestExternalProjectsFailing" (fun _ -> testExternalProjects externalProjectsToTestFailing)

Target "Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin" }))

Target "MyGet" (fun _ ->
    Paket.Push (fun p ->
        { p with
            WorkingDir = "bin"
            PublishUrl = "https://www.myget.org/F/fantomas/api/v2/package"
            ApiKey = Fake.EnvironmentHelper.getBuildParam "myget-key"
        }
    )
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "ProjectVersion"
  ==> "Build"
  ==> "UnitTests"
  ==> "Pack"
  ==> "All"
  ==> "Push"

"Build"
  ==> "TestExternalProjects"
  
"Pack"
  ==> "MyGet"

RunTargetOrDefault "All"
