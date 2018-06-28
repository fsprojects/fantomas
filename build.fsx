// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System


// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitHome = "https://github.com/dungpa"
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
let iconUrl = "https://raw.githubusercontent.com/dungpa/fantomas/master/fantomas_logo.png"
let licenceUrl = "https://github.com/dungpa/fantomas/blob/master/LICENSE.md"
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

type ExternalProjectInfo =
    { GitUrl : string
      DirectoryName : string
      BuildScriptArguments : string }
let externalProjectsToTest = [
    { GitUrl = @"https://github.com/fsprojects/Argu"
      DirectoryName = "Argu"
      BuildScriptArguments = "Build" } ]

// path of the fantomas executable to use for external project tests relative to the src/Fantomas.Cmd/bin/CONFIGURATIION/ path
let fantomasExecutableForExternalTests = "net45/dotnet-fantomas.exe"

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

Target "AssemblyInfo" (fun _ ->
  let shared =
      [ Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ]

  CreateFSharpAssemblyInfo "src/Fantomas/AssemblyInfo.fs"
      ( Attribute.InternalsVisibleTo "Fantomas.Tests" :: Attribute.Title "FantomasLib" :: shared)

  CreateFSharpAssemblyInfo "src/Fantomas.Cmd/AssemblyInfo.fs"
      (Attribute.Title "Fantomas" :: shared)
)

Target "ProjectVersion" (fun _ ->
    let setProjectVersion project =
        XMLHelper.XmlPoke ("src/"+project+"/"+project+".fsproj")
            "Project/PropertyGroup/Version/text()" release.NugetVersion
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
    let pack project =
        let packParameters =
            [
                "--no-build"
                "--no-restore"
                sprintf "/p:Title=\"%s\"" project
                "/p:PackageVersion=" + release.NugetVersion
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



Target "TestExternalProjects" (fun _ ->
    for project in externalProjectsToTest do
        let relativeProjectDir = sprintf "external-project-tests/%s" project.DirectoryName
        let buildCommandScript = sprintf "%s/%s/build.cmd" __SOURCE_DIRECTORY__ relativeProjectDir
        if Fake.FileSystemHelper.directoryExists relativeProjectDir then
            Fake.Git.Reset.ResetHard relativeProjectDir
            Fake.Git.Branches.pull relativeProjectDir "origin" "master"
        else
            Fake.Git.Repository.clone "." project.GitUrl relativeProjectDir

        let buildExternalProject() = ExecProcess (fun info -> info.FileName <- buildCommandScript; info.WorkingDirectory <- relativeProjectDir; info.Arguments <- project.BuildScriptArguments) (TimeSpan.FromMinutes 5.0)

        //let (success, messages) = executeFSI relativeProjectDir script env
        let cleanResult = buildExternalProject()
        if cleanResult <> 0 then failwithf "Initial build of external project %s returned with a non-zero exit code" project.DirectoryName

        let fantomasExecutable = sprintf "src/Fantomas.Cmd/bin/%s/%s" configuration fantomasExecutableForExternalTests
        let invokeFantomas() = ExecProcess (fun info -> info.FileName <- fantomasExecutable; info.WorkingDirectory <- relativeProjectDir; info.Arguments <- "--recurse .") (TimeSpan.FromMinutes 5.0)
        let fantomasResult = invokeFantomas()
        if fantomasResult <> 0 then failwithf "Fantomas invokation for %s returned with a non-zero exit code" project.DirectoryName

        let formattedResult = buildExternalProject()
        if formattedResult <> 0 then failwithf "Build of external project after fantomas formatting failed for project %s" project.DirectoryName
        ()
)

Target "Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin" }))

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing
Target "Root" DoNothing

"Root"
  ==> "Clean"
  ==> "AssemblyInfo"
  ==> "ProjectVersion"
  ==> "Build"
  ==> "UnitTests"

"UnitTests"

  ==> "Pack"
  ==> "All"
  ==> "Push"

"UnitTests"
  ==> "TestExternalProjects"

RunTargetOrDefault "All"
