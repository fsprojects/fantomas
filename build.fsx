// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System

setEnvironVar "MSBuild" (ProgramFilesX86 @@ @"\MSBuild\12.0\Bin\MSBuild.exe")

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/dungpa"
// The name of the project on GitHub
let gitName = "fantomas"
let cloneUrl = "git@github.com:dungpa/fantomas.git"

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "Fantomas"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Source code formatting tool for F#"

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
// Tags for your project (for NuGet package)
let tags = "F# fsharp formatting beautifier indentation indenter"

// (<solutionFile>.sln is built during the building process)
let solutionFile  = "fantomas"
let testAssemblies = "src/**/bin/Release/*Tests*.dll"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs ["bin"; "nuget"]
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

  CreateFSharpAssemblyInfo "src/Fantomas.UI/AssemblyInfo.fs"
      (Attribute.Title "Fantomas.UI" :: shared) 
)

Target "CopyPrerequisites" (fun _ ->
    let additionalFiles = 
        ["./packages/FSharp.Core/lib/net40/FSharp.Core.sigdata";
         "./packages/FSharp.Core/lib/net40/FSharp.Core.optdata"]
    CopyTo "src/Fantomas.Tests/bin/Release" additionalFiles
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    // We would like to build only one solution
    !! ("src/" + solutionFile + ".sln")
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

Target "UnitTests" (fun _ ->
    !! testAssemblies 
    |> NUnit (fun p ->        
          { p with
              DisableShadowCopy = true
              TimeOut = TimeSpan.FromMinutes 20.
              Framework = "4.5"
              Domain = NUnitDomainModel.MultipleDomainModel
              OutputFile = "TestResults.xml" })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "src/Fantomas.Cmd/bin/Release"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Dependencies = [ "FSharp.Compiler.Service", GetPackageVersion "packages" "FSharp.Compiler.Service" ] })
        (project + ".nuspec")
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "CopyPrerequisites"
  ==> "UnitTests"
  ==> "All"
  ==> "NuGet"

RunTargetOrDefault "All"