// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open System

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/dungpa"
// The name of the project on GitHub
let gitName = "fantomas"
let cloneUrl = "git@github.com:dungpa/fantomas.git"

// (<solutionFile>.sln is built during the building process)
let solutionFile  = "fantomas"
let testAssemblies = "src/**/bin/Release/*Tests*.dll"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs ["bin"]
)

Target "RestorePackages" (fun _ ->
    !! "./**/packages.config"
    |> Seq.iter (RestorePackage (fun p -> { p with ToolPath = "src/.nuget/NuGet.exe" }))
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    // We would like to build only one solution
    !! (solutionFile + ".sln")
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
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "RestorePackages"
  ==> "Build"
  ==> "UnitTests"
  ==> "All"


RunTargetOrDefault "All"