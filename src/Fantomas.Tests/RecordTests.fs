module Fantomas.Tests.RecordTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since the if is not aligned properly
[<Test>]
let ``should not break inside of if statements in records``() =
    formatSourceString false """let XpkgDefaults() =
    {
        ToolPath = "./tools/xpkg/xpkg.exe"
        WorkingDir = "./";
        TimeOut = TimeSpan.FromMinutes 5.
        Package = null
        Version = if not isLocalBuild then buildVersion else "0.1.0.0"
        OutputPath = "./xpkg"
        Project = null
        Summary = null
        Publisher = null
        Website = null
        Details = "Details.md"
        License = "License.md"
        GettingStarted = "GettingStarted.md"
        Icons = []
        Libraries = []
        Samples = [];
    }

    """ config
    |> should equal """let XpkgDefaults() = 
    { ToolPath = "./tools/xpkg/xpkg.exe"; WorkingDir = "./"; 
      TimeOut = TimeSpan.FromMinutes 5.0; Package = null; 
      Version = 
          if not isLocalBuild
          then buildVersion
          else "0.1.0.0"; 
      OutputPath = "./xpkg"; Project = null; Summary = null; 
      Publisher = null; Website = null; Details = "Details.md"; 
      License = "License.md"; GettingStarted = "GettingStarted.md"; Icons = []; 
      Libraries = []; Samples = [] }
"""