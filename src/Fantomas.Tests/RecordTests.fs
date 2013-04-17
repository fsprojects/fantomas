module Fantomas.Tests.RecordTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

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