module Fantomas.Tests.SignatureTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

// the current behavior results in a compile error since "(string * string) list" is converted to "string * string list"
[<Test>]
let ``should break on . operator``() =
    formatSourceString false """type MSBuildParams = 
    { Targets : string list
      Properties : (string * string) list
      MaxCpuCount : int option option
      ToolsVersion : string option
      Verbosity : MSBuildVerbosity option
      FileLoggers : MSBuildFileLoggerConfig list option }

    """ config
    |> should equal """type MSBuildParams = 
    { Targets : string list;
      Properties : (string * string) list;
      MaxCpuCount : int option option;
      ToolsVersion : string option;
      Verbosity : MSBuildVerbosity option;
      FileLoggers : MSBuildFileLoggerConfig list option }
"""