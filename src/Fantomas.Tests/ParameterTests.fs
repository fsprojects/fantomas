module Fantomas.Tests.ParameterTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

// the current behavior results in a compile error since the ? is removed from the optional parameter
[<Test>]
let ``should keep the ? in optional parameters``() =
    formatSourceString false """type Shell() = 
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = 
        shellExec(Shell.GetParams(cmd, ?args = args))

    """ config
    |> should equal """type Shell() = 
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = 
        shellExec(Shell.GetParams(cmd, ?args = args))
"""