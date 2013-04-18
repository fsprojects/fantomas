module Fantomas.Tests.ParameterTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

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