module Fantomas.CoreGlobalTool.Tests.ExitCodeTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Literal>]
let WithErrors = """let a ="""

[<Test>]
let ``invalid files should report exit code 1`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)
    let exitCode, _ = formatCode fileFixture.Filename
    exitCode |> should equal 1
