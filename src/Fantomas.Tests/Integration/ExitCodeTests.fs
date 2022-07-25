module Fantomas.Tests.Integration.ExitCodeTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let WithErrors = """let a ="""

[<Literal>]
let CorrectlyFormatted =
    """module A

"""

[<Test>]
let ``invalid files should report exit code 1`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)
    let { ExitCode = exitCode } = formatCode [ fileFixture.Filename ]
    exitCode |> should equal 1

[<Test>]
let ``non-existing file should report exit code 1`` () =
    let { ExitCode = exitCode } = formatCode [ "somenonexistingfile.fs" ]
    exitCode |> should equal 1

[<Test>]
let ``unsupported file should report exit code 1`` () =
    use fileFixture = new TemporaryFileCodeSample(CorrectlyFormatted, extension = "txt")
    let { ExitCode = exitCode } = formatCode [ fileFixture.Filename ]
    exitCode |> should equal 1

[<Test>]
let ``missing file should report exit code 1`` () =
    let { ExitCode = exitCode } = formatCode []
    exitCode |> should equal 1
