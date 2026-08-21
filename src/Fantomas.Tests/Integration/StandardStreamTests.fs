module Fantomas.Tests.Integration.StandardStreamTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let WithErrors = """let a ="""

[<Test>]
let ``errors are written to standard error and not to standard out`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)

    let { ExitCode = exitCode
          Output = output
          Error = error } =
        formatCode [ fileFixture.Filename ]

    exitCode |> should equal 1
    output |> should equal ""
    Assert.That(error, Does.Contain "Could not parse the file.")

[<Test>]
let ``progress messages are written to standard out and not to standard error`` () =
    use fileFixture = new TemporaryFileCodeSample("let a =   0")

    let { ExitCode = exitCode
          Output = output
          Error = error } =
        formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    Assert.That(output, Does.Contain "was formatted")
    error |> should equal ""

// Fantomas.Client discovers the tool by running `--version` and reading standard out.
// Moving this to standard error would break every editor integration.
[<Test>]
let ``version is written to standard out`` () =
    let { ExitCode = exitCode
          Output = output
          Error = error } =
        runFantomasTool [ "--version" ]

    exitCode |> should equal 0
    Assert.That(output, Does.Contain "Fantomas v")
    error |> should equal ""
