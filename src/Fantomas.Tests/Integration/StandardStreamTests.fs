module Fantomas.Tests.Integration.StandardStreamTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let WithErrors = """let a ="""

[<Test>]
let ``errors are written to standard error and not to standard out`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)

    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        formatCode [ fileFixture.Filename ]

    exitCode |> should equal 1
    output |> should equal ""
    Assert.That(error, Does.Contain "error FS0010:")

[<Test>]
let ``progress messages are written to standard out and not to standard error`` () =
    use fileFixture = new TemporaryFileCodeSample("let a =   0")

    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    Assert.That(output, Does.Contain "was formatted")
    error |> should equal ""

// Fantomas.Client discovers the tool by running `--version` and reading standard out.
// Moving this to standard error would break every editor integration.
[<Test>]
let ``version is written to standard out`` () =
    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        runFantomasTool [ "--version" ]

    exitCode |> should equal 0
    Assert.That(output, Does.Contain "Fantomas v")
    error |> should equal ""

// The banner used to go through the logger, which at detailed verbosity prefixes what it writes
// with a timestamp and a level. What Fantomas.Client parses should not depend on that.
[<Test>]
let ``version reads the same at any verbosity`` () =
    let normal: FantomasToolResult = runFantomasTool [ "--version" ]
    let detailed: FantomasToolResult = runFantomasTool [ "--version"; "-v"; "d" ]

    detailed.ExitCode |> should equal 0
    detailed.Output |> should equal normal.Output

// `--version` answers whatever else was asked for, and before any of it is validated, so it can
// always be used to find out what you are running.
[<Test>]
[<TestCase("--daemon")>]
[<TestCase("--json")>]
[<TestCase("--check")>]
let ``version wins over any other argument`` (argument: string) =
    let { ExitCode = exitCode; Output = output } =
        runFantomasTool [ "--version"; argument ]

    exitCode |> should equal 0
    Assert.That(output, Does.Contain "Fantomas v")

[<Test>]
let ``version is answered even when the rest of the command line is not valid`` () =
    let { ExitCode = exitCode; Output = output } =
        runFantomasTool [ "--version"; "-v"; "not-a-level" ]

    exitCode |> should equal 0
    Assert.That(output, Does.Contain "Fantomas v")

// A parse failure is the one error an agent or a CI job can act on without opening the file,
// provided it is told where the failure is.
[<Test>]
let ``a parse failure is reported with its position and the source around it`` () =
    use fileFixture = new TemporaryFileCodeSample("module A\n\nlet a = (1 + 2\n")

    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        formatCode [ fileFixture.Filename ]

    exitCode |> should equal 1
    output |> should equal ""
    Assert.That(error, Does.Contain $"%s{fileFixture.Filename}(3,9): error FS0583: Unmatched '('")
    Assert.That(error, Does.Contain "3 | let a = (1 + 2")
    Assert.That(error, Does.Contain "  |         ^")

[<Test>]
let ``--check reports a parse failure the same way a format run does`` () =
    use fileFixture = new TemporaryFileCodeSample("module A\n\nlet a = (1 + 2\n")

    let { ExitCode = exitCode; Error = error } =
        runFantomasTool [ "--check"; fileFixture.Filename ]

    exitCode |> should equal 1
    Assert.That(error, Does.Contain $"%s{fileFixture.Filename}(3,9): error FS0583: Unmatched '('")
    Assert.That(error, Does.Not.Contain "at Fantomas.Core.CodeFormatterImpl")
