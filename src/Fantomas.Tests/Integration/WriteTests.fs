module Fantomas.Tests.Integration.WriteTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let FormattedCode = "let a = 9\n"

[<Literal>]
let UnformattedCode = "let a =   9"

[<Literal>]
let Verbosity = "--verbosity d"

[<Test>]
let ``correctly formatted file should not be written, 1984`` () =
    let fileName = "A"

    use configFixture =
        new ConfigurationFile(
            """
[*]
end_of_line=lf
"""
        )

    use inputFixture = new TemporaryFileCodeSample(FormattedCode, fileName = fileName)
    let args = sprintf "%s %s" Verbosity inputFixture.Filename
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0

    output |> should contain "was unchanged"

[<Test>]
let ``incorrectly formatted file should be written`` () =
    let fileName = "A"

    use inputFixture = new TemporaryFileCodeSample(UnformattedCode, fileName = fileName)
    let args = sprintf "%s %s" Verbosity inputFixture.Filename
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0

    output |> should contain "has been written"
