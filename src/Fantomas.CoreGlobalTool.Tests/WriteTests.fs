module Fantomas.CoreGlobalTool.Tests.WriteTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Literal>]
let FormattedCode =
    """let a = 9
"""

[<Literal>]
let UnformattedCode = "let a =   9"

[<Test>]
let ``correctly formatted file should not be written, 1984`` () =
    let fileName = "A"

    use inputFixture =
        new TemporaryFileCodeSample(FormattedCode, fileName = fileName)

    let { ExitCode = exitCode; Output = output } = runFantomasTool inputFixture.Filename
    exitCode |> should equal 0

    output |> should contain "was unchanged"

[<Test>]
let ``incorrectly formatted file should be written`` () =
    let fileName = "A"

    use inputFixture =
        new TemporaryFileCodeSample(UnformattedCode, fileName = fileName)

    let { ExitCode = exitCode; Output = output } = runFantomasTool inputFixture.Filename
    exitCode |> should equal 0

    output |> should contain "has been written"
