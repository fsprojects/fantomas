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

[<Test>]
let ``file should be written to out folder when input folder has trailing slash`` () =
    use fileFixtureOne =
        new TemporaryFileCodeSample(FormattedCode, fileName = "A", subFolder = "subsrc")

    use outputFolder = new OutputFolder()

    let arguments =
        sprintf @"%s subsrc%c --out %s" Verbosity System.IO.Path.DirectorySeparatorChar outputFolder.Foldername

    let { ExitCode = exitCode; Output = output } = runFantomasTool arguments

    exitCode |> should equal 0
    let outputFilePath = System.IO.Path.Combine(outputFolder.Foldername, "A.fs")
    output |> should contain outputFilePath

[<Test>]
let ``file should be written to out folder when input folder has no trailing slash`` () =
    use fileFixtureOne =
        new TemporaryFileCodeSample(FormattedCode, fileName = "A", subFolder = "subsrc")

    use outputFolder = new OutputFolder()

    let arguments = sprintf @"%s subsrc --out %s" Verbosity outputFolder.Foldername

    let { ExitCode = exitCode; Output = output } = runFantomasTool arguments

    exitCode |> should equal 0
    let outputFilePath = System.IO.Path.Combine(outputFolder.Foldername, "A.fs")
    output |> should contain outputFilePath
