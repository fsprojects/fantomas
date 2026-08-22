module Fantomas.Tests.Integration.WriteTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let FormattedCode = "let a = 9\n"

[<Literal>]
let UnformattedCode = "let a =   9"

let Verbosity = [ "--verbosity"; "d" ]

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
    let args = Verbosity @ [ inputFixture.Filename ]
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0

    output |> should contain "was unchanged"

[<Test>]
let ``incorrectly formatted file should be written`` () =
    let fileName = "A"

    use inputFixture = new TemporaryFileCodeSample(UnformattedCode, fileName = fileName)
    let args = Verbosity @ [ inputFixture.Filename ]
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0

    output |> should contain "has been written"

[<Test>]
let ``file should be written to out folder when input folder has trailing slash`` () =
    use fileFixtureOne =
        new TemporaryFileCodeSample(FormattedCode, fileName = "A", subFolder = "subsrc")

    use outputFolder = new OutputFolder()

    let arguments =
        Verbosity
        @ [ $"subsrc%c{Path.DirectorySeparatorChar}"; "--out"; outputFolder.Foldername ]

    let { ExitCode = exitCode; Output = output } = runFantomasTool arguments

    exitCode |> should equal 0
    let outputFilePath = Path.Combine(outputFolder.Foldername, "A.fs")
    output |> should contain outputFilePath

[<Test>]
let ``file should be written to out folder when input folder has no trailing slash`` () =
    use fileFixtureOne =
        new TemporaryFileCodeSample(FormattedCode, fileName = "A", subFolder = "subsrc")

    use outputFolder = new OutputFolder()

    let arguments = Verbosity @ [ "subsrc"; "--out"; outputFolder.Foldername ]

    let { ExitCode = exitCode; Output = output } = runFantomasTool arguments

    exitCode |> should equal 0
    let outputFilePath = Path.Combine(outputFolder.Foldername, "A.fs")
    output |> should contain outputFilePath

[<Test>]
let ``out folder mirrors the structure of the input folder`` () =
    let root = System.Guid.NewGuid().ToString("N")

    // Disposing this fixture deletes the entire root folder, so the sibling file written below
    // does not need a fixture of its own.
    use _nestedFixture =
        new TemporaryFileCodeSample(UnformattedCode, fileName = "A", subFolders = [| root; "nested" |])

    File.WriteAllText(Path.Join(Path.GetTempPath(), root, "A.fs"), UnformattedCode)

    use outputFolder = new OutputFolder()

    let arguments = Verbosity @ [ root; "--out"; outputFolder.Foldername ]

    let { ExitCode = exitCode } = runFantomasTool arguments

    exitCode |> should equal 0

    Path.Join(outputFolder.Foldername, "A.fs")
    |> File.ReadAllText
    |> String.normalizeNewLine
    |> should equal FormattedCode

    Path.Join(outputFolder.Foldername, "nested", "A.fs")
    |> File.ReadAllText
    |> String.normalizeNewLine
    |> should equal FormattedCode

[<Test>]
let ``out file is written when its folder does not exist yet`` () =
    use inputFixture = new TemporaryFileCodeSample(UnformattedCode, fileName = "A")

    use outputFolder = new OutputFolder()
    let outputFile = Path.Join(outputFolder.Foldername, "nested", "A.fs")

    let arguments = Verbosity @ [ inputFixture.Filename; "--out"; outputFile ]

    let { ExitCode = exitCode } = runFantomasTool arguments

    exitCode |> should equal 0

    outputFile
    |> File.ReadAllText
    |> String.normalizeNewLine
    |> should equal FormattedCode

[<Test>]
let ``out file without a folder is written next to the working directory`` () =
    use inputFixture = new TemporaryFileCodeSample(UnformattedCode, fileName = "A")

    // The tool runs with the temp folder as its working directory, so a bare file name has no
    // folder part at all. Creating "the parent folder" of such a path must not be attempted.
    let outputName = System.Guid.NewGuid().ToString("N") + ".fs"

    let arguments = Verbosity @ [ inputFixture.Filename; "--out"; outputName ]

    let { ExitCode = exitCode } = runFantomasTool arguments

    exitCode |> should equal 0

    let outputFile = Path.Join(Path.GetTempPath(), outputName)

    try
        outputFile
        |> File.ReadAllText
        |> String.normalizeNewLine
        |> should equal FormattedCode
    finally
        File.Delete outputFile
