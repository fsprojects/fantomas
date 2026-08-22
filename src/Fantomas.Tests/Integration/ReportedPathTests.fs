module Fantomas.Tests.Integration.ReportedPathTests

open System
open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let UnformattedCode = "let a =   9"

[<Literal>]
let FormattedCode = "let a = 9\n"

[<Literal>]
let WithErrors = "let a ="

// A single result used to be reported by file name only, while a run over several files
// reported the path the file was found at. Both now report the path.

let private subFolderName () = Guid.NewGuid().ToString("N")

let private relativePath (subFolder: string) (fileName: string) =
    $"%s{subFolder}%c{Path.DirectorySeparatorChar}%s{fileName}.fs"

[<Test>]
let ``a formatted single file is reported with the path it was given`` () =
    use config = new ConfigurationFile("[*]\nend_of_line = lf")
    let subFolder = subFolderName ()

    use fileFixture =
        new TemporaryFileCodeSample(UnformattedCode, fileName = "A", subFolder = subFolder)

    let path = relativePath subFolder "A"
    let { ExitCode = exitCode; Output = output } = formatCode [ path ]

    exitCode |> should equal 0
    output |> should contain $"%s{path} was formatted."

[<Test>]
let ``an unchanged single file is reported with the path it was given`` () =
    use config = new ConfigurationFile("[*]\nend_of_line = lf")
    let subFolder = subFolderName ()

    use fileFixture =
        new TemporaryFileCodeSample(FormattedCode, fileName = "A", subFolder = subFolder)

    let path = relativePath subFolder "A"
    let { ExitCode = exitCode; Output = output } = formatCode [ path ]

    exitCode |> should equal 0
    output |> should contain $"%s{path} was unchanged."

[<Test>]
let ``a single file that could not be parsed is reported with the path it was given`` () =
    let subFolder = subFolderName ()

    use fileFixture =
        new TemporaryFileCodeSample(WithErrors, fileName = "A", subFolder = subFolder)

    let path = relativePath subFolder "A"
    let { ExitCode = exitCode; Error = error } = formatCode [ path ]

    exitCode |> should equal 1
    error |> should contain $"Fantomas could not parse %s{path}:"

[<Test>]
let ``a file that could not be parsed is reported the same way among several files`` () =
    use config = new ConfigurationFile("[*]\nend_of_line = lf")
    let subFolder = subFolderName ()

    use erroneousFixture =
        new TemporaryFileCodeSample(WithErrors, fileName = "A", subFolder = subFolder)

    use otherFixture = new TemporaryFileCodeSample(FormattedCode)

    let path = relativePath subFolder "A"

    let { ExitCode = exitCode; Error = error } =
        formatCode [ path; otherFixture.Filename ]

    exitCode |> should equal 1
    error |> should contain $"Fantomas could not parse %s{path}:"
